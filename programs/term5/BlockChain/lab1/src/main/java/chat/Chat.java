package chat;

import io.ipfs.multiaddr.MultiAddress;
import io.ipfs.multihash.Multihash;
import io.libp2p.core.*;
import io.libp2p.core.crypto.PrivKey;
import io.libp2p.core.multiformats.Multiaddr;
import io.netty.buffer.*;
import io.netty.handler.codec.http.*;
import org.peergos.*;
import org.peergos.blockstore.*;
import org.peergos.config.*;
import org.peergos.net.ConnectionException;
import org.peergos.protocol.dht.*;
import org.peergos.protocol.http.HttpProtocol;

import java.net.*;
import java.nio.charset.Charset;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Chat {
    private final EmbeddedIpfs embeddedIpfs;
    private boolean isHost = false;

    private String answer = null;
    private String word = null;
    private int cnt = 0;
    private boolean inGame = false;
    private boolean isEnd = false;
    private boolean isSecond = false;
    private Set<Character> letters = new HashSet<>();

    private static final Scanner scanner = new Scanner(System.in);

    private final String HOST_STR = "YOU ARE HOST";
    private final String NOT_HOST_STR = "YOU ARE NOT HOST";
    private final String END = "END GAME";
    private final String WIN = "YOU ARE WIN";
    private final String LOSE = "YOU ARE LOSE";

    private void getMessage(String message) {
        switch (message) {
            case HOST_STR -> {
                isHost = true;
                inGame = true;
                isSecond = true;
                System.out.println("Print something to continue");
            }
            case NOT_HOST_STR -> {
                isHost = false;
                isSecond = true;
                System.out.println("Print something to continue");
            }
            case END -> isEnd = true;
            case WIN -> System.out.println("You are win!");
            case LOSE -> System.out.println("You are lose :(");
            default -> {
                String[] split = message.split(" ");
                if (split.length == 2) {
                    cnt = Integer.parseInt(split[1]);
                    if (cnt <= 0) {
                        cnt = 5;
                    }

                    answer = split[0].trim();
                    word = "_".repeat(answer.length());
                } else {
                    System.out.println(split[0]);
                }
            }
        }
    }

    private void printGameStatus() {
        System.out.println(word + " attempts left: " + cnt);
    }

    private static String getAdr() {
        try {
            InetAddress localHost = InetAddress.getLocalHost();
            return localHost.getHostAddress();
        } catch (UnknownHostException e) {
            System.out.println("Failed to get IP address: " + e.getMessage());
        }
        return null;
    }

    private HttpProtocol.HttpRequestProcessor proxyHandler() {
        return (s, req, h) -> {
            ByteBuf content = req.content();
            String receivedMove = content.getCharSequence(0, content.readableBytes(), Charset.defaultCharset()).toString();

            getMessage(receivedMove);

            FullHttpResponse replyOk = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, Unpooled.buffer(0));
            replyOk.headers().set(HttpHeaderNames.CONTENT_LENGTH, 0);
            h.accept(replyOk.retain());
        };
    }

    public Chat() throws ConnectionException {
        RecordStore recordStore = new RamRecordStore();
        Blockstore blockStore = new RamBlockstore();

        String ipAddress = getAdr();
        System.out.println("Enter port number or type NO:");
        String port = scanner.nextLine();
        int portNumber = 0;
        try {
            portNumber = port.equals("NO") ? 10000 + new Random().nextInt(50000) : Integer.parseInt(port);
        } catch (NumberFormatException e) {
            portNumber = new Random().nextInt(50000) + 10000;
        }

        String multiAddressStr = "/ip4/" + ipAddress + "/tcp/" + portNumber;
        List<MultiAddress> swarmAddresses = List.of(new MultiAddress(multiAddressStr));
        List<MultiAddress> bootstrapNodes = new ArrayList<>(Config.defaultBootstrapNodes);

        HostBuilder builder = new HostBuilder().generateIdentity();
        PrivKey privKey = builder.getPrivateKey();
        PeerId peerId = builder.getPeerId();
        System.out.println("My PeerId: " + peerId.toBase58());

        IdentitySection identitySection = new IdentitySection(privKey.bytes(), peerId);
        BlockRequestAuthoriser authoriser = (c, p, a) -> CompletableFuture.completedFuture(true);

        embeddedIpfs = EmbeddedIpfs.build(recordStore, blockStore, true,
                swarmAddresses,
                bootstrapNodes,
                identitySection,
                authoriser, Optional.of(proxyHandler()));
        embeddedIpfs.start();

        System.out.println("Enter the PeerId of the opponent:");
        String peerIdStr = scanner.nextLine().trim();
        if (peerIdStr.isEmpty()) throw new IllegalArgumentException("Invalid PeerId");
        Multihash targetNodeId = Multihash.fromBase58(peerIdStr);
        PeerId targetPeerId = PeerId.fromBase58(targetNodeId.toBase58());
        Multiaddr[] addrs = EmbeddedIpfs.getAddresses(embeddedIpfs.node, embeddedIpfs.dht, targetNodeId);

        runClientChat(targetPeerId, addrs);
    }

    private void runClientChat(PeerId targetPeerId, Multiaddr[] addressesToDial) {
        while (true) {
            if (!inGame) {
                System.out.println("Are you the host? (yes/no): ");
                boolean isHostTmp = scanner.nextLine().trim().equalsIgnoreCase("yes");
                if (!isSecond) {
                    isHost = isHostTmp;
                    if (!isHost) {
                        send(HOST_STR, targetPeerId, addressesToDial);
                    } else {
                        inGame = true;
                        send(NOT_HOST_STR, targetPeerId, addressesToDial);
                    }
                }
            }

            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }

            if (isHost && answer == null) {
                System.out.print("");
                continue;
            }


            if (isHost && inGame && cnt > 0) {
                runHostGame(targetPeerId, addressesToDial);
            } else if (!isHost && !inGame) {
                runGame(targetPeerId, addressesToDial);
            }

            if (isEnd) {
                inGame = false;
                isEnd = false;
                word = null;
                answer = null;
                isHost = false;
                isSecond = false;
                cnt = 0;
                letters = new HashSet<>();
            }
        }
    }

    private void runGame(PeerId targetPeerId, Multiaddr[] addressesToDial) {
        String input;
        do {
            System.out.println("Type word and amount of attempts");
            input = scanner.nextLine().trim().toLowerCase();
        } while (!input.matches("\\b\\w+\\b \\d+\\b"));

        send(input, targetPeerId, addressesToDial);
        inGame = true;
    }

    private void runHostGame(PeerId targetPeerId, Multiaddr[] addressesToDial) {
        System.out.println("Type word (" + answer.length() + " letters)");
        String input = scanner.nextLine().trim().toLowerCase();
        if (input.length() < answer.length()) {
            input += " ".repeat(answer.length() - input.length());
        }

        input = input.substring(0, answer.length());

        StringBuilder wrd = new StringBuilder();
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c == answer.charAt(i)) {
                wrd.append(c);
            } else if (word.charAt(i) == answer.charAt(i)) {
                wrd.append(word.charAt(i));
            } else {
                wrd.append('_');
                if (answer.contains(c + "") && !itWas(c)) {
                    System.out.println("Word has letter " + c);
                }
            }
            letters.add(c);
        }
        word = wrd.toString();
        cnt--;
        printGameStatus();
        send(word, targetPeerId, addressesToDial);
        if (answer.equals(word)) {
            send(END, targetPeerId, addressesToDial);
            send(LOSE, targetPeerId, addressesToDial);
            isEnd = true;
            System.out.println("You are win!");
        }

        if (cnt == 0) {
            send(END, targetPeerId, addressesToDial);
            send(WIN, targetPeerId, addressesToDial);
            isEnd = true;
            System.out.println("You are lose :( word is " + answer);
        }
    }

    private boolean itWas(char c) {
        return letters.contains(c);
    }

    private void send(String move, PeerId targetPeerId, Multiaddr[] addressesToDial) {
        byte[] msg = move.getBytes();
        FullHttpRequest httpRequest = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.POST, "/", Unpooled.copiedBuffer(msg));
        httpRequest.headers().set(HttpHeaderNames.CONTENT_LENGTH, msg.length);
        if (embeddedIpfs.p2pHttp.isPresent()) {
            HttpProtocol.HttpController proxier = embeddedIpfs.p2pHttp.get().dial(embeddedIpfs.node, targetPeerId, addressesToDial).getController().join();
            proxier.send(httpRequest.retain()).join().release();
        } else {
            throw new RuntimeException();
        }
    }

    public static void main(String[] args) throws ConnectionException {
        System.out.println("Type 'NO' if you don't want to see logs:");
        if (scanner.nextLine().equals("NO")) {
            Logger.getGlobal().setLevel(Level.OFF);
        }
        new Chat();
    }
}
