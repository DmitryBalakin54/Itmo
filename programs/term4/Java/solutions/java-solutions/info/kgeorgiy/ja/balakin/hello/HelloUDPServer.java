package info.kgeorgiy.ja.balakin.hello;

import info.kgeorgiy.java.advanced.hello.*;

import java.io.IOException;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;

/**
 * This class implements {@link NewHelloServer},this is a UDP server that receives requests and sends responses with Hello.
 *
 * @author DmitryBalakin
 */
public class HelloUDPServer implements NewHelloServer {
    private List<Element> ports;
    private ExecutorService workers;

    private static class Element {
        public final DatagramSocket socket;
        public final String str;
        public int bufferSize = 0;

        public Element(DatagramSocket socket, String str) {
            this.socket = socket;
            this.str = str;
        }
    }

    private static boolean checkArgs(int port, int threads) {
        return port >= 0 && threads > 0;
    }

    /**
     * method for running this class from the command line:
     * HelloUDPServer port [threads].
     *
     * @param args command-line arguments: int port, int threads
     */
    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Arguments is null");
            return;
        }

        if (args.length != 2) {
            System.err.println("Incorrect number of args");
            return;
        }

        int port, threads;
        try {
            port = Integer.parseInt(args[0]);
            threads = Integer.parseInt(args[1]);
        } catch (NumberFormatException e) {
            System.err.println("Args must be integer numbers");
            return;
        }

        if (!checkArgs(port, threads)) {
            System.err.println("Incorrect args");
            return;
        }


        try (var server = new HelloUDPServer()) {
            server.start(port, threads);
        }
    }

    private void init(Map<Integer, String> port, int threads) throws SocketException {
        ports = new ArrayList<>();
        for (int i: port.keySet()) {
            var el = new Element(new DatagramSocket(i), port.get(i));

            el.bufferSize = el.socket.getReceiveBufferSize();
            ports.add(el);
        }

        workers = Executors.newFixedThreadPool(threads);
    }


    private void response(HelloPacketWrapper packet, Element el) {
        var responsePacket = new HelloPacketWrapper(
                el.str.replace("$", packet.toString()),
                packet.getSocketAddress()
        );

        try {
            el.socket.send(responsePacket.get());
        } catch (IOException e) {
            System.err.printf("Error while trying to send: %s%n", e.getMessage());
        }
    }

    /**
     * Starts the UDP server.
     *
     * @param port    the ports for this server
     * @param threads the number of worker
     */
    @Override
    public void start(int threads, Map<Integer, String> port) {
        try {
            init(port, threads);
        } catch (SocketException e) {
            System.err.printf("Socket exception %s%n", e.getMessage());
            return;
        }

        workers.submit(() -> {
            boolean open = true;

            while (!Thread.currentThread().isInterrupted() && open) {
                open = false;
                for (var el : ports) {
                    if (el.socket.isClosed()) {
                        continue;
                    }

                    final var packet = new HelloPacketWrapper(el.bufferSize);

                    try {
                        el.socket.receive(packet.get());
                        response(packet, el);
                    } catch (IOException e) {
                        System.err.printf("Receive or submit exception: %s%n", e.getMessage());
                    }
                }

                for (var el : ports) {
                    if (!el.socket.isClosed()) {
                        open = true;
                    }
                }
            }
        });
    }


    /**
     * Closes the UDP server.
     */
    @Override
    public void close() {
        if (ports != null) {
            ports.forEach(el -> el.socket.close());
        }

        workers.close();
    }
}
