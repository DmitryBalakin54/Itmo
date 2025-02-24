package info.kgeorgiy.ja.balakin.hello;

import info.kgeorgiy.java.advanced.hello.*;

import java.io.IOException;
import java.net.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

/**
 * This class implements {@link HelloClient},this is a UDP client that sends requests and receives responses with Hello.
 *
 * @author DmitryBalakin
 */
public class HelloUDPClient implements HelloClient {

    final int SOCKET_TIMEOUT = 100;
    private final TimeUnit TIME_UNIT = TimeUnit.SECONDS;
    final int AWAIT_TERMINATION_TIME = 10;

    private static boolean checkArgs(int port, int threads, int requests) {
        return port >= 0 && threads > 0 && requests > 0;
    }

    /**
     * method for running this class from the command line:
     * HelloUDPClient host [port [prefix [threads [requests]]]].
     *
     * @param args command-line arguments: String host, int port, String threads, int requests
     */
    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Arguments is null");
            return;
        }

        if (args.length != 5) {
            System.err.println("Incorrect numbers of args");
            return;
        }

        String host = args[0];
        String prefix = args[2];
        int port, threads, requests;
        try {
            port = Integer.parseInt(args[1]);
            threads = Integer.parseInt(args[3]);
            requests = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {
            System.err.println("Args numbered 2, 4, 5 must be integer");
            return;
        }

        if (!checkArgs(port, threads, requests)) {
            System.err.println("Incorrect integer arguments");
            return;
        }

        new HelloUDPClient().run(host, port, prefix, threads, requests);
    }

    private String createRequest(String prefix, int threadNum, int requestNum) {
        return String.format("%s%d_%d", prefix, threadNum, requestNum);
    }

    private String createAnswer(String request) {
        return String.format("Hello, %s", request);
    }
    private void log(String message, HelloPacketWrapper pocket) {
        System.out.printf("[HelloUDPClient]: %s, Packet data is {%s}%n", message, pocket);
    }
    private void runRequest(DatagramSocket socket, SocketAddress address, String request, HelloPacketWrapper responsePacket) {
        var correctResponse = createAnswer(request);

        while (!socket.isClosed() && !Thread.currentThread().isInterrupted()) {
            try {
                var requestPacket = new HelloPacketWrapper(request, address);

                socket.send(requestPacket.get());
                log("request started", requestPacket);
                socket.receive(responsePacket.get());
                if (responsePacket.dataIsEqual(correctResponse)) {
                    log("correct packet received", responsePacket);
                    break;
                }
            } catch (SocketTimeoutException ignored){
            }catch (IOException e) {
                System.err.printf("Error while trying to send or receive packet: %s%n", e.getMessage());
                break;
            }
        }
    }

    private void runRequests(SocketAddress address, String prefix, int threadNum, int bound) {
        try (DatagramSocket socket = new DatagramSocket())  {
            socket.setSoTimeout(SOCKET_TIMEOUT);
            var responsePacket = new HelloPacketWrapper(socket.getReceiveBufferSize());

            IntStream.range(1, bound + 1).forEach(i ->
                    runRequest(socket, address, createRequest(prefix, threadNum, i), responsePacket));
        } catch (SocketException e) {
            System.err.printf("Couldn't create socket or set timeout: %s%n", e.getMessage());
        }
    }

    private SocketAddress createSocketAddress(String host, int port) throws UnknownHostException {
        var address = InetAddress.getByName(host);
        return new InetSocketAddress(address, port);
    }

    /**
     * Runs the client.
     *
     * @param host     the host to connect to
     * @param port     the port to connect to
     * @param prefix   the prefix for request messages
     * @param threads  the number of threads to use for sending requests
     * @param requests the number of requests to send per thread
     */
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try {
            SocketAddress address = createSocketAddress(host, port);
            ExecutorService workers = Executors.newFixedThreadPool(threads);

            IntStream.range(1, threads + 1).forEach(i ->
                    workers.submit(() -> runRequests(address, prefix, i, requests)));

            closeExecutorService(workers, (long) threads * requests);
        } catch (UnknownHostException e) {
            System.err.printf("Unknown host name=%s, port=%s: %s%n", host, port, e.getMessage());
        }
    }

    private void closeExecutorService(ExecutorService executorService, long scale) {
        executorService.shutdown();
        try {
            var ignoredVal = executorService.awaitTermination(scale * AWAIT_TERMINATION_TIME, TIME_UNIT);
        } catch (InterruptedException ignored) {
        }
    }
}
