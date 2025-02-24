package info.kgeorgiy.ja.balakin.hello;

import java.net.DatagramPacket;
import java.net.SocketAddress;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Wrapper class for {@link DatagramPacket} providing additional functionality.
 *
 * @author DmitryBalakin
 */
public class HelloPacketWrapper {
    private final Charset CHARSET = StandardCharsets.UTF_8;
    private final DatagramPacket packet;

    /**
     * Constructs a new instance of {@code HelloPacketWrapper} with the specified buffer size.
     *
     * @param bufferSize the size of the buffer for the underlying {@link DatagramPacket}
     */
    public HelloPacketWrapper(int bufferSize) {
        packet = new DatagramPacket(new byte[bufferSize], bufferSize);
    }

    /**
     * Constructs a new instance of {@code HelloPacketWrapper} with the specified message and socket address.
     *
     * @param message the message to be sent
     * @param address the socket address to which the message will be sent
     */
    public HelloPacketWrapper(String message, SocketAddress address) {
        var bytesMessage = message.getBytes(CHARSET);
        packet = new DatagramPacket(bytesMessage, 0, bytesMessage.length, address);
    }

    /**
     * Returns the underlying {@link DatagramPacket}.
     *
     * @return the underlying {@link DatagramPacket}
     */
    public DatagramPacket get() {
        return packet;
    }

    /**
     * Returns the socket address associated with the underlying {@link DatagramPacket}.
     *
     * @return the socket address associated with the underlying {@link DatagramPacket}
     */
    public SocketAddress getSocketAddress() {
        return packet.getSocketAddress();
    }

    /**
     * Returns a string representation of the data contained in the underlying {@link DatagramPacket}.
     *
     * @return a string representation of the data contained in the underlying {@link DatagramPacket}
     */
    @Override
    public String toString() {
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), CHARSET);
    }

    /**
     * Checks if the data contained in the underlying {@link DatagramPacket} is equal to the specified string.
     *
     * @param expected the string to compare the data against
     * @return {@code true} if the data is equal to the specified string, {@code false} otherwise
     */
    public boolean dataIsEqual(String expected) {
        return toString().equals(expected);
    }
}
