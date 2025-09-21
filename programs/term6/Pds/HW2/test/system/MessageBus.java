package system;

public interface MessageBus {
    void addNewMessage(int senderPid, int receiverPid, Object message);

    Message getNextMessage();
}
