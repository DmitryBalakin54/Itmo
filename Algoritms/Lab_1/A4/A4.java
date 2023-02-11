import java.util.*;

public class A4 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        Stack stack = new Stack();
        label : while (true) {
            String str = scan.next();
            switch (str) {
                case "push":
                    stack.push(scan.nextInt());
                    System.out.println("ok");
                    break;
                case "back":
                    System.out.println(stack.back());
                    break;
                case "pop":
                    System.out.println(stack.pop());
                    break;
                case "size":
                    System.out.println(stack.getSize());
                    break;
                case "clear":
                    stack.clear();
                    System.out.println("ok");
                    break;
                default:
                    System.out.println("bye");
                    scan.close();
                    break label;
            }
        }

    }
    static class Stack {
        private int[] stack;
        private int head;

        private final int START_SIZE = 2;
        private final int START_HEAD = -1;

        Stack() {
            stack = new int[START_SIZE];
            head = START_HEAD;
        }

        private void resize() {
            stack = Arrays.copyOf(stack, stack.length * 2);
        }

        public void push(int value) {
            if (head == stack.length - 1) {
                resize();
            }
            stack[++head] = value;
        }

        public int pop() {
            return stack[head--];
        }

        private int back() {
            return stack[head];
        }
        private void clear() {
            stack = new int[START_SIZE];
            head = START_HEAD;
        }

        public int getSize() {
            return head + 1;
        }
    }
}
