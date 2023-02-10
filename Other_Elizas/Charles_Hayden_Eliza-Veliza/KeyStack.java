package Eliza;

/**
 *  A stack of keys.
 *  The keys are kept in rank order.
 */
public class KeyStack {

    /** The stack size */
    final int stackSize = 20;
    /** The key stack */
    Key keyStack[] = new Key[stackSize];
    /** The top of the key stack */
    int keyTop = 0;

    /**
     *  Prints the key stack.
     */
    public void print() {
        System.out.println("Key stack " + keyTop);
        for (int i = 0; i < keyTop; i++) {
            keyStack[i].printKey(0);
        }
    }

    /**
     *  Get the stack size.
     */
    public int keyTop() {
        return keyTop;
    }

    /**
     *  Reset the key stack.
     */
    public void reset() {
        keyTop = 0;
    }

    /**
     *  Get a key from the stack.
     */
    public Key key(int n) {
        if (n < 0 || n >= keyTop) return null;
        return keyStack[n];
    }

    /**
     *  Push a key in the stack.
     *  Keep the highest rank keys at the bottom.
     */
    public void pushKey(Key key) {
        if (key == null) {
            System.out.println("push null key");
            return;
        }
        int i;
        for (i = keyTop; i > 0; i--) {
            if (key.rank > keyStack[i-1].rank) keyStack[i] = keyStack[i-1];
            else break;
        }
        keyStack[i] = key;
        keyTop++;
    }


}
