package Eliza;


/**
 *  Eliza memory class
 */

public class Mem {

    /** The memory size */
    final int memMax = 20;
    /** The memory */
    String memory[] = new String[memMax];
    /** The memory top */
    int memTop = 0;

    public void save(String str) {
        if (memTop < memMax) {
            memory[memTop++] = new String(str);
        }
    }

    public String get() {
        if (memTop == 0) return null;
        String m = memory[0];
        for (int i = 0; i < memTop-1; i++)
            memory[i] = memory[i+1];
        memTop--;
        return m;
    }
}


