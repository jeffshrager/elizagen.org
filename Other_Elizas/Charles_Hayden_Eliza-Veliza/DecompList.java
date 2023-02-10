package Eliza;

import java.util.Vector;

/**
 *  Eliza decomp list.
 *  This stores all the decompositions of a single key.
 */
public class DecompList extends Vector {

    /**
     *  Add another decomp rule to the list.
     */
    public void add(String word, boolean mem, ReasembList reasmb) {
        addElement(new Decomp(word, mem, reasmb));
    }

    /**
     *  Print the whole decomp list.
     */
    public void print(int indent) {
        for (int i = 0; i < size(); i++) {
            Decomp d = (Decomp)elementAt(i);
            d.print(indent);
        }
    }
}

