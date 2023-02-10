package Eliza;

import java.util.Vector;

/**
 *  Eliza word list.
 */
public class WordList extends Vector {

    /**
     *  Add another word to the list.
     */
    public void add(String word) {
        addElement(word);
    }

    /**
     *  Print a word list on one line.
     */
    public void print(int indent) {
        for (int i = 0; i < size(); i++) {
            String s = (String)elementAt(i);
            System.out.print(s + "  ");
        }
        System.out.println();
    }

    /**
     *  Find a string in a word list.
     *  Return true if the word is in the list, false otherwise.
     */
    boolean find(String s) {
        for (int i = 0; i < size(); i++) {
            if (s.equals((String)elementAt(i))) return true;
        }
        return false;
    }

}

