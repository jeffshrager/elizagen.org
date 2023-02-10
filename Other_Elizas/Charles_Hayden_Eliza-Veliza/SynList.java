package Eliza;

import java.util.Vector;

/**
 *  Eliza synonym list.
 *  Collection of all the synonym elements.
 */
public class SynList extends Vector {

    /**
     *  Add another word list the the synonym list.
     */
    public void add(WordList words) {
        addElement(words);
    }

    /**
     *  Prnt the synonym lists.
     */
    public void print(int indent) {
        for (int i = 0; i < size(); i++) {
            for (int j = 0; j < indent; j++) System.out.print(" ");
            System.out.print("synon: ");
            WordList w = (WordList)elementAt(i);
            w.print(indent);
        }
    }

    /**
     *  Find a synonym word list given the any word in it.
     */
    public WordList find(String s) {
        for (int i = 0; i < size(); i++) {
            WordList w = (WordList)elementAt(i);
            if (w.find(s)) return w;
        }
        return null;
    }
    /**
     *  Decomposition match,
     *  If decomp has no synonyms, do a regular match.
     *  Otherwise, try all synonyms.
     */
    boolean matchDecomp(String str, String pat, String lines[]) {
        if (! EString.match(pat, "*@* *", lines)) {
            //  no synonyms in decomp pattern
            return EString.match(str, pat, lines);
        }
        //  Decomp pattern has synonym -- isolate the synonym
        String first = lines[0];
        String synWord = lines[1];
        String theRest = " " + lines[2];
        //  Look up the synonym
        WordList syn = find(synWord);
        if (syn == null) {
            System.out.println("Could not fnd syn list for " + synWord);
            return false;
        }
        //  Try each synonym individually
        for (int i = 0; i < syn.size(); i++) {
            //  Make a modified pattern
            pat = first + (String)syn.elementAt(i) + theRest;
            if (EString.match(str, pat, lines)) {
                int n = EString.count(first, '*');
                //  Make room for the synonym in the match list.
                for (int j = lines.length-2; j >= n; j--)
                    lines[j+1] = lines[j];
                //  The synonym goes in the match list.
                lines[n] = (String)syn.elementAt(i);
                return true;
            }
        }
        return false;
    }

}
