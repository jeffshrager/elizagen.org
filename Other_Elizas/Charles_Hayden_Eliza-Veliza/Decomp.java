package Eliza;

import java.lang.Math;

/**
 *  Eliza decomposition rule
 */
public class Decomp {
    /** The decomp pattern */
    String pattern;
    /** The mem flag */
    boolean mem;
    /** The reassembly list */
    ReasembList reasemb;
    /** The current reassembly point */
    int currReasmb;

    /**
     *  Initialize the decomp rule
     */
    Decomp(String pattern, boolean mem, ReasembList reasemb) {
        this.pattern = pattern;
        this.mem = mem;
        this.reasemb = reasemb;
        this.currReasmb = 100;
    }

    /**
     *  Print out the decomp rule.
     */
    public void print(int indent) {
        String m = mem ? "true" : "false";
        for (int i = 0; i < indent; i++) System.out.print(" ");
        System.out.println("decomp: " + pattern + " " + m);
        reasemb.print(indent + 2);
    }

    /**
     *  Get the pattern.
     */
    public String pattern() {
        return pattern;
    }

    /**
     *  Get the mem flag.
     */
    public boolean mem() {
        return mem;
    }

    /**
     *  Get the next reassembly rule.
     */
    public String nextRule() {
        if (reasemb.size() == 0) {
            System.out.println("No reassembly rule.");
            return null;
        }
        return (String)reasemb.elementAt(currReasmb);
    }

    /**
     *  Step to the next reassembly rule.
     *  If mem is true, pick a random rule.
     */
    public void stepRule() {
        int size = reasemb.size();
        if (mem) {
            currReasmb = (int)(Math.random() * size);
        }
        //  Increment and make sure it is within range.
        currReasmb++;
        if (currReasmb >= size) currReasmb = 0;
    }


}

