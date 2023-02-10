package Eliza;

/**
 *  Eliza string functions.
 */
public class EString {

    /** The digits. */
    static final String num = "0123456789";

    /**
     *  Look for a match between the string and the pattern.
     *  Return count of maching characters before * or #.
     *  Return -1 if strings do not match.
     */
    public static int amatch(String str, String pat) {
        int count = 0;
        int i = 0;  // march through str
        int j = 0;  // march through pat
        while (i < str.length() && j < pat.length()) {
            char p = pat.charAt(j);
            // stop if pattern is * or #
            if (p == '*' || p == '#') return count;
            if (str.charAt(i) != p) return -1;
            // they are still equal
            i++; j++; count++;
        }
        return count;
    }

    /**
     *  Search in successive positions of the string,
     *  looking for a match to the pattern.
     *  Return the string position in str of the match,
     *  or -1 for no match.
     */
    public static int findPat(String str, String pat) {
        int count = 0;
        for (int i = 0; i < str.length(); i++) {
            if (amatch(str.substring(i), pat) >= 0)
                return count;
            count++;
        }
        return -1;
    }

    /**
     *  Look for a number in the string.
     *  Return the number of digits at the beginning.
     */
    public static int findNum(String str) {
        int count = 0;
        for (int i = 0; i < str.length(); i++) {
            if (num.indexOf(str.charAt(i)) == -1)
                return count;
            count++;
        }
        return count;
    }

    /**
     *  Match the string against a pattern and fills in
     *  matches array with the pieces that matched * and #
     */
    static boolean matchA(String str, String pat, String matches[]) {
        int i = 0;      //  move through str
        int j = 0;      //  move through matches
        int pos = 0;    //  move through pat
        while (pos < pat.length() && j < matches.length) {
            char p = pat.charAt(pos);
            if (p == '*') {
                int n;
                if (pos+1 == pat.length()) {
                    //  * is the last thing in pat
                    //  n is remaining string length
                    n = str.length() - i;
                } else {
                    //  * is not last in pat
                    //  find using remaining pat
                    n = findPat(str.substring(i), pat.substring(pos+1));
                }
                if (n < 0) return false;
                matches[j++] = str.substring(i, i+n);
                i += n; pos++;
            } else if (p == '#') {
                int n = findNum(str.substring(i));
                matches[j++] = str.substring(i, i+n);
                i += n; pos++;
            } else {
                int n = amatch(str.substring(i), pat.substring(pos));
                if (n <= 0) return false;
                i += n; pos += n;
            }
        }
        if (i >= str.length() && pos >= pat.length()) return true;
        return false;
    }

    /*
     *  This version is clearer, but hopelessly slow
     */
    static boolean matchB(String strIn, String patIn, String matches[]) {
        String str = new String(strIn);
        String pat = new String(patIn);
        int j = 0;      //  move through matches
        while (pat.length() > 0 && str.length() >= 0 && j < matches.length) {
            char p = pat.charAt(0);
            if (p == '*') {
                int n;
                if (pat.length() == 1) {
                    //  * is the last thing in pat
                    //  n is remaining string length
                    n = str.length();
                } else {
                    //  * is not last in pat
                    //  find using remaining pat
                    n = findPat(str, pat.substring(1));
                }
                if (n < 0) return false;
                matches[j++] = str.substring(0, n);
                str = str.substring(n);
                pat = pat.substring(1);
            } else if (p == '#') {
                int n = findNum(str);
                matches[j++] = str.substring(0, n);
                str = str.substring(n);
                pat = pat.substring(1);
 //           } else if (p == ' ' && str.length() > 0 && str.charAt(0) != ' ') {
 //               pat = pat.substring(1);
            } else {
                int n = amatch(str, pat);
                if (n <= 0) return false;
                str = str.substring(n);
                pat = pat.substring(n);
            }
        }
        if (str.length() == 0 && pat.length() == 0) return true;
        return false;
    }

    public static boolean match(String str, String pat, String matches[]) {
        return matchA(str, pat, matches);
    }

    /*
     *  Translates corresponding characters in src to dest.
     *  Src and dest must have the same length.
     */
    public static String translate(String str, String src, String dest) {
        if (src.length() != dest.length()) {
            // impossible error
        }
        for (int i = 0; i < src.length(); i++) {
            str = str.replace(src.charAt(i), dest.charAt(i));
        }
        return str;
    }

    /**
     *  Compresses its input by:
     *    dropping space before space, comma, and period;
     *    adding space before question, if char before is not a space; and
     *    copying all others
     */
    public static String compress(String s) {
        String dest = "";
        if (s.length() == 0) return s;
        char c = s.charAt(0);
        for (int i = 1; i < s.length(); i++) {
            if (c == ' ' &&
                 ((s.charAt(i) == ' ') ||
                 (s.charAt(i) == ',') ||
                 (s.charAt(i) == '.'))) {
                    // nothing
            } else if (c != ' ' && s.charAt(i) == '?') {
                dest += c + " ";
            } else {
                dest += c;
            }
            c = s.charAt(i);
        }
        dest += c;
        return dest;
    }

    /**
     *  Trim off leading space
     */
    public static String trim(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != ' ') return s.substring(i);
        }
        return "";
    }

    /**
     *  Pad by ensuring there are spaces before and after the sentence.
     */
    public static String pad(String s) {
        if (s.length() == 0) return " ";
        char first = s.charAt(0);
        char last = s.charAt(s.length()-1);
        if (first == ' ' && last == ' ') return s;
        if (first == ' ' && last != ' ') return s + " ";
        if (first != ' ' && last == ' ') return " " + s;
        if (first != ' ' && last != ' ') return " " + s + " ";
        // impossible
        return s;
    }

    /**
     *  Count number of occurrances of c in str
     */
    public static int count(String s, char c) {
        int count = 0;
        for (int i = 0; i < s.length(); i++)
            if (s.charAt(i) == c) count++;
        return count;
    }
}