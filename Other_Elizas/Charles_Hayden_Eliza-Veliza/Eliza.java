package Eliza;

import java.applet.*;
import java.awt.*;

public class Eliza extends Applet {

    static String scriptPathname = "c:\\cch\\eliza\\script";
    static String testPathname = "c:\\cch\\eliza\\test";
    static String scriptURL = "http://www.monmouth.com/~chayden/eliza/script";
    static String testURL = "http://www.monmouth.com/~chayden/eliza/test";
    //static String testURL = "http://www-gbcs.mt.att.com/~cch/eliza/test";

    boolean useWindow = true;
    boolean local = false;

    ElizaMain eliza;

    public void init() {
        showStatus("Loading Eliza");
        eliza = new ElizaMain();
    }

    public void start() {
        String script = getScriptParam();
        String test = getTestParam();
        if (local) {
            script = scriptPathname;
            test = testPathname;
        }
        showStatus("Loading script from " + script);
        eliza.readScript(local, script);
        showStatus("Ready");
        if (useWindow)
            eliza.runProgram(test, this);
        else
            eliza.runProgram(test, null);
    }

    public boolean handleEvent(Event e) {
        return eliza.handleEvent(e);
    }

    String getScriptParam() {
        String script = getParameter("script");
        if (script == null) script = scriptURL;
        return script;
    }

    String getTestParam() {
        String test = getParameter("test");
        if (test == null) test = testURL;
        return test;
    }

    public String[][] getParameterInfo() {
        String[][] info = {
            {"script", "URL", "URL of script file"},
            {"test", "URL", "URL of test file"}
        };
        return info;
    }

    public String getAppletInfo() {
        return "Eliza v0.1 written by Charles Hayden chayden@monmouth.com";
    }


}
