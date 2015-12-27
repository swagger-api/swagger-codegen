package io.swagger.codegen.gui;

import javax.swing.*;

/**
 * Created by championswimmer on 27/12/15.
 */
public class MenuBar extends JMenuBar {
    private static MenuBar menuBar;

    public MenuBar initialise () {
        addFileMenu();
        addHelpMenu();

        return this;
    }

    public void addFileMenu () {
        JMenu fileMenu = new JMenu("File");
        add(fileMenu);
    }

    public void addHelpMenu () {
        JMenu helpMenu = new JMenu("Help");
        add(helpMenu);
    }
}
