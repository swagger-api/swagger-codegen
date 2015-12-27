package io.swagger.codegen;

import ch.lambdaj.Lambda;
import ch.lambdaj.collection.LambdaIterable;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.codegen.gui.*;
import io.swagger.codegen.gui.MenuBar;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import static ch.lambdaj.Lambda.on;
import static ch.lambdaj.collection.LambdaCollections.with;
import static java.util.ServiceLoader.load;

/**
 * User: championswimmer
 * Date: 27/12/15.
 * <p>
 *     Graphical interface for swagger codegen
 * </p>
 */
public class SwaggerCodegen extends JFrame{


    public void start () {
        setTitle("Swagger Codegen");

        setJMenuBar(new MenuBar().initialise());

        addComponentsToPane(getContentPane());

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        pack();
        setVisible(true);


    }


    public static void addComponentsToPane(final Container pane) {
        //Lay out the components vertically in the root pane
        pane.setLayout(new BoxLayout(pane, BoxLayout.PAGE_AXIS));

        //Add input file pane
        final JTextField inputFileField = new JTextField("Select swagger spec file");
        inputFileField.setEditable(false);
        JLabel inputFileLabel = new JLabel("Input file");
        inputFileLabel.setLabelFor(inputFileField);
        JButton inputFileBrowse = new JButton("Browse");
        inputFileBrowse.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                JFileChooser fileChooser = new JFileChooser();
                fileChooser.showOpenDialog(pane);
                inputFileField.setText(fileChooser.getSelectedFile().getAbsolutePath());
            }
        });
        JPanel inputFilePanel = new JPanel();

        inputFilePanel.add(inputFileLabel);
        inputFilePanel.add(inputFileField);
        inputFilePanel.add(inputFileBrowse);

        // Add output directory selector panel
        final JTextField outputDirField = new JTextField("Select output folder");
        outputDirField.setEditable(false);
        JLabel outputDirLabel = new JLabel("Output dir");
        outputDirLabel.setLabelFor(outputDirField);
        JButton outputDirBrowse = new JButton("Browse");
        outputDirBrowse.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                JFileChooser fileChooser = new JFileChooser();
                fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                fileChooser.setAcceptAllFileFilterUsed(false);
                fileChooser.showOpenDialog(pane);
                outputDirField.setText(fileChooser.getSelectedFile().getAbsolutePath());
            }
        });
        JPanel outputDirPanel = new JPanel();

        outputDirPanel.add(outputDirLabel);
        outputDirPanel.add(outputDirField);
        outputDirPanel.add(outputDirBrowse);


        // Add language selector pane
        LambdaIterable<String> langs = with(load(CodegenConfig.class)).extract(on(CodegenConfig.class).getName());
        final JComboBox<String> langCombo = new JComboBox<String>();
        for (String s : langs) {
            langCombo.addItem(s);
        }
        JLabel langSelectLabel = new JLabel("Language  :  ");
        langSelectLabel.setLabelFor(langCombo);
        JPanel langSelectorPanel = new JPanel();
        langSelectorPanel.add(langSelectLabel);
        langSelectorPanel.add(langCombo);


        //Add generate button to bottom
        JPanel buttonPanel = new JPanel();
        JButton generateButton = new JButton("Generate");
        generateButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                run (
                        inputFileField.getText(),
                        outputDirField.getText(),
                        langCombo.getSelectedItem().toString()
                );
            }
        });
        buttonPanel.add(generateButton);

        // Attach everything to the root pane
        pane.add(inputFilePanel); // Attach input panel to root pane
        pane.add(outputDirPanel); // Attach output directory panel
        pane.add(langSelectorPanel); // Add the swagger supported languages here
        pane.add(buttonPanel); // Attach buttons to the bottom



    }

    public static void run (String inputSpec, String outputDir, String lang) {
        CodegenConfigurator configurator = new CodegenConfigurator();
        configurator.setInputSpec(inputSpec);
        configurator.setOutputDir(outputDir);
        configurator.setLang(lang);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        new DefaultGenerator().opts(clientOptInput).generate();
    }

    public static void main(String[] args) {

        try {
            // Set System L&F
            UIManager.setLookAndFeel(
                    UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            // Could not set default L&F
        }

        new SwaggerCodegen().start();
    }


}
