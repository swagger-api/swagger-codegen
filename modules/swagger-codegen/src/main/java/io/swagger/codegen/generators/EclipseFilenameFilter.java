package io.swagger.codegen.generators;

import java.io.File;

public class EclipseFilenameFilter implements java.io.FilenameFilter {

    @Override
    public boolean accept(File dir, String name) {

        if (".settings".equals(name) || "target".equals(name) || ".classpath".equals(name) || ".project".equals(name)) {
            return false;
        } else {
            return true;
        }

    }

}
