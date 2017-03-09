package io.swagger.codegen.generators;

import java.io.File;

public class EclipseFilenameFilter implements java.io.FilenameFilter {

    @Override
    public boolean accept(File dir, String name) {

    	// TODO don't compare swagger.json for now
        if (".settings".equals(name) || "target".equals(name) || ".classpath".equals(name) || ".project".equals(name) || "swagger.json".equals(name)) {
            return false;
        } else {
            return true;
        }

    }

}
