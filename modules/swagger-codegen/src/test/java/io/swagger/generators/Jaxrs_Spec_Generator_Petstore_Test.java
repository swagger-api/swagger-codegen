package io.swagger.generators;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.generators.JaxrsSpec_Generator_Petstore;
import io.swagger.codegen.generators.util.ComparePathUtil;

public class Jaxrs_Spec_Generator_Petstore_Test extends Abstract_Generator_Test {

    private static final Logger LOGGER = LoggerFactory.getLogger(Jaxrs_Spec_Generator_Petstore_Test.class);

    JaxrsSpec_Generator_Petstore generator = new JaxrsSpec_Generator_Petstore();
    
    @Before
    public void setup() throws IOException {
    	super.setup();
    	generator.generateToFolder(folder.getRoot());
    }

    @Test
    @Ignore
    public void test() {
        File output = folder.getRoot();
        java.nio.file.Path actualPath = FileSystems.getDefault().getPath(output.getAbsolutePath());

        java.nio.file.Path samplePath = FileSystems.getDefault().getPath(generator.getSamplesFolder());
        LOGGER.info("path: " + samplePath.toFile().getAbsolutePath());

        try {
            assertTrue(ComparePathUtil.isEqualDirectories(actualPath, samplePath, true));

        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }

}
