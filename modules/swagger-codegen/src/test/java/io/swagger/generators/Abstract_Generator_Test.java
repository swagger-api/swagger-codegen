package io.swagger.generators;

import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.rules.TemporaryFolder;

public abstract class Abstract_Generator_Test {

    TemporaryFolder folder;
    
    @Before
    public void setup() throws IOException {
    	folder = new TemporaryFolder();
        folder.create();
    }
    

    @After
    public void teardown() {
        // cleanup temporary folder if everything is OK
        folder.delete();
    }

}
