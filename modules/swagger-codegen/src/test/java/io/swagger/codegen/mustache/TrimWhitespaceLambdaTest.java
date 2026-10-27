package io.swagger.codegen.mustache;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

import java.io.IOException;
import java.io.StringWriter;

import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.samskivert.mustache.Template.Fragment;

public class TrimWhitespaceLambdaTest {

    @Mock
    private Fragment fragment;

    @BeforeMethod
    public void init() {
        MockitoAnnotations.initMocks(this);
    }

    @AfterMethod
    public void reset() {
        Mockito.reset(fragment);
    }

    @Test
    public void testTrimWhitespace() throws IOException {
        when(fragment.execute()).thenReturn("\t a  b\t\tc \t");

        StringWriter output = new StringWriter();
        new TrimWhitespaceLambda().execute(fragment, output);
        assertEquals(output.toString(), " a b c ");
    }

}
