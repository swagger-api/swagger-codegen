package io.swagger.codegen.mustache;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.samskivert.mustache.Template.Fragment;

public class SplitStringLambdaTest {
    private static final String INPUT_STRING = "1112223334";

    private static final Map<Integer, String> EXPECTED_OUTPUTS;
    static {
        EXPECTED_OUTPUTS = new HashMap<>();
        EXPECTED_OUTPUTS.put(2,
                String.format(
                        "new StringBuilder(%d).append(\"11\").append(\"12\").append(\"22\").append(\"33\").append(\"34\").toString()",
                        INPUT_STRING.length()));
        EXPECTED_OUTPUTS.put(3,
                String.format(
                        "new StringBuilder(%d).append(\"111\").append(\"222\").append(\"333\").append(\"4\").toString()",
                        INPUT_STRING.length()));
    }

    private static final String INPUT_QUOTED_STRING = "1\\\"11\\\"2223\\\"334";
    private static final String INPUT_QUOTED_OUTPUT = String.format(
            "new StringBuilder(%d).append(\"1\\\"\").append(\"11\").append(\"\\\"2\").append(\"223\").append(\"\\\"3\").append(\"34\").toString()",
            INPUT_QUOTED_STRING.length());

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

    private void testString(String input, int maxLength, String expected) throws IOException {
        when(fragment.execute()).thenReturn(input);

        StringWriter output = new StringWriter();
        new SplitStringLambda(maxLength).execute(fragment, output);
        assertEquals(output.toString(), expected);
    }

    @Test
    public void testSplitGroupsOf2() throws IOException {
        int maxLength = 2;
        testString(INPUT_STRING, maxLength, EXPECTED_OUTPUTS.get(maxLength));
    }

    @Test
    public void testSplitGroupsOf3() throws IOException {
        int maxLength = 3;
        testString(INPUT_STRING, maxLength, EXPECTED_OUTPUTS.get(maxLength));
    }

    @Test
    public void testSplitQuotedString() throws IOException {
        int maxLength = 3;
        testString(INPUT_QUOTED_STRING, maxLength, INPUT_QUOTED_OUTPUT);
    }

    @Test
    public void testShortString() throws IOException {
        testString(INPUT_STRING, INPUT_STRING.length(), String.format("\"%s\"", INPUT_STRING));
    }

}
