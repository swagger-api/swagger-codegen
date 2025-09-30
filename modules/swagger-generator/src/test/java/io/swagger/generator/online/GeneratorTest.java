package io.swagger.generator.online;

import org.testng.annotations.Test;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.HashMap;
import java.util.Map;

/**
 * Unit test for Generator class to verify SecurityException handling
 * when SecureFileUtils.validatePath throws exceptions.
 */
public class GeneratorTest {

    @Test(expectedExceptions = SecurityException.class)
    public void testGenerateWithPathTraversalInOutputFolder() throws Exception {
        io.swagger.generator.model.GeneratorInput opts = new io.swagger.generator.model.GeneratorInput();

        ObjectMapper mapper = new ObjectMapper();
        JsonNode spec = mapper.readTree(
            "{\"swagger\":\"2.0\",\"info\":{\"title\":\"Test\",\"version\":\"1.0\"}," +
            "\"paths\":{\"/test\":{\"get\":{\"responses\":{\"200\":{\"description\":\"OK\"}}}}}}"
        );
        opts.setSpec(spec);

        Map<String, String> options = new HashMap<>();
        options.put("outputFolder", "../../../etc/passwd");
        opts.setOptions(options);

        Generator.generateClient("java", opts);
    }
}
