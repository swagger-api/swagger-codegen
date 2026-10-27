package io.swagger.codegen.mustache;

import java.io.IOException;
import java.io.Writer;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;

/**
 * Replaces duplicate whitespace characters in a fragment with single space.
 *
 * Register:
 *
 * <pre>
 * additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());
 * </pre>
 *
 * Use:
 *
 * <pre>
 * {{#lambdaTrimWhitespace}}{{summary}}{{/lambdaTrimWhitespace}}
 * </pre>
 */
public class TrimWhitespaceLambda implements Mustache.Lambda {
    private static final String SINGLE_SPACE = " ";

    private static final String WHITESPACE_REGEX = "\\s+";

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        writer.write(fragment.execute().replaceAll(WHITESPACE_REGEX, SINGLE_SPACE));
    }

}
