package io.swagger.codegen.mustache;

import java.io.IOException;
import java.io.Writer;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template.Fragment;

/**
 * Splits long fragments into smaller strings and uses a StringBuilder to merge
 * them back.
 *
 * Register:
 *
 * <pre>
 * additionalProperties.put("lambdaSplitString", new SplitStringLambda());
 * </pre>
 *
 * Use:
 *
 * <pre>
 * {{#lambdaSplitString}}{{summary}}{{/lambdaSplitString}}
 * </pre>
 */
public class SplitStringLambda implements Mustache.Lambda {
    private static final int DEFAULT_MAX_LENGTH = 65535;

    private static final String SPLIT_INIT = "new StringBuilder(%d)";

    private static final String SPLIT_PART = ".append(\"%s\")";

    private static final String SPLIT_SUFFIX = ".toString()";

    private final int maxLength;

    public SplitStringLambda() {
        this(DEFAULT_MAX_LENGTH);
    }

    public SplitStringLambda(int maxLength) {
        this.maxLength = maxLength;
    }

    @Override
    public void execute(Fragment fragment, Writer writer) throws IOException {
        String input = fragment.execute();
        int inputLength = input.length();

        StringBuilder builder = new StringBuilder();
        if (inputLength > maxLength) {

            // Initialize a StringBuilder
            builder.append(String.format(SPLIT_INIT, inputLength));

            int currentPosition = 0;
            int currentStringLength = 0;
            char currentLastChar = '\\';

            // Split input into parts of at most maxLength and not ending with an escape character
            // Append each part to the StringBuilder
            while (currentPosition + maxLength < input.length()) {
                currentStringLength = maxLength;
                currentLastChar = input.charAt(currentPosition + currentStringLength - 1);
                if (currentLastChar == '\\') {
                    --currentStringLength;
                }

                builder.append(String.format(SPLIT_PART, input.substring(currentPosition, currentPosition + currentStringLength)));
                currentPosition += currentStringLength;
            }

            // Append last part if necessary
            if (currentPosition < input.length()) {
                builder.append(String.format(SPLIT_PART, input.substring(currentPosition)));
            }

            // Close the builder and merge everything back to a string
            builder.append(SPLIT_SUFFIX);
        } else {
            builder.append(String.format("\"%s\"", input));
        }

        writer.write(builder.toString());
    }

}
