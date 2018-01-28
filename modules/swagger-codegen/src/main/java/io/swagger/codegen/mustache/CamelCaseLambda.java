package io.swagger.codegen.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.DefaultCodegen;

import java.io.IOException;
import java.io.Writer;

/**
 * Converts text in a fragment to camelCase.
 *
 * Register:
 * <pre>
 * additionalProperties.put("camelcase", new CamelCaseLambda());
 * </pre>
 *
 * Use:
 * <pre>
 * {{#camelcase}}{{name}}{{/camelcase}}
 * </pre>
 */
public class CamelCaseLambda implements Mustache.Lambda {
    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        String text = fragment.execute();
        writer.write(DefaultCodegen.camelize(text, true));
    }
}
