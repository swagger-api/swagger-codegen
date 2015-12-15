package io.swagger.codegen.haxenodejs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.HaxeNodeJSServerCodegen;
import io.swagger.codegen.options.HaxeNodeJSServerOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

/**
 * Created by jonathanjayet on 15/12/15.
 */
public class HaxeNodeJSServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private HaxeNodeJSServerCodegen clientCodegen;

    public HaxeNodeJSServerOptionsTest() {
        super(new HaxeNodeJSServerOptionsProvider());
    }



    @Override
    protected CodegenConfig getCodegenConfig() { return clientCodegen; }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(HaxeNodeJSServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
        }};
    }
}
