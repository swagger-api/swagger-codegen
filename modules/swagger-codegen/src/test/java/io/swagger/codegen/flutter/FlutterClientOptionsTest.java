package io.swagger.codegen.flutter;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.FlutterClientCodegen;
import io.swagger.codegen.options.DartClientOptionsProvider;
import io.swagger.codegen.options.FlutterClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class FlutterClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private FlutterClientCodegen clientCodegen;

    public FlutterClientOptionsTest() {
        super(new FlutterClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(DartClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setPubName(FlutterClientOptionsProvider.PUB_NAME_VALUE);
            times = 1;
            clientCodegen.setPubVersion(FlutterClientOptionsProvider.PUB_VERSION_VALUE);
            times = 1;
            clientCodegen.setPubDescription(FlutterClientOptionsProvider.PUB_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(FlutterClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setUseEnumExtension(Boolean.valueOf(FlutterClientOptionsProvider.USE_ENUM_EXTENSION));
            times = 1;
        }};
    }
}
