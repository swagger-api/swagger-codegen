package io.swagger.codegen.languages;

import io.swagger.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PureCloudDotNetGuestClientCodegen extends PureCloudDotNetClientCodegen {
    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudDotNetGuestClientCodegen.class);

    @Override
    public String getName() {
        return "pureclouddotnet-guest";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // We don't need these test templates added by the super method because there are no tests in the guest chat SDK
        supportingFiles.remove(new SupportingFile("test-packages.mustache", "", "src/" + this.packageName + ".Tests/packages.config"));
        supportingFiles.remove(new SupportingFile("test-AssemblyInfo.mustache", "", "src/" + this.packageName + ".Tests/Properties/AssemblyInfo.cs"));
        supportingFiles.remove(new SupportingFile("test-csproj.mustache", "", "src/" + this.packageName + ".Tests/" + this.packageName + ".Tests.csproj"));
        supportingFiles.remove(new SupportingFile("test-SdkTests.mustache", "", "src/" + this.packageName + ".Tests/SdkTests.cs"));
    }
}
