package io.swagger.codegen.languages;

public class PureCloudCSharpClientCodegen extends CSharpClientCodegen {

    public PureCloudCSharpClientCodegen() {
        super();

        // Override some variables
        packageTitle = "PureCloud Public API Library";
        packageProductName = "PureCloudPublicAPILibrary";
        packageDescription = "A .NET library to interface with the PureCloud Public API";
        packageCompany = "Interactive Intelligence, Inc.";
        packageCopyright = "Copyright © Interactive Intelligence, Inc. 2015";
        packageName = "ININ.PureCloud.PublicAPI";
        apiPackage = packageName + ".Api";
        modelPackage = packageName + ".Model";
        clientPackage = packageName + ".Client";
        //TODO: determine this programatically somehow
        packageVersion = "0.0.0.1";

        /* There is a type named "PureCloud" that must be fully qualified since the compiler assumes that "PureCloud"
         * means the namespace "ININ.PureCloud"
         */
        typeMapping.put("PureCloud", "ININ.PureCloud.PublicAPI.Model.PureCloud");
    }

    @Override
    public String getName() {
        return "purecloudcsharp";
    }
}
