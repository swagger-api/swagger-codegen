package io.swagger.codegen.languages;

public class PureCloudJavascriptClientCodegen extends JavascriptClientCodegen {

    public PureCloudJavascriptClientCodegen() {
        super();
    }

    @Override
    //Swagger definition has a model called Number which is a reserved word
    public String toModelName(String name) {
        if (reservedWords.contains(name)) {
            name = name + "Value";
        }

        return super.toModelName(name);
    }

    @Override
    public String getName() {
        return "purecloudjavascript";
    }


}
