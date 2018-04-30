package io.swagger.codegen.kotlin;

import io.swagger.codegen.languages.KotlinClientCodegen;

import org.testng.Assert;
import org.testng.annotations.Test;

public class KotlinClientCodgenFunTest {

    @Test(description = "camelCase operationId for function name")
    public void operationIdCamelCase(){
        final KotlinClientCodegen codegen = new KotlinClientCodegen();
        Assert.assertEquals(codegen.toOperationId("Get Pony_name"), "getPonyName");
    }
}
