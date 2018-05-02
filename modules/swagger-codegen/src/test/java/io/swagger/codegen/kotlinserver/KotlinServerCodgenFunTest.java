package io.swagger.codegen.kotlinserver;

import io.swagger.codegen.languages.KotlinServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class KotlinServerCodgenFunTest {

    @Test(description = "camelCase operationId for function name")
    public void operationIdCamelCase(){
        final KotlinServerCodegen codegen = new KotlinServerCodegen();
        Assert.assertEquals(codegen.toOperationId("Get Pony_name"), "getPonyName");
        Assert.assertEquals(codegen.toOperationId("_"), "underscore");
        Assert.assertEquals(codegen.toOperationId("__"), "underscoreUnderscore");
        Assert.assertEquals(codegen.toOperationId("___"), "underscoreUnderscoreUnderscore");
        Assert.assertEquals(codegen.toOperationId("1st"), "_1st");
        Assert.assertEquals(codegen.toOperationId("123"), "_123");
        Assert.assertEquals(codegen.toOperationId("14710C3C-70B7-4DCC-A529-DBB6BD004D1D"), "_14710C3C70B74DCCA529DBB6BD004D1D");
    }

}
