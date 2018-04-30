package io.swagger.codegen.kotlinserver;

import io.swagger.codegen.languages.KotlinServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class KotlinServerCodgenFunTest {

    @Test(description = "camelCase operationId for function name")
    public void operationIdCamelCase(){
        final KotlinServerCodegen codegen = new KotlinServerCodegen();
        Assert.assertEquals(codegen.toOperationId("Get Pony_name"), "getPonyName");
    }
}
