package com.wordnik.swagger.codegen;

import static org.junit.Assert.*;
import com.wordnik.swagger.models.Operation;
import com.wordnik.swagger.models.RefModel;
import com.wordnik.swagger.models.parameters.BodyParameter;
import org.junit.Test;

public class DefaultCodegenTest {

  @Test
  public void fromOperation_refModel_import() {
    // GIVEN: an operation with a body parameter that references a user-defined model-class
    Operation op = new Operation();
    BodyParameter bodyParameter = new BodyParameter();
    bodyParameter.setSchema(new RefModel("MyCustomClass"));
    op.addParameter(bodyParameter);

    // WHEN: create a codegeneration-model
    CodegenOperation genOp = new DefaultCodegen().fromOperation("/somePath", "post", op);

    // THEN: the model-class must be imported
    assertEquals(1, genOp.imports.size());
    assertEquals("MyCustomClass", genOp.imports.iterator().next());
  }
}
