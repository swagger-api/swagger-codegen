package io.swagger.codegen.typescript.typescriptangular;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.TypeScriptAngularClientCodegen;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.testng.Assert;
import org.testng.annotations.Test;

public class TypescriptAngularCodegenTest {

	private CodegenOperation createCodegenOperationFromTestData(String path){
		Swagger model = new SwaggerParser().read("src/test/resources/2_0/petstore-nested-maps-or-arrays.json");
		DefaultCodegen codegen = new TypeScriptAngularClientCodegen();
		Operation p = model.getPaths().get(path).getGet();
		return codegen.fromOperation(path, "get", p, model.getDefinitions());
	}

	@Test(description = "sets imports of map of array to child object")
	public void mapOfArrayOfObjectTest() {
		final CodegenOperation op = createCodegenOperationFromTestData("/one");

		Assert.assertEquals(op.returnType, "{ [key: string]: Array<OneDto>; }");
		Assert.assertEquals(op.imports.size(), 1);
		Assert.assertEquals(op.imports, Sets.newHashSet("OneDto"));
	}

	@Test(description = "sets imports of array of array to nested child object")
	public void arrayOfArrayOfObjectTest() {
		final CodegenOperation op = createCodegenOperationFromTestData("/two");

		Assert.assertEquals(op.returnType, "Array<Array<OneDto>>");
		Assert.assertEquals(op.imports.size(), 1);
		Assert.assertEquals(op.imports, Sets.newHashSet("OneDto"));
	}

	@Test(description = "sets imports of array to child object")
	public void ArrayOfObjectTest() {
		final CodegenOperation op = createCodegenOperationFromTestData("/three");

		Assert.assertEquals(op.returnType, "Array<OneDto>");
		Assert.assertEquals(op.imports.size(), 1);
		Assert.assertEquals(op.imports, Sets.newHashSet("OneDto"));
	}

	@Test(description = "no imports for array of primitive type")
	public void ArrayOfStringTest() {
		final CodegenOperation op = createCodegenOperationFromTestData("/four");

		Assert.assertEquals(op.returnType, "Array<string>");
		Assert.assertEquals(op.imports.size(), 0);
	}

	@Test(description = "sets imports of map of map of array to child object")
	public void mapOfMapOfArrayOfObjectTest() {
		final CodegenOperation op = createCodegenOperationFromTestData("/five");

		Assert.assertEquals(op.returnType, "{ [key: string]: { [key: string]: Array<OneDto>; }; }");
		Assert.assertEquals(op.imports.size(), 1);
		Assert.assertEquals(op.imports, Sets.newHashSet("OneDto"));
	}
}
