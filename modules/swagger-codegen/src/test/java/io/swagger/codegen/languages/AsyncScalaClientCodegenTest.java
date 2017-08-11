package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenProperty;
import junit.framework.Assert;
import org.mockito.Mockito;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class AsyncScalaClientCodegenTest {

    private AsyncScalaClientCodegen asyncScalaClientCodegen;

    @BeforeTest
    public void setup() {
        this.asyncScalaClientCodegen = new AsyncScalaClientCodegen();
    }

    @Test
    public void shouldCallFormatIdentifierOnGetModelName() {
        String className = "models.WebsiteBodyModel";

        AsyncScalaClientCodegen asyncScalaClientCodegenSpy = Mockito.spy(asyncScalaClientCodegen);

        String result = asyncScalaClientCodegenSpy.toModelName(className);

        verify(asyncScalaClientCodegenSpy, times(1)).stripPackageName(anyString());
        verify(asyncScalaClientCodegenSpy, times(1)).formatIdentifier(anyString(), anyBoolean());
        Assert.assertEquals("WebsiteBodyModel", result);
    }

    @Test
    public void shouldCallFormatIdentifierOnToEnumName() {
        String className = "models.WebsiteBodyModel";

        AsyncScalaClientCodegen asyncScalaClientCodegenSpy = Mockito.spy(asyncScalaClientCodegen);

        CodegenProperty property = new CodegenProperty();
        property.baseName = className;
        String result = asyncScalaClientCodegenSpy.toEnumName(property);

        verify(asyncScalaClientCodegenSpy, times(1)).stripPackageName(anyString());
        verify(asyncScalaClientCodegenSpy, times(1)).formatIdentifier(anyString(), anyBoolean());
        Assert.assertEquals("WebsiteBodyModel", result);
    }
}
