package io.swagger.codegen.languages;

import org.mockito.Mockito;
import org.testng.annotations.Test;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class AkkaScalaCodegenTest {

    @Test
    public void shouldCallFormatIdentifierOnGetModelName() {
        String className = "models.WebsiteBodyModel";

        AkkaScalaClientCodegen akkaScalaClientCodegen = new AkkaScalaClientCodegen();
        AkkaScalaClientCodegen akkaScalaClientCodegenSpy = Mockito.spy(akkaScalaClientCodegen);

        akkaScalaClientCodegenSpy.toModelName(className);

        verify(akkaScalaClientCodegenSpy, times(1)).formatIdentifier(anyString(), anyBoolean());
    }
}
