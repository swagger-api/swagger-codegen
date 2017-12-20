package io.swagger.codegen.akkascala;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.languages.AkkaScalaClientCodegen;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.Assert.assertThat;

public class AkkaScalaClientCodegenTest {

    private AkkaScalaClientCodegen akkaScalaClientCodegen;

    @Before
    public void setup() {
        this.akkaScalaClientCodegen = new AkkaScalaClientCodegen();
    }

    @Test
    public void shouldGenerateReadmeFile() {
        List<SupportingFile> supportingFiles = this.akkaScalaClientCodegen.supportingFiles();

        assertThat(supportingFiles.contains(new SupportingFile("README.mustache", "", "README.md")), is(equalTo(true)));
        assertThat(supportingFiles.contains(new SupportingFile("README.mustache", "", "README.md")), is(equalTo(true)));
    }

    @Test
    public void shouldGenerateBuildSbt() {
        this.akkaScalaClientCodegen.additionalProperties().put(CodegenConstants.BUILD_TOOL, "SBT");
        this.akkaScalaClientCodegen.processOpts();
        List<SupportingFile> supportingFiles = this.akkaScalaClientCodegen.supportingFiles();

        assertThat(supportingFiles.contains(new SupportingFile("build.sbt.mustache", "", "build.sbt")), is(equalTo(true)));
        assertThat(supportingFiles.contains(new SupportingFile("pom.mustache", "", "pom.xml")), is(equalTo(false)));
    }

    @Test
    public void shouldGeneratePomXML() {
        this.akkaScalaClientCodegen.additionalProperties().put(CodegenConstants.BUILD_TOOL, "MAVEN");
        this.akkaScalaClientCodegen.processOpts();
        List<SupportingFile> supportingFiles = this.akkaScalaClientCodegen.supportingFiles();

        assertThat(supportingFiles.contains(new SupportingFile("pom.mustache", "", "pom.xml")), is(equalTo(true)));
        assertThat(supportingFiles.contains(new SupportingFile("build.sbt.mustache", "", "build.sbt")), is(equalTo(false)));
    }

    @Test
    public void shouldGeneratePomXMLWhenBuildToolNameIsLowercase() {
        this.akkaScalaClientCodegen.additionalProperties().put(CodegenConstants.BUILD_TOOL, "maven");
        this.akkaScalaClientCodegen.processOpts();
        List<SupportingFile> supportingFiles = this.akkaScalaClientCodegen.supportingFiles();

        assertThat(supportingFiles.contains(new SupportingFile("pom.mustache", "", "pom.xml")), is(equalTo(true)));
        assertThat(supportingFiles.contains(new SupportingFile("build.sbt.mustache", "", "build.sbt")), is(equalTo(false)));
    }
}
