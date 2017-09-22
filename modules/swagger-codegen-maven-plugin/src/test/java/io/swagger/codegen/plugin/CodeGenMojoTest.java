package io.swagger.codegen.plugin;

/*
 * Copyright 2001-2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */


import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.expect;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.junit.runner.RunWith;
import org.powermock.api.easymock.PowerMock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;

@RunWith(PowerMockRunner.class)
@PrepareForTest(CodegenConfigurator.class)
public class CodeGenMojoTest extends AbstractMojoTestCase {

   /** {@inheritDoc} */
    protected void setUp()
        throws Exception
    {
        // required
        super.setUp();

    }

    /** {@inheritDoc} */
    protected void tearDown()
        throws Exception
    {
        // required
        super.tearDown();

    }

    /**
     * @throws Exception if any
     */
    public void testPomMapping()
            throws Exception
    {

        File pom = getTestFile("src/test/resources/pom.xml");

        assertNotNull(pom);
        assertTrue(pom.exists());

        CodeGenMojo codeGenMojo = (CodeGenMojo) lookupMojo("generate", pom);
        assertNotNull(codeGenMojo);
    }

    /**
     * @throws Exception if any
     */
    @PrepareForTest({CodeGenMojo.class,CodegenConfigurator.class,DefaultGenerator.class})
    public void testGenerate()
        throws Exception
    {

        CodegenConfigurator tc = PowerMock.createPartialMock(CodegenConfigurator.class, "toClientOptInput");
        tc.setSupportFilesMapping(new HashMap<String, String>());
        PowerMock.expectNew(CodegenConfigurator.class).andReturn(tc);
        ClientOptInput clientOptInput = new ClientOptInput();
        CodegenConfig codegenConfig = PowerMock.createMock(CodegenConfig.class);
        clientOptInput.setConfig(codegenConfig);
        expect(codegenConfig.cliOptions()).andReturn(new ArrayList<CliOption>());

        expect(tc.toClientOptInput()).andReturn(clientOptInput);

        DefaultGenerator dg = PowerMock.createPartialMock(DefaultGenerator.class, "opts", "generate");
        PowerMock.expectNew(DefaultGenerator.class).andReturn(dg);
        expect(dg.opts(anyObject(ClientOptInput.class))).andReturn(dg);
        expect(dg.generate()).andReturn(null);

        PowerMock.replayAll();

        File pom = getTestFile("src/test/resources/pom.xml");

        assertNotNull(pom);
        assertTrue(pom.exists());

        CodeGenMojo codeGenMojo = (CodeGenMojo) lookupMojo("generate", pom);
        assertNotNull(codeGenMojo);

        try
        {
            codeGenMojo.execute();
        } catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}
