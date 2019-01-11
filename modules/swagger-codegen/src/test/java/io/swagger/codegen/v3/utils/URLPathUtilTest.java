package io.swagger.codegen.v3.utils;

import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.net.URL;
import java.util.List;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

public class URLPathUtilTest {

    @Mock
    private OpenAPI openAPI;
    @Mock
    private CodegenConfig config;
    @Mock
    private List<Server> servers;
    @Mock
    private Server server;

    @BeforeMethod
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        when(openAPI.getServers()).thenReturn(servers);
        when(servers.isEmpty()).thenReturn(false);
        when(servers.get(0)).thenReturn(server);
    }

    @Test
    public void testNoServers() {
        when(servers.isEmpty()).thenReturn(true);
        when(server.getUrl()).thenReturn(null);

        URL url = URLPathUtil.getServerURL(openAPI, config);
        Assert.assertEquals(url.getHost(), "localhost");
        Assert.assertEquals(url.getPort(), 8080);

        verify(openAPI).getServers();
        verify(servers).isEmpty();

        verifyZeroInteractions(server);
        verifyZeroInteractions(config);
    }

    @Test
    public void testNoServerURL() {
        when(server.getUrl()).thenReturn(null);

        URL url = URLPathUtil.getServerURL(openAPI, config);
        Assert.assertEquals(url.getHost(), "localhost");
        Assert.assertEquals(url.getPort(), 8080);

        verify(openAPI).getServers();
        verify(servers).isEmpty();
        verifyZeroInteractions(config);
    }

    @Test
    public void testServerURL() {
        when(server.getUrl()).thenReturn("http://some.place:9999/v2");

        URL url = URLPathUtil.getServerURL(openAPI, config);
        Assert.assertEquals(url.getHost(), "some.place");
        Assert.assertEquals(url.getPort(), 9999);

        verify(openAPI).getServers();
        verify(servers).isEmpty();
        verifyZeroInteractions(config);
    }

    @Test (description = "verify a relative url when input url is not present.")
    public void testRelativeServerURLv1() {
        when(server.getUrl()).thenReturn("/v2");
        when(config.getInputURL()).thenReturn(null);

        URL url = URLPathUtil.getServerURL(openAPI, config);
        Assert.assertEquals(url.getHost(), "localhost");
        Assert.assertEquals(url.getPort(), 8080);

        verify(openAPI).getServers();
        verify(servers).isEmpty();
        verify(config).getInputURL();
    }

    @Test (description = "verify a relative url when input url is not remote.")
    public void testRelativeServerURLv2() {
        when(server.getUrl()).thenReturn("/v2");
        when(config.getInputURL()).thenReturn("/some/place/locally/file.json");

        URL url = URLPathUtil.getServerURL(openAPI, config);
        Assert.assertEquals(url.getHost(), "localhost");
        Assert.assertEquals(url.getPort(), 8080);

        verify(openAPI).getServers();
        verify(servers).isEmpty();
        verify(config).getInputURL();
    }

    @Test (description = "verify a relative url when input url is remote.")
    public void testRelativeServerURLv3() {
        when(server.getUrl()).thenReturn("/v2");
        when(config.getInputURL()).thenReturn("http://some.place.com.co:80/remote/file.json");

        URL url = URLPathUtil.getServerURL(openAPI, config);
        Assert.assertEquals(url.getHost(), "some.place.com.co");
        Assert.assertEquals(url.getPort(), 80);

        verify(openAPI).getServers();
        verify(servers).isEmpty();
        verify(config).getInputURL();
    }
}
