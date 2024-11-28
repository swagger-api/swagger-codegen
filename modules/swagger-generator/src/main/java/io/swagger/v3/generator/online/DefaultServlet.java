package io.swagger.v3.generator.online;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import org.apache.commons.io.IOUtils;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import java.io.IOException;

public class DefaultServlet extends HttpServlet {

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String yaml = IOUtils.toString(getClass().getResourceAsStream("/openapi.yaml"));
        response.setContentType(MediaType.APPLICATION_JSON);
        response.setStatus(HttpServletResponse.SC_OK);
        OpenAPI openAPI = Yaml.mapper().readValue(yaml, OpenAPI.class);
        response.getWriter().append(Json.pretty(openAPI));
    }
}
