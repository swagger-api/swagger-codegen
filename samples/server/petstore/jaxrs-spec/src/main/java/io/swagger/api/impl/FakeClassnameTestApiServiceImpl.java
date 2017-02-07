package io.swagger.api.impl;

import io.swagger.model.Client;

import io.swagger.api.FakeClassnameTestApi;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;

import java.util.List;

@Path("/fake_classname_test")

@Api(description = "the fake_classname_test API")




public class FakeClassnameTestApiServiceImpl implements FakeClassnameTestApi {

    public Response testClassname(Client body) {
        return Response.ok().entity("magic!").build();
    }
}

