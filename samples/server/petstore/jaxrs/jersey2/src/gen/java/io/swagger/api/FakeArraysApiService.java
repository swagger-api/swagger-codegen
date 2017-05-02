package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;

import java.util.List;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;

public abstract class FakeArraysApiService {
    public abstract Response postJSON( @NotNull Long items,SecurityContext securityContext) throws NotFoundException;
    public abstract Response postJSON_1(List<Long> items,SecurityContext securityContext) throws NotFoundException;
}
