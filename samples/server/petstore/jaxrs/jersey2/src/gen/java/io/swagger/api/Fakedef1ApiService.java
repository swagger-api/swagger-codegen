package io.swagger.api;

import io.swagger.api.*;
import io.swagger.model.*;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;

import io.swagger.model.Definition1;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.validation.constraints.*;

public abstract class Fakedef1ApiService {
    public abstract Response definition1(SecurityContext securityContext) throws NotFoundException;
    public abstract Response postJSON(Object body,SecurityContext securityContext) throws NotFoundException;
}
