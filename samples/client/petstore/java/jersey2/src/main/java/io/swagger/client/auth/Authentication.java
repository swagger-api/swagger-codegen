package io.swagger.client.auth;

import java.util.List;
import java.util.Map;

import io.swagger.client.Pair;

public interface Authentication {
    /**
     * Apply authentication settings to header and query params.
     */
    void applyToParams(List<Pair> queryParams, Map<String, String> headerParams);
}
