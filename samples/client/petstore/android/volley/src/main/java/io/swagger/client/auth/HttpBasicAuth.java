package io.swagger.client.auth;

import android.util.Base64;

import java.util.List;
import java.util.Map;

import io.swagger.client.Pair;

public class HttpBasicAuth implements Authentication {
    private String username;
    private String password;

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public void applyToParams(List<Pair> queryParams, Map<String, String> headerParams) {
        String str = (username == null ? "" : username) + ":" + (password == null ? "" : password);
        headerParams.put("Authorization", "Basic " + Base64.encodeToString(str.getBytes(), Base64.DEFAULT));
    }
}
