package io.swagger.codegen.v3;

import java.io.File;
import java.util.List;
import java.util.Map;

public interface Generator {
    Generator opts(ClientOptInput opts);

    List<File> generate();
    Map<String, Object> generateBundle();

}