/**
 * Copyright 2018 coldrye.eu, Carsten Klein.
 * https://github.com/coldrye-java/test-assertions
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.swagger;

import io.swagger.DiffMatchPatch.Operation;
import org.apache.tomcat.util.http.fileupload.util.Streams;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;

/*
 * This is work in progress and will be made available on github soon, so that we can reuse this for the standard Java
 * code generation target as well.
 */
public class Assertions {

  public static void assertFileEquals(String expected, String actual) throws IOException {

    DiffMatchPatch dmp = new DiffMatchPatch();

    LinkedList<DiffMatchPatch.Diff> diffs = dmp.diff_main(readFile(expected), readFile(actual), true);
    if (diffs.size() == 1 && Operation.EQUAL.equals(diffs.get(0).operation)) {

      return;
    }
    if (!diffs.isEmpty()) {

      String msg = "The contents of the expectation file \""
        + expected
        + "\" and the actual file \""
        + actual
        + "\" differ.\n"
        + diffPrettyText(diffs);

      throw new AssertionError(msg);
    }
  }

  private static String readFile(String path) throws IOException {

    InputStream is = Assertions.class.getClassLoader().getResourceAsStream(path);
    if (is != null) {

      return Streams.asString(is);
    }

    return new String(Files.readAllBytes(Paths.get(path)));
  }

  private static String diffPrettyText(LinkedList<DiffMatchPatch.Diff> diffs) {

    StringBuilder builder = new StringBuilder();

    for(DiffMatchPatch.Diff diff : diffs) {

      switch (diff.operation) {
        case DELETE:
          builder.append("<<<<");
          builder.append(diff.text);
          builder.append("<<<<");
          break;
        case INSERT:
          builder.append(">>>>");
          builder.append(diff.text);
          builder.append(">>>>");
          break;
        default:
        case EQUAL:
          builder.append(diff.text);
      }
    }

    return builder.toString();
  }

  private Assertions() {}
}
