package io.swagger.handler;

import com.networknt.server.Server;
import org.junit.rules.ExternalResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestServer extends ExternalResource {
    static final Logger logger = LoggerFactory.getLogger(TestServer.class);

    private static volatile int refCount = 0;
    private static Server server;

    private static final TestServer instance  = new TestServer();

    public static TestServer getInstance () {
        return instance;
    }

    private TestServer() {

    }

    protected void before() {
        try {
            if (refCount == 0) {
                server.start();
            }
        }
        finally {
            refCount++;
        }
    }

    protected void after() {
        refCount--;
        if (refCount == 0) {
            server.stop();
        }
    }

}
