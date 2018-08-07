DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Enables the core Jetty server on the classpath.

[optional]
jvm
ext
resources
logging

[depend]
threadpool

[lib]
lib/servlet-api-3.1.jar
lib/jetty-schemas-3.1.jar
lib/jetty-http-${jetty.version}.jar
lib/jetty-server-${jetty.version}.jar
lib/jetty-xml-${jetty.version}.jar
lib/jetty-util-${jetty.version}.jar
lib/jetty-io-${jetty.version}.jar

[xml]
etc/jetty.xml

[ini-template]
### Common HTTP configuration
## Scheme to use to build URIs for secure redirects
# jetty.httpConfig.secureScheme=https

## Port to use to build URIs for secure redirects
# jetty.httpConfig.securePort=8443

## Response content buffer size (in bytes)
# jetty.httpConfig.outputBufferSize=32768

## Max response content write length that is buffered (in bytes)
# jetty.httpConfig.outputAggregationSize=8192

## Max request headers size (in bytes)
# jetty.httpConfig.requestHeaderSize=8192

## Max response headers size (in bytes)
# jetty.httpConfig.responseHeaderSize=8192

## Whether to send the Server: header
# jetty.httpConfig.sendServerVersion=true

## Whether to send the Date: header
# jetty.httpConfig.sendDateHeader=false

## Max per-connection header cache size (in nodes)
# jetty.httpConfig.headerCacheSize=512

## Whether, for requests with content, delay dispatch until some content has arrived
# jetty.httpConfig.delayDispatchUntilContent=true

## Maximum number of error dispatches to prevent looping
# jetty.httpConfig.maxErrorDispatches=10

## Maximum time to block in total for a blocking IO operation (default -1 is to use idleTimeout on progress)
# jetty.httpConfig.blockingTimeout=-1

## Cookie compliance mode of: RFC2965, RFC6265
# jetty.httpConfig.cookieCompliance=RFC6265

### Server configuration
## Whether ctrl+c on the console gracefully stops the Jetty server
# jetty.server.stopAtShutdown=true

## Timeout in ms to apply when stopping the server gracefully
# jetty.server.stopTimeout=5000

## Dump the state of the Jetty server, components, and webapps after startup
# jetty.server.dumpAfterStart=false

## Dump the state of the Jetty server, components, and webapps before shutdown
# jetty.server.dumpBeforeStop=false
