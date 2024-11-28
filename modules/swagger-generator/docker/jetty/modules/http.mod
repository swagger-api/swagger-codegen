DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Enables a HTTP connector on the server.
By default HTTP/1 is support, but HTTP2C can
be added to the connector with the http2c module.

[tags]
connector
http

[depend]
server

[xml]
etc/jetty-http.xml

[ini-template]
### HTTP Connector Configuration

## Connector host/address to bind to
# jetty.http.host=0.0.0.0

## Connector port to listen on
# jetty.http.port=8080

## Connector idle timeout in milliseconds
# jetty.http.idleTimeout=30000

## Connector socket linger time in seconds (-1 to disable)
# jetty.http.soLingerTime=-1

## Number of acceptors (-1 picks default based on number of cores)
# jetty.http.acceptors=-1

## Number of selectors (-1 picks default based on number of cores)
# jetty.http.selectors=-1

## ServerSocketChannel backlog (0 picks platform default)
# jetty.http.acceptorQueueSize=0

## Thread priority delta to give to acceptor threads
# jetty.http.acceptorPriorityDelta=0

## Connect Timeout in milliseconds
# jetty.http.connectTimeout=15000

## HTTP Compliance: RFC7230, RFC7230_LEGACY, RFC2616, RFC2616_LEGACY, LEGACY or CUSTOMn
# jetty.http.compliance=RFC7230_LEGACY
