DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Enables the Server thread pool.

[xml]
etc/jetty-threadpool.xml

[ini-template]

### Server Thread Pool Configuration
## Minimum Number of Threads
#jetty.threadPool.minThreads=10

## Maximum Number of Threads
#jetty.threadPool.maxThreads=200

## Number of reserved threads (-1 for heuristic)
# jetty.threadPool.reservedThreads=-1

## Thread Idle Timeout (in milliseconds)
#jetty.threadPool.idleTimeout=60000

## Whether to Output a Detailed Dump
#jetty.threadPool.detailedDump=false
