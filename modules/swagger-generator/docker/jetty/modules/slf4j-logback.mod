DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Provides a SLF4J binding to Logback logging.

[tags]
logging
slf4j
internal

[depends]
slf4j-api
logback-impl
resources

[provides]
slf4j-impl

[files]
maven://ch.qos.logback/logback-classic/${logback.version}|lib/logback/logback-classic-${logback.version}.jar

[lib]
lib/logback/logback-classic-${logback.version}.jar

