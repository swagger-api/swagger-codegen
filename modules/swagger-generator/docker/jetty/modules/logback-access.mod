DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Enables logback request log.

[tags]
requestlog
logging
logback

[depend]
server
logback-impl
resources

[provide]
requestlog

[xml]
etc/jetty-logback-access.xml

[files]
logs/
basehome:modules/logback-access/jetty-logback-access.xml|etc/jetty-logback-access.xml
basehome:modules/logback-access/logback-access.xml|resources/logback-access.xml
maven://ch.qos.logback/logback-access/${logback.version}|lib/logback/logback-access-${logback.version}.jar

[lib]
lib/logback/logback-access-${logback.version}.jar

