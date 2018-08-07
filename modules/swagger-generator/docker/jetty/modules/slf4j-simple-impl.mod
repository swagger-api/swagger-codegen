DO NOT EDIT - See: https://www.eclipse.org/jetty/documentation/current/startup-modules.html

[description]
Provides SLF4J simple logging implementation.
To receive jetty logs enable the jetty-slf4j module.

[tags]
logging
slf4j
internal

[depends]
slf4j-api
resources

[provides]
slf4j-impl

[files]
maven://org.slf4j/slf4j-simple/${slf4j.version}|lib/slf4j/slf4j-simple-${slf4j.version}.jar
basehome:modules/slf4j-simple-impl

[lib]
lib/slf4j/slf4j-simple-${slf4j.version}.jar
