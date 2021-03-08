FROM jboss/wildfly:21.0.2.Final

ADD target/swagger-jaxrs-resteasy-server-1.0.0.war /opt/jboss/wildfly/standalone/deployments/

EXPOSE 8080 9990 8009

CMD ["/opt/jboss/wildfly/bin/standalone.sh", "-b", "0.0.0.0", "-bmanagement", "0.0.0.0", "-c", "standalone.xml"]
