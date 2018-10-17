FROM openjdk:8-jre-alpine

WORKDIR /jetty_home
COPY docker/jetty /jetty_home/
COPY target/lib/jetty* /jetty_home/lib/
RUN rm /jetty_home/lib/jetty-runner*
COPY target/lib/javax.servlet-api* /jetty_home/lib/servlet-api-3.1.jar
COPY target/lib/start.jar /jetty_home/
ENV JETTY_HOME /jetty_home
WORKDIR /generator
COPY docker/jetty_base /generator/
COPY docker/ROOT.xml /generator/webapps/ROOT.xml
COPY target/*.war /generator/webapps/ROOT.war
ENV JETTY_BASE /generator
ARG HIDDEN_OPTIONS_DEFAULT_PATH
COPY ${HIDDEN_OPTIONS_DEFAULT_PATH} /generator/resources/
ARG HTTP_PORT
ENV HTTP_PORT ${HTTP_PORT}
ARG JAVA_MEM
ENV JAVA_MEM ${JAVA_MEM}
WORKDIR $JETTY_BASE
CMD ["./start"]

