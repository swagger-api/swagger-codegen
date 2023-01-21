FROM ubuntu:22.04
ENV disableOas31Resolve=False
ENV JAVA_HOME=/opt/java/openjdk
COPY --from=eclipse-temurin:8u322-b06-jre $JAVA_HOME $JAVA_HOME
ENV PATH="${JAVA_HOME}/bin:${PATH}"

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
COPY grantall.policy /generator/grantall.policy
ENV JETTY_BASE /generator
ARG HIDDEN_OPTIONS_DEFAULT_PATH
COPY ${HIDDEN_OPTIONS_DEFAULT_PATH} /generator/resources/
ARG HTTP_PORT
ENV HTTP_PORT ${HTTP_PORT}
ARG JAVA_MEM
ENV JAVA_MEM ${JAVA_MEM}
WORKDIR $JETTY_BASE

RUN addgroup --gid 2000 codegengroup
RUN adduser --home "/generator" --ingroup codegengroup --disabled-password --gecos '' --shell "/bin/bash" --uid "1000" codegenuser
RUN chown -R codegenuser /generator
RUN chown -R codegenuser /jetty_home

USER codegenuser
CMD ["./start"]

