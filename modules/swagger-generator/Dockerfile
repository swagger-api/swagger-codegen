FROM java:8-jdk

WORKDIR /generator
COPY src/main/docker/entry-point.sh /generator/entry-point.sh
RUN chmod 555 /generator/entry-point.sh
COPY target/lib/jetty-runner* /generator/jetty-runner.jar
COPY target/*.war /generator/swagger-generator.war

ENV GENERATOR_HOST=https://generator.swaggerhub.com/api/swagger.json
RUN apt-get update

EXPOSE 8080
ENTRYPOINT ["/generator/entry-point.sh"]
