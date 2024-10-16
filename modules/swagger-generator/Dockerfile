FROM openjdk:8-jre-alpine

WORKDIR /generator

COPY entrypoint.sh entrypoint.sh
COPY target/lib/jetty-runner* /generator/jetty-runner.jar
COPY target/*.war /generator/swagger-generator.war

ENV GENERATOR_HOST=https://generator.swaggerhub.com/api/swagger.json

EXPOSE 8080

ENTRYPOINT entrypoint.sh

