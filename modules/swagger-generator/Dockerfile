FROM openjdk:8-jre-alpine

WORKDIR /generator
RUN mkdir /generator/lib
COPY src/main/resources/inflector.yaml /generator/
COPY src/main/resources/openapi.yaml /generator/
COPY target/lib/jetty-runner* /generator/jetty-runner.jar
COPY target/*.war /generator/swagger-generator.war

EXPOSE 8080

CMD ["java", "-jar", "/generator/jetty-runner.jar", "--classes", "/generator/lib", "/generator/swagger-generator.war"]

