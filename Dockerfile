FROM maven:3-eclipse-temurin-11-alpine

RUN set -x && \
    apk add --no-cache bash

ARG GEN_DIR /opt/swagger-codegen
ENV GEN_DIR ${GEN_DIR}
WORKDIR ${GEN_DIR}

VOLUME ${MAVEN_HOME}/.m2/repository

# Required from a licensing standpoint
COPY ./LICENSE ./

# Required to compile swagger-codegen
COPY ./google_checkstyle.xml ./

# Modules are copied individually here to allow for caching of docker layers between major.minor versions
# NOTE: swagger-generator is not included here, it is available as swaggerapi/swagger-generator
COPY ./modules/swagger-codegen-maven-plugin ./modules/swagger-codegen-maven-plugin
COPY ./modules/swagger-codegen-cli ./modules/swagger-codegen-cli
COPY ./modules/swagger-codegen ./modules/swagger-codegen
COPY ./modules/swagger-generator ./modules/swagger-generator
COPY ./pom.xml ./

# Pre-compile swagger-codegen-cli
RUN mvn -am -pl "modules/swagger-codegen-cli" package

# This exists at the end of the file to benefit from cached layers when modifying docker-entrypoint.sh.
COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
