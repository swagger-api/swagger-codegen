FROM openjdk:8-jdk-alpine

RUN set -x \
 && apk update && apk upgrade \
 && apk add --no-cache bash ca-certificates maven

ENV GEN_DIR /opt/swagger-codegen
WORKDIR ${GEN_DIR}
VOLUME  ${MAVEN_HOME}/.m2/repository

# Required from a licensing standpoint
COPY ./LICENSE ${GEN_DIR}

# Required to compile swagger-codegen
COPY ./google_checkstyle.xml ${GEN_DIR}

# Modules are copied individually here to allow for caching of docker layers between major.minor versions
# NOTE: swagger-generator is not included here, it is available as swaggerapi/swagger-generator-v3-minimal
COPY ./modules/swagger-codegen-maven-plugin ${GEN_DIR}/modules/swagger-codegen-maven-plugin
COPY ./modules/swagger-codegen-cli ${GEN_DIR}/modules/swagger-codegen-cli
COPY ./modules/swagger-codegen ${GEN_DIR}/modules/swagger-codegen
COPY ./pom.docker.xml ${GEN_DIR}/pom.xml

# Pre-compile swagger-codegen-cli
RUN mvn -am -pl "modules/swagger-codegen-cli" package

# This exists at the end of the file to benefit from cached layers when modifying docker-entrypoint.sh.
COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["help"]
