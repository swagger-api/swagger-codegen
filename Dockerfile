FROM openjdk:8-jdk-alpine

ENV GEN_DIR /opt/swagger-codegen
ENV MAVEN_HOME=/usr/share/maven
VOLUME  ${MAVEN_HOME}/.m2/repository

RUN set -x \
    && apk add --no-cache bash curl jq nodejs-npm maven \
    && mkdir /opt

ADD . ${GEN_DIR}

WORKDIR ${GEN_DIR}

RUN mvn -am -pl "modules/swagger-codegen-cli" package

COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["build"]
