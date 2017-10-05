FROM openjdk:8-jdk-alpine

ENV GEN_DIR /opt/swagger-codegen
ENV MAVEN_HOME=/usr/share/maven
VOLUME  ${MAVEN_HOME}/.m2/repository

RUN set -x \
    && apk add --no-cache ca-certificates openssl bash curl jq nodejs nodejs-npm \
    && update-ca-certificates \
    && cd /tmp \
    && wget https://archive.apache.org/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.tar.gz \
    && wget https://archive.apache.org/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.tar.gz.sha1 \
    && echo -e "$(cat apache-maven-3.3.9-bin.tar.gz.sha1)  apache-maven-3.3.9-bin.tar.gz" | sha1sum -c - \
    && tar zxf apache-maven-3.3.9-bin.tar.gz \
    && rm -rf apache-maven-3.3.9-bin.tar.gz \
    && rm -rf *.sha1 \
    && mv ./apache-maven-3.3.9 /usr/share/maven \
    && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn \
    && mkdir /opt

ADD . ${GEN_DIR}

WORKDIR ${GEN_DIR}

RUN mvn -am -pl "modules/swagger-codegen-cli" package

COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["build"]
