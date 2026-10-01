#!/bin/bash

CUR=$(pwd)

export SC_VERSION=`mvn -q -Dexec.executable="echo" -Dexec.args='${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}' --non-recursive build-helper:parse-version org.codehaus.mojo:exec-maven-plugin:1.3.1:exec`
export SC_NEXT_VERSION=`mvn -q -Dexec.executable="echo" -Dexec.args='${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.nextIncrementalVersion}' --non-recursive build-helper:parse-version org.codehaus.mojo:exec-maven-plugin:1.3.1:exec`
SC_QUALIFIER=`mvn -q -Dexec.executable="echo" -Dexec.args='${parsedVersion.qualifier}' --non-recursive build-helper:parse-version org.codehaus.mojo:exec-maven-plugin:1.3.1:exec`
#SC_LAST_RELEASE=`mvn -q -Dexec.executable="echo" -Dexec.args='${releasedVersion.version}' --non-recursive org.codehaus.mojo:build-helper-maven-plugin:3.2.0:released-version org.codehaus.mojo:exec-maven-plugin:1.3.1:exec`
SC_LAST_RELEASE=`python $CUR/CI/lastRelease.py`



SC_RELEASE_TITLE="Swagger Codegen $SC_VERSION released!"
SC_RELEASE_TAG="v$SC_VERSION"


#####################
### draft release Notes with next release after last release, with tag
#####################
python $CUR/CI/releaseNotes.py "$SC_LAST_RELEASE" "$SC_RELEASE_TITLE" "$SC_RELEASE_TAG"

#####################
### update the version to release in maven project with set version
#####################
mvn versions:set -DnewVersion=$SC_VERSION
mvn versions:commit

#####################
### build and test maven ###
#####################
mvn -B -U verify --file pom.xml
