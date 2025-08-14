#!/bin/bash

if [-z $PORT]; then
  PORT=8080
fi

java -jar /generator/jetty-runner.jar --port $PORT /generator/swagger-generator.war