#!/bin/bash
java -jar /generator/jetty-runner.jar --port ${HTTP_PORT-8080} /generator/swagger-generator.war
