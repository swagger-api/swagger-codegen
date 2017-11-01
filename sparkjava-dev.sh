#!/bin/bash

# REMOVE THIS WHEN DONE WITH SPARKJAVA GENERATOR
 
DIR="samples/server/petstore/java-sparkjava"

CLEAN="rm -rv $DIR/*"

MVN="mvn clean package"

GEN="./bin/java-sparkjava-server-petstore.sh"

TREE="tree $DIR"

$CLEAN && $MVN && $GEN && $TREE

