#!/bin/bash

npm install ../../builds/default

# Build Node.js
tsc

# Build Webpack
webpack

# Build browserify
browserify test -p [ tsify ] > ./dist/test.browserify-bundle.js
