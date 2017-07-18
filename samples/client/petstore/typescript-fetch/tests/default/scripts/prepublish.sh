#!/bin/bash

npm install ../../builds/with-npm-version
 
# Build Node.js
npm run build

# Build Webpack
npm run webpack

# Build browserify
npm run browserify
