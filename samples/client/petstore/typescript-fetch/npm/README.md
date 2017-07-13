# @swagger/typescript-fetch-petstore@0.0.1

This generator creates TypeScript/JavaScript client that utilizes [Fetch API](https://fetch.spec.whatwg.org/). The generated Node module can be used in the following environments: 

Environment
* Node.js
* Webpack
* Browserify

### Building

To build an compile the typescript sources to javascript use:
```
npm install
npm run build
```

### publishing

First build the package than run ```npm publish```

### consuming

navigate to the folder of your consuming project and run one of next commando's.

_published:_

```
npm install @swagger/typescript-fetch-petstore@0.0.1 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```
