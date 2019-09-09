# swagger_petstore

> Test module for "html" language option in `swagger-codegen`, using [BackstopJS](https://github.com/garris/BackstopJS)

## Setup

To install dependencies from npm:

```bash
npm install
```

## Run test

To test with your changes to the generator, first run `bin/html-petstore.sh` (or the Windows equiv; see `CONTRIBUTING.MD`). to generate a fresh html document.

Then, to run the BackstopJS test:

```bash
npm test
```

This will run the tests using [Puppeteer](https://pptr.dev/), headlessly. It will launch a report in your browser afterwards showing the diffs for any failures.

## Update reference

If you have made an intentional change and want to update the reference screenshot accordingly, first run the test so you have a failure, then immediately run:

```bash
npx backstop approve
```

This will take the screenshot from the failed test as the new reference - you'll need to commit this changed file.

## In CI

When run via Maven, the node and npm binaries will be installed locally and discarded afterwards, and BackstopJS will not try to launch a browser-based report afterwards, but will instead generate a JUnit-style report where it can be picked up by surefire.