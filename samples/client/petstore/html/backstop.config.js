const config = {
    id: "swagger_petstore",
    viewports: [
        {
            label: "standard",
            width: 1024,
            height: 768
        }
    ],
    onBeforeScript: "puppet/onBefore.js",
    onReadyScript: "puppet/onReady.js",
    scenarios: [
        {
            label: "Petstore API",
            url: "/index.html",
            delay: 0,
            selectorExpansion: true,
            misMatchThreshold: 2.5,
            requireSameDimensions: true
        }
    ],
    paths: {
        bitmaps_reference: "backstop_data/bitmaps_reference",
        bitmaps_test: "backstop_data/bitmaps_test",
        engine_scripts: "backstop_data/engine_scripts",
        html_report: "backstop_data/html_report",
        ci_report: "target/surefire-reports"
    },
    report: ["browser"],
    engine: "puppeteer",
    engineOptions: {
        args: ["--no-sandbox"]
    },
    asyncCaptureLimit: 5,
    asyncCompareLimit: 50,
    debug: false,
    debugWindow: false
};

if (process.env.CI) {
    Object.assign(config, {
        report: ["CI"],
        ci: {
            format: "junit",
            testReportFileName: "TEST-backstop",
            testSuiteName: "backstop"
        }
    });
}

module.exports = config;
