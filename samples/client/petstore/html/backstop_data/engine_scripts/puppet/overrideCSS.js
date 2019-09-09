const BACKSTOP_TEST_CSS_OVERRIDE = `html {background-image: none;}`;

module.exports = function (page, scenario) {
  // inject arbitrary css to override styles
  page.evaluate(`window._styleData = '${BACKSTOP_TEST_CSS_OVERRIDE}'`);
  page.evaluate(() => {
    var style = document.createElement('style');
    style.type = 'text/css';
    var styleNode = document.createTextNode(window._styleData);
    style.appendChild(styleNode);
    document.head.appendChild(style);
  });

  console.log('BACKSTOP_TEST_CSS_OVERRIDE injected for: ' + scenario.label);
};
