(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD.
    define(factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory();
  } else {
    // Browser globals (root is window)
    root.assertEquals = factory();
  }
}(this, function() {
  'use strict';

  var assertEquals = function(expected, actual, ptr) {
    if (!ptr)
      ptr = "";
    if (actual === expected)
      return;
    if (expected instanceof Date || actual instanceof Date) {
      expected = toISODateString(expected);
      actual = toISODateString(actual);
      if (actual !== expected)
        fail(expected, actual, ptr, "date value incorrect;");
    }
    if (!expected || !actual || typeof expected != 'object' && typeof actual != 'object') {
      if (typeof actual != typeof expected)
        fail(typeof expected, typeof actual, ptr, "value type incorrect;");
      if (actual != expected)
        fail(expected, actual, ptr, "value incorrect;");
    }
    return checkObject(expected, actual, ptr);
  }

  function toISODateString(value) {
    if (value instanceof Date) {
      // JavaScript's ISO string contains a milliseconds component that must be stripped out.
      value = value.toISOString().replace('.000', '');
    }
    return value;
  }

  function checkObject(expected, actual, ptr) {
    if (undefOrNull(expected) || undefOrNull(actual))
      fail(expected, actual, ptr, "missing value;");
    if (typeof expected !== typeof actual)
      fail(typeof expected, typeof actual, ptr, "wrong type;");
    if (expected.prototype !== actual.prototype)
      fail(expected.prototype, actual.prototype, ptr, "wrong prototype;");
    try {
      var expectedKeys = Object.keys(expected);
      var actualKeys = Object.keys(actual);
    } catch (e) {
      fail(expectedKeys, actualKeys, ptr, "wrong keys;");
    }
    if (actualKeys.length != expectedKeys.length)
      fail(expectedKeys.length, actualKeys.length, ptr, "key count incorrect;");
    expectedKeys.sort();
    actualKeys.sort();
    for (var i = 0; i < expectedKeys.length; i++) {
      if (actualKeys[i] != expectedKeys[i])
        fail(expectedKeys, actualKeys, ptr, "wrong keys;");
    }
    for (i = 0; i < expectedKeys.length; i++) {
      var key = expectedKeys[i];
      assertEquals(expected[key], actual[key], ptr + '/' + key);
    }
  }

  function undefOrNull(v) {
    return v === undefined || v === null;
  }

  function fail(expected, actual, ptr, msg) {
    var text = ptr + ' ' + msg + " expected: " + expected + ", actual: " + actual;
    console.log(text);
    throw new Error(text);
  }

  return assertEquals;
}));
