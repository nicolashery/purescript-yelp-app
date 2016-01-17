"use strict";

// module App.Utils

exports.encodeURIComponent = encodeURIComponent;

exports.decodeURIComponent = decodeURIComponent;

exports.extractQueryString = function(url) {
  var parts = url.split("?");
  if (parts.length < 2) {
    return "";
  } else {
    return "?" + parts[1];
  }
};

exports.unsafeQuerySelector = function(selector) {
  return function(src) {
    return function() {
      var el = src.querySelector(selector);
      if (!el) {
        throw new Error("Could not find element for selector '" + selector + "'");
      }
      return el;
    };
  };
};
