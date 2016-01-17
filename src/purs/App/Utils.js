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
