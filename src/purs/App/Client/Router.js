"use strict";

// module App.Client.Router

// Inspired by https://github.com/spicydonuts/lucid-router

var history = window.history;

var hasHistoryApi = (
  history !== undefined &&
  typeof history.pushState === "function"
);

var urlChangeListeners = [];

var started = false;

exports.navigate = function(url) {
  return function() {
    if (hasHistoryApi) {
      history.pushState(null, "", url);
      onUrlChange(url);
    } else {
      window.location = url;
    }
    return {};
  };
};

function onUrlChange(url) {
  urlChangeListeners.forEach(function(cb) {
    cb(url);
  });
}

exports.addUrlChangeListener = function(handler) {
  return function() {
    urlChangeListeners.push(function(url) {
      handler(url)();
    });
    return {};
  };
};

exports.start = function() {
  if (!started && hasHistoryApi) {
    window.addEventListener("popstate", function(e) {
      var url = getWindowUrl();
      onUrlChange(url);
    });
    started = true;
  }
  return {};
}

function getWindowUrl() {
  var location = window.location;
  return location.pathname + location.search;
}
