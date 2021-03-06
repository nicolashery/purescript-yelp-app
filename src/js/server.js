var express = require("express");
var bodyParser = require("body-parser");

var YelpClient = require("./server/yelp-client");

// PureScript modules
// Make sure to run with:
// NODE_PATH=$PWD/output:$NODE_PATH node app.js
var Main = require("App.Server.Main");

var DEV_SEARCH_RESPONSE = require("../../data/search.json");
var DEV_SEARCH_ERROR_RESPONSE = require("../../data/search-error.json");
var DEV_SEARCH_EMPTY_RESPONSE = require("../../data/search-empty.json");

var app = express();

var PORT = process.env.PORT || 3000;

app.use(bodyParser.json());
app.use(express.static("public"));
app.use(express.static("dist"));

function getSearchQuery(req) {
  return {
    term: req.query.term || "",
    location: req.query.location || ""
  };
}

function handleYelpError(res, err) {
  var result = {
    error: {
      name: err.name,
      message: err.message
    }
  };
  res.status(err.status || 500).send(result);
}

app.get("/api/search", function(req, res) {
  var query = getSearchQuery(req);

  // Uncomment one of these for development
  // return setTimeout(function() { res.send(DEV_SEARCH_RESPONSE); }, 1000);
  // return setTimeout(function() { res.status(500).send(DEV_SEARCH_ERROR_RESPONSE); }, 1000);
  // return setTimeout(function() { res.send(DEV_SEARCH_EMPTY_RESPONSE); }, 1000);

  YelpClient.search(query, function(err, results) {
    if (err) {
      return handleYelpError(res, err);
    }

    res.send(results);
  });
});

app.use("/", function(req, res) {
  var query = getSearchQuery(req);

  // Uncomment one of these for development
  // return res.send(Main.renderSearchPageResults(query, DEV_SEARCH_RESPONSE));
  // return res.status(500).send(Main.renderSearchPageError(query, DEV_SEARCH_ERROR_RESPONSE));
  // return res.send(Main.renderSearchPageResults(query, DEV_SEARCH_EMPTY_RESPONSE));
  // return res.send(Main.renderSearchPageWelcome());

  if (!query.location.length) {
    return res.send(Main.renderSearchPageWelcome());
  }

  YelpClient.search(query, function(err, results) {
    var status = 200;
    var html = "";
    if (err) {
      status = err.status || 500;
      html = Main.renderSearchPageError(query, {error: err});
    } else {
      html = Main.renderSearchPageResults(query, results);
    }

    res.status(status).send(html);
  });

});

app.listen(PORT, function() {
  console.log("Listening on " + PORT);
});
