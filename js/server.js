var express = require("express");
var bodyParser = require("body-parser");

var YelpClient = require("./server/yelp-client");

// PureScript modules
// Make sure to run with:
// NODE_PATH=$PWD/output:$NODE_PATH node app.js
var App = require("App");

var DEV_SEARCH_RESPONSE = require("../data/search.json");
var DEV_SEARCH_ERROR_RESPONSE = require("../data/search-error.json");

var app = express();

var PORT = process.env.PORT || 3000;

app.use(bodyParser.json());
app.use(express.static('public'));

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
  YelpClient.search(req.query, function(err, data) {
    if (err) {
      return handleYelpError(res, err);
    }

    res.send(data);
  });
});

app.get("/", function(req, res) {
  // Uncomment one of these for development
  // return res.send(App.renderErrorPageToHtml(DEV_SEARCH_ERROR_RESPONSE));
  // return res.send(App.renderHomePageToHtml(DEV_SEARCH_RESPONSE));

  YelpClient.search(req.query, function(err, data) {
    var html = "";
    if (err) {
      html = App.renderErrorPageToHtml({error: err});
    } else {
      html = App.renderHomePageToHtml(data);
    }

    res.send(html);
  });

});

app.listen(PORT, function() {
  console.log("Listening on " + PORT);
});