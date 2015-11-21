var express = require("express");
var bodyParser = require("body-parser");

// Require PureScript modules
// Make sure to run with:
// NODE_PATH=$PWD/output:$NODE_PATH node app.js
var App = require("App");

var searchResponse = require("./data/search.json");

var app = express();

var PORT = process.env.PORT || 3000;

app.use(bodyParser.json());
app.use(express.static('public'));

app.get("/api/search", function(req, res) {
  var businesses = App.getBusinessesFromSearch(searchResponse);
  res.send(businesses);
});

app.get("/", function(req, res) {
  var html = App.renderHomePageToHtml(searchResponse);
  res.send(html);
});

app.listen(PORT, function() {
  console.log("Listening on " + PORT);
});
