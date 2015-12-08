var fs = require("fs");
var path = require("path");

var IS_PROD = process.env.NODE_ENV === "production";

function prodPureScriptExternals() {
  var moduleNames = fs.readdirSync(path.join(__dirname, "output"));
  return moduleNames.reduce(function(result, moduleName) {
    result[moduleName] = "PS[\"" + moduleName + "\"]";
    return result;
  }, {});
}

module.exports = {
  entry: "./src/js/client.js",
  devtool: IS_PROD ? null : "source-maps",
  output: {
    path: path.join(__dirname, "dist"),
    filename: IS_PROD ? "javascript-bundle.js" : "bundle.js"
  },
  resolve: {
    modulesDirectories: [
      "node_modules",
      "output"
    ]
  },
  externals: IS_PROD ? prodPureScriptExternals() : null
};
