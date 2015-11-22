var path = require("path");

var IS_PROD = process.env.NODE_ENV === "production";

module.exports = {
  entry: "./js/client.js",
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
  externals: IS_PROD ? {
    "App.Client.Main": "PS[\"App.Client.Main\"]"
  } : null
};
