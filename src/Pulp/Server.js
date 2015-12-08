// module Pulp.Server
"use strict";

exports.__dirname = __dirname;

exports.webpackOptions = function webpackOptions(args) {
  return function() {
    return {
      noInfo: args.noInfo,
      quiet: args.quiet,
      stats: {
        cached: false,
        cachedAssets: false,
        colors: args.colors
      }
    };
  };
};

exports.makeDevServer = function makeDevServer(config) {
  return function(options) {
    return function() {
      var webpack = require("webpack");
      var Server = require("webpack-dev-server");
      return new Server(webpack(config), options);
    };
  };
};

exports["listen'"] = function listen$prime(server, host, port, callback) {
  server.listen(port, host, callback);
};
