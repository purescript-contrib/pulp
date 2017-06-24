"use strict";

exports["new"] = function(path) {
  return function() {
    var s = require('node-static');
    return new s.Server(path, {
      headers: {
        'cache-control': 'no-cache'
      }
    });
  };
};

exports.serve = function(server) {
  return function(req) {
    return function(res) {
      return function() {
        server.serve(req, res);
      };
    };
  };
};

exports.serveFile = function(server) {
  return function(file) {
    return function(statusCode) {
      return function(req) {
        return function(res) {
          return function() {
            server.serveFile(file, statusCode, {}, req, res);
          };
        };
      };
    };
  };
};
