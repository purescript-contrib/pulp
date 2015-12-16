// module Pulp.Watch

"use strict";

exports.watch = function watch(directories) {
  return function(act) {
    return function() {
      var Watchpack = require("watchpack");
      var watchpack = new Watchpack();
      watchpack.watch([], directories, Date.now() - 10000);
      watchpack.on("change", function(path) {
        act(path)();
      });
    };
  };
};

exports.minimatch = function(str) {
  return function(glob) {
    return require("minimatch")(str, glob);
  };
};
