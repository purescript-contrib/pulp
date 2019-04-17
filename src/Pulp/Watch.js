// module Pulp.Watch

"use strict";

exports.watch = function(pattern) {
  return function(act) {
    return function() {
      var Gaze = require("gaze").Gaze;

      var gaze = new Gaze(pattern);

      gaze.on("all", function(_, path) {
        act(path)();
      });
    };
  };
};
