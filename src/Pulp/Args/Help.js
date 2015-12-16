// module Pulp.Args.Help
"use strict";

exports.pad = function pad(n) {
  return new Array(n + 1).join(" ");
};

exports.wrap = function wrap(s) {
  return function(indent) {
    return function() {
      var cols = process.stdout.columns;
      return cols ? require("wordwrap")(indent, cols)(s).slice(indent) : s;
    };
  };
};
