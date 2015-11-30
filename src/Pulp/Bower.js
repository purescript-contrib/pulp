// module Pulp.Bower
"use strict";

exports.requireResolve = function requireResolve(path) {
  return function() {
    return require.resolve(path);
  };
};
