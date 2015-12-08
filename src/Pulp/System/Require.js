// module Pulp.System.Require
"use strict";

exports.requireResolve = function requireResolve(path) {
  return function() {
    return require.resolve(path);
  };
};

exports.unsafeRequire = function unsafeRequire(path) {
  return function() {
    return require(path);
  };
};
