// module Pulp.System.Process

"use strict";

exports.stdin = process.stdin;
exports.stdout = process.stdout;
exports.stderr = process.stderr;

exports.argv$prime = process.argv;

exports.exit = function exit(code) {
  return function() {
    process.exit(code);
  };
};
