// module Pulp.System.Process

"use strict";

exports.stdin = process.stdin;
exports.stdout = process.stdout;
exports.stderr = process.stderr;

exports["argv'"] = process.argv;

exports.exit = function exit(code) {
  return function() {
    process.exit(code);
  };
};

exports.getEnvironment = function getEnvironment() {
  return process.env
}

exports.getPlatform = function getPlatform() {
  return process.platform;
};
