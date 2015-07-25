// module Pulp.System.Ansi

"use strict";

exports.ansi = function ansi(stream) {
  return require("ansi")(stream);
}

exports.castToNodeStream = function castToNodeStream(stream) {
  return stream;
}

exports.col$prime = function col$prime(stream, c, callback) {
  stream[c](); callback();
}

exports.bgCol$prime = function bgCol$prime(stream, c, callback) {
  stream.bg[c](); callback();
}

exports.bold$prime = function bold$prime(stream, callback) {
  stream.bold(); callback();
}

exports.underline$prime = function underline$prime(stream, callback) {
  stream.underline(); callback();
}

exports.reset$prime = function reset$prime(stream, callback) {
  stream.reset(); callback();
}
