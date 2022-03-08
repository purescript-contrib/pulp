// module Pulp.System.Files

"use strict";

exports.isEEXIST = function isEEXIST(err) {
  return err && err.code === 'EEXIST';
};

var temp = require('temp').track();
exports.openTempImpl = function openTemp$prime(opts, callback) {
  temp.open(opts, callback);
};


exports.tempDirImpl = function tempDir$prime(opts, callback) {
  temp.mkdir(opts, callback);
};

exports.createWriteStream = function createWriteStream(path) {
  return function() {
    return require('fs').createWriteStream(path);
  };
};

exports.isENOENT = function isENOENT(error) {
  return error.code === "ENOENT";
};

exports.touchImpl = function touch$prime(path, callback) {
  require("touch")(path, callback);
};
