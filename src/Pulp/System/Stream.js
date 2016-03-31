// module Pulp.System.Stream
"use strict";

exports["concatStreamToBuffer'"] = function concatStream$prime(stream, callback) {
  var concat = require("concat-stream");

  var onSuccess = function(buf) {
    callback(null, buf);
  };

  var onError = function(err) {
    callback(err, null);
  };

  stream.on('error', onError);
  stream.pipe(concat(onSuccess));
};

exports.createGzip = require("zlib").createGzip;
