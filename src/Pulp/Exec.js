
// module Pulp.Exec

"use strict";

exports["concatStream'"] = function concatStream$prime(stream, callback) {
  var concat = require("concat-stream");

  var onSuccess = function(buf) {
    callback(null, buf.toString("utf-8"));
  };

  var onError = function(err) {
    callback(err, null);
  }

  stream.on('error', onError);
  stream.pipe(concat(onSuccess));
}
