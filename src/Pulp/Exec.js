
// module Pulp.Exec

"use strict";

exports.isENOENT = function isENOENT(error) {
  return error.code === "ENOENT";
};

exports["concatStream'"] = function exec$prime(stream, callback) {
  stream.pipe(require("concat-stream")(callback));
}
