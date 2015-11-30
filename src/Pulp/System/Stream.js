// module Pulp.System.Stream

"use strict";

exports.writeToNodeStream = function writeToNodeStream(stream, data, callback) {
  stream.write(data, callback);
};
