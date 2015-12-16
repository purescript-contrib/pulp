// module Pulp.Rebuild
"use strict";

exports.hashAny = function(thing) {
  var hash = require("crypto").createHash("md5");
  hash.update(JSON.stringify(thing));
  return hash.digest("hex");
};
