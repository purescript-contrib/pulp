// module Pulp.Files

"use strict";

exports.globImpl = function glob$prime(pat, callback) {
  require("glob")(pat, {}, callback);
};
