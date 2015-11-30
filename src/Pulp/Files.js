// module Pulp.Files

"use strict";

exports["glob'"] = function glob$prime(pat, callback) {
  require("glob")(pat, {}, callback);
};
