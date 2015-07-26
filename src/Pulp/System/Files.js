// module Pulp.System.Files

"use strict";

exports["exists'"] = function exists$prime(path, callback) {
  require("fs").exists(path, function(r) { callback(null, r); });
};
