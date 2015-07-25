// module Pulp.System.FFI

"use strict";

exports.runNode$prime = function runNode$prime(error, success, fn) {
  return function() {
    fn(function(err, val) {
      if (err) { error(err)(); } else { success(val)(); }
    });
  };
}
