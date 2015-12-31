// module Pulp.System.TreeKill
"use strict";

exports.treeKill = function treeKill(pid) {
  return function(signal) {
    return function() {
      require("tree-kill")(pid, signal);
    };
  };
};
