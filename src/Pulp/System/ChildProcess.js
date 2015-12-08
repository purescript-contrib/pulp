// module Pulp.System.ChildProcess
"use strict";

exports["spawn'"] = function spawn$prime(cmd, args, env, stdio) {
  return function() {
    var opts = { env: env, stdio: stdio };
    return require("child_process").spawn(cmd, args, opts);
  };
};

exports["wait'"] = function wait$prime(child, callback) {
  child.on("exit", function(r) {
    callback(null, r);
  });
  child.on("error", function(err) {
    callback(err);
  });
};

exports.fork = function fork(args) {
  return function() {
    return require("child_process").fork(__filename, args);
  };
};

exports.treeKill = function treeKill(pid) {
  return function(signal) {
    return function() {
      require("tree-kill")(pid, signal);
    };
  };
};
