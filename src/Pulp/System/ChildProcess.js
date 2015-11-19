
// module Pulp.System.ChildProcess

exports["spawn'"] = function spawn$prime(cmd, args, env, stdio, callback) {
  var opts = { env: env, stdio: stdio };
  require("child_process").spawn(cmd, args, opts, callback);
}

exports["wait'"] = function wait$prime(child, callback) {
  child.on("exit", callback);
}
