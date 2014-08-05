var child = require("child_process");
var glob = require("glob");

function exec(cmd, quiet, args, env, callback) {
  child.spawn(cmd, args, {
    stdio: [process.stdin, quiet ? "ignore" : process.stdout, process.stderr],
    env: env
  }).on("exit", function(code, signal) {
    if (code > 0) {
      callback(new Error("Subcommand terminated with error code " + code), code);
    } else {
      callback(null, 0);
    }
  }).on("error", function(err) {
    if (err.code === "ENOENT") {
      callback(new Error("Node executable not found."));
    }
  });
}

function invokeCompiler(cmd, quiet, match, args, env, callback) {
  glob(match, {}, function(err, files) {
    if (err) {
      callback(err);
    } else {
      exec(cmd, quiet, args.concat(files), env, callback);
    }
  });
}

module.exports.exec = exec;
module.exports.invokeCompiler = invokeCompiler;
module.exports.psc = invokeCompiler.bind(null, "psc", true);
module.exports.pscMake = invokeCompiler.bind(null, "psc-make", true);
