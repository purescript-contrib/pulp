var files = require("./files");
var child = require("child_process");
var q = require("q");

function exec(cmd, quiet, args, env, callback) {
  var output = q.defer();
  var c = child.spawn(cmd, args, {
    stdio: [process.stdin, quiet ? "pipe" : process.stdout, process.stderr],
    env: env
  }).on("exit", function(code) {
    if (code > 0) {
      if (quiet) {
        output.promise.then(function(buf) {
          process.stderr.write(buf.toString("utf-8"));
          callback(new Error("Subcommand terminated with error code " + code), code);
        });
      } else {
        callback(new Error("Subcommand terminated with error code " + code), code);
      }
    } else {
      if (quiet) {
        output.promise.then(function(r) {
          callback(null, r.toString("utf-8"));
        });
      } else {
        callback(null);
      }
    }
  }).on("error", function(err) {
    if (err.code === "ENOENT") {
      callback(new Error("`" + cmd + "` executable not found."));
    }
  });
  if (quiet) {
    c.stdout.pipe(require("concat-stream")(function(data) {
      output.resolve(data);
    }));
  }
}

function invokeCompiler(cmd, quiet, deps, ffi, args, env, callback) {
  files.resolve(deps, function(err, deps) {
    if (err) {
      callback(err);
    } else {
      files.resolve(ffi, function(err, ffi) {
        if (err) {
          callback(err);
        } else {
          var allArgs = args.concat(deps).concat([].concat.apply([], ffi.map(function(path) {
            return ["--ffi", path];
          })));

          exec(cmd, quiet, allArgs, env, callback);
        }
      });
    }
  });
}

module.exports.exec = exec;
module.exports.invokeCompiler = invokeCompiler;
module.exports.psc = invokeCompiler.bind(null, "psc", true);
module.exports.pscMake = invokeCompiler.bind(null, "psc-make", true);
