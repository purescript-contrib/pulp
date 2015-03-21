var exec = require("./exec");
var log = require("./log");
var files = require("./files");

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  exec.pscMake(
    [files.src, files.deps], ["-o", args.buildPath].concat(args.remainder),
    null, function(err, rv) {
      if (err) return callback(err);
      log("Build successful.");
      callback(null);
    }
  );
};
