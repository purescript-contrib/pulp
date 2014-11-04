var log = require("./log");
var files = require("./files");
var child = require("child_process");
var glob = require("glob");
var fs = require("fs");

module.exports = function(pro, args, callback) {
  log("Generating documentation in", process.cwd());
  var input = args.to ? fs.createWriteStream(args.to) : process.stdout;
  
  function next(code, signal) {
    if (code) {
      callback(new Error("Subcommand terminated with error code " + code), code);
    } else {
      log("Documentation generated.");
      callback(null, 0);
    }
  }
  
  function go(binaryName, success, failure) {
    var c = child.spawn("psc-docs", files, {
      stdio: [process.stdin, "pipe", process.stderr]
    }).on("exit", success).on("error", failure);
    c.stdout.pipe(input);
  }
  
  files.src(function(err, files) {
    if (err) return callback(err);
    go("psc-docs", next, function(err) {
      if (err.code !== "ENOENT") return callback(err);
      go("docgen", next, function(err) {
        if (err.code !== "ENOENT") return callback(err);
        callback(new Error("psc-docs (formerly docgen) executable not found."));
      });
    });
  });
};
