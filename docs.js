var log = require("./log");
var files = require("./files");
var child = require("child_process");
var glob = require("glob");
var fs = require("fs");

module.exports = function(pro, args, callback) {
  log("Generating documentation in", process.cwd());
  var src = [files.src];
  if (args.withTests) src.push(files.test);
  if (args.withDeps) src.push(files.deps);
  files.resolve(src, function(err, files) {
    var c = child.spawn("psc-docs", args.remainder.concat(files), {
      stdio: [process.stdin, "pipe", process.stderr]
    }).on("exit", function(code, signal) {
      if (code) {
        callback(new Error("Subcommand terminated with error code " + code), code);
      } else {
        log("Documentation generated.");
        callback(null, 0);
      }
    }).on("error", function(err) {
      if (err.code === "ENOENT") {
        callback(new Error("`psc-docs` executable not found."));
      }
    });
    c.stdout.pipe(args.to ? fs.createWriteStream(args.to) : process.stdout);
  });
};
