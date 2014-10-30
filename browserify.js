var exec = require("./exec");
var log = require("./log");
var files = require("./files");
var browserify = require("browserify");
var path = require("path");
var fs = require("fs");
var stringStream = require("string-stream");

module.exports = function(pro, args, callback) {
  log("Browserifying project in", process.cwd());
  exec.psc([files.src, files.deps], [
    "--module", args.main, "--main", args.main
  ], null, function(err, src) {
    if (err) return callback(err);
    var nodePath = process.env.NODE_PATH;
    var buildPath = path.resolve(args.buildPath);
    process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
    var b = browserify({
      basedir: buildPath,
      entries: new stringStream(src)
    });
    if (args.transform) b.transform(args.transform);
    b.bundle().pipe(args.to ? fs.createWriteStream(args.to) : process.stdout)
      .on("close", function() {
        log("Browserified.");
        callback();
      });
  });
}
