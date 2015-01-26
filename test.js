var exec = require("./exec");
var path = require("path");
var log = require("./log");
var files = require("./files");

module.exports = function(pro, args, callback) {
  log.message("Building project in", process.cwd());
  exec.pscMake([files.src, files.test, files.deps], ["--output", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    log.message("Build successful. Running tests...");
    var buildPath = path.resolve(args.buildPath);
    var mainPath = path.resolve(args, buildPath, "Main", "index.js");
    exec.exec("node", false, ["-e", "require('Test.Main').main()"], {
      PATH: process.env.PATH,
      NODE_PATH: buildPath + ":" + process.env.NODE_PATH
    }, function(err, rv) {
      if (err) return callback(err);
      log.message("Tests OK.");
      callback();
    });
  });
};
