var exec = require("./exec");
var path = require("path");
var log = require("./log");
var files = require("./files");
var util = require('util');

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  exec.pscMake([files.src, files.deps], ["-o", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    log("Build successful.");
    var buildPath = path.resolve(args.buildPath);
    var mainPath = path.resolve(args, buildPath, "Main", "index.js");
    var entryPoint = args.main.replace("\\", "\\\\").replace("'", "\\'");
    var sargs = "";
    if (args.args) {
      var vargs = [process.argv[0], mainPath].concat(args.args.split(' '));
      sargs = "process.argv = " + util.inspect(vargs) + ";";
    }
    exec.exec(process.argv[0], false, ["-e", sargs + "require('" + entryPoint + "').main()"], {
      PATH: process.env.PATH,
      NODE_PATH: buildPath + ":" + process.env.NODE_PATH
    }, callback);
  });
};
