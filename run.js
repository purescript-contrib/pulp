var exec = require("./exec");
var path = require("path");
var log = require("./log");
var files = require("./files");
var extend = require('util')._extend;

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  exec.pscMake([files.src, files.deps], ["--output", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    log("Build successful.");
    var buildPath = path.resolve(args.buildPath);
    var mainPath = path.resolve(args, buildPath, "Main", "index.js");
    var entryPoint = args.main.replace("\\", "\\\\").replace("'", "\\'");
    var env = extend({}, process.env);
    env.NODE_PATH = buildPath + ":" + process.env.NODE_PATH;
    exec.exec("node", false, ["-e", "require('" + entryPoint + "').main()"], env, callback);
  });
};
