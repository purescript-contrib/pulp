var exec = require("./exec");
var path = require("path");
var log = require("./log");

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  exec.pscMake("{src,bower_components}/**/*.purs", ["--output", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    log("Build successful.");
    var buildPath = path.resolve(args.buildPath);
    var mainPath = path.resolve(args, buildPath, "Main", "index.js");
    var entryPoint = args.main.replace("\\", "\\\\").replace("'", "\\'");
    exec.exec("node", false, ["-e", "require('" + entryPoint + "').main()"], {
      PATH: process.env.PATH,
      NODE_PATH: buildPath + ":" + process.env.NODE_PATH
    }, callback);
  });
};
