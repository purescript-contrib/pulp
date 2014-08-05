var exec = require("./exec");
var path = require("path");

module.exports = function(pro, args, callback) {
  console.error("* Building project in", process.cwd());
  exec.pscMake("{test,src,bower_components}/**/*.purs", ["--output", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    console.error("* Build successful. Running tests...");
    var buildPath = path.resolve(args.buildPath);
    var mainPath = path.resolve(args, buildPath, "Main", "index.js");
    exec.exec("node", false, ["-e", "require('Main').main()"], {
      PATH: process.env.PATH,
      NODE_PATH: buildPath + ":" + process.env.NODE_PATH
    }, function(err, rv) {
      if (err) return callback(err);
      console.error("* Tests OK.");
      callback();
    });
  });
};
