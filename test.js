var exec = require("./exec");
var path = require("path");
var log = require("./log");
var files = require("./files");
var build = require("./build");
var merge = require("merge");

module.exports = function(pro, args, callback) {
  var done = function(err, rv) {
    if (err) return callback(err);
    log("Tests OK.");
    callback();
  };
  var globSet =
        [ files.defaultGlobs(args)
          , files.testGlobs(args)
          , files.SourceFileGlobSet(args.includePaths)
        ].reduce(function(a, b) { return a.union(b); });

  var buildArgs = merge({}, args, {testBuild: true, remainder: []});
  if (args.engine !== "node") {
    buildArgs.to = "./output/test.js";
  }

  build(pro, buildArgs, function(err) {
    if (err) return callback(err);
    log("Running tests...");
    if (args.engine !== "node") {
      exec.exec(
        args.engine, false, [buildArgs.to].concat(args.remainder),
        process.env, done
      );
    } else {
      exec.exec(
        args.engine, false,
        ["-e", "require('" + args.main + "').main()"].concat(args.remainder),
        {
          PATH: process.env.PATH,
          NODE_PATH: args.buildPath + path.delimiter + process.env.NODE_PATH
        }, done
      );
    }
  });
};
