var exec = require("./exec");
var path = require("path");
var log = require("./log");
var files = require("./files");

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  var done = function(err, rv) {
    if (err) return callback(err);
    log("Tests OK.");
    callback();
  };
  var globSet = files.defaultGlobs.union(files.SourceFileGlobSet(args.includePaths));

  exec.psc(
    globSet.sources(),
    globSet.ffis(),
    ["-o", args.buildPath],
    null, function(err, rv) {
      if (err) return callback(err);

      if (args.testRuntime) {
        if (!args.to) args.to = "./output/test.js";
        log("Build successful. Bundling Javascript...");
        exec.pscBundle(
          [files.outputModules(args.buildPath)],
          ["-o", args.to, "--module", args.main, "--main", args.main],
          null, function(err, rv) {
            if (err) return callback(err);
            log("Running tests...");
            exec.exec(
              args.testRuntime, false, [args.to].concat(args.remainder),
              process.env, done
            );
          }
        );
      } else {
        var buildPath = path.resolve(args.buildPath);
        log("Build successful. Running tests...");
        exec.exec(
          args.engine, false,
          ["-e", "require('" + args.main + "').main()"].concat(args.remainder),
          {
            PATH: process.env.PATH,
            NODE_PATH: buildPath + path.delimiter + process.env.NODE_PATH
          }, done
        );
      }
    }
  );
};
