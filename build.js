var exec = require("./exec");
var log = require("./log");
var files = require("./files");
var fs = require("fs");
var rebuild = require("./rebuild");

function build(pro, args) {
  return new Promise(function(resolve, reject) {
    log("Building project in", process.cwd());
    var globSet = files.defaultGlobs(args).union(files.SourceFileGlobSet(args.includePaths));
    if (args.testBuild) {
      globSet = globSet.union(files.testGlobs(args));
    }

    exec.psc(
      globSet.sources(),
      globSet.ffis(),
      ["-o", args.buildPath].concat(args.remainder),
      null, function(err, rv) {
        if (err) return reject(err);
        rebuild.touch(pro, args).then(function() {
          log("Build successful.");
          if (args.optimise || args.to) {
            log("Bundling Javascript...");

            var bundleArgs = [
              "--module=" + args.main, "--main=" + args.main
            ].concat(
              (args.modules || "").split(",").map(function(m) { return "--module=" + m; }),
              args.remainder
            );

            exec.pscBundle(
              [files.outputModules(args.buildPath)],
              bundleArgs, null, function(err, src) {
                if (err) return reject(err);
                var out = args.to ? fs.createWriteStream(args.to) : process.stdout;
                out.write(src, "utf-8", function(err) {
                  if (err) return reject(err);
                  log("Bundled.");
                  resolve(null);
                });
              });
          } else {
            resolve(null);
          }
        });
      }
    );
  });
};

module.exports = function(pro, args, callback) {
  var paths = [args.srcPath, args.dependencyPath].concat(args.includePaths || []);
  rebuild.needs(pro, args, paths).then(function(shouldRebuild) {
    if (shouldRebuild || args.force) {
      build(pro, args).then(callback, callback);
    } else {
      log("Project unchanged; skipping build step.");
      callback(null);
    }
  });
};
