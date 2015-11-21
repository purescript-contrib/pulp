var fs = require("fs");
var path = require("path");
var child = require("child_process");
var log = require("./log");
var files = require("./files");
var index = require("./index");
var minimatch = require("minimatch");
var Watchpack = require("watchpack");

function stripWatchArg(arg) {
  return arg !== "-w" && arg !== "--watch";
}

function topLevel(directories) {
  return directories.map(function(a){
    return a.split(path.sep)[0];
  });
}

function watch(directories, act) {
  var watchpack = new Watchpack();

  watchpack.watch([], directories, Date.now() - 10000);

  watchpack.on("change", act);
}

module.exports = function() {
  var mod = path.join(__dirname, "/index");

  var args = process.argv.filter(stripWatchArg).slice(2);

  var p = child.fork(mod, args);

  var srcPath = topLevel(index.opts.opts.srcPath);

  var testPath = topLevel(index.opts.opts.testPath);

  var dependencyPath = topLevel(index.opts.opts.dependencyPath);

  var directories = srcPath.concat(testPath).concat(dependencyPath);

  var localGlobs = files.localGlobs(index.opts.opts);

  var testGlobs = files.testGlobs(index.opts.opts);

  var dependencyGlobs = files.dependencyGlobs(index.opts.opts);

  var globs = localGlobs.union(testGlobs).union(dependencyGlobs)

  var sourceGlobs = globs.sources();

  var ffiGlobs = globs.ffis();

  var fileGlobs = sourceGlobs.concat(ffiGlobs);

  watch(directories, function(change){
    var matched = fileGlobs.some(function(a){
      return minimatch(change, a);
    });
    if (matched) {
      p.kill("SIGTERM");
      log("Source tree changed; restarting:");
      p = child.fork(mod, args);
    }
  });
};

module.exports.watch = watch;
