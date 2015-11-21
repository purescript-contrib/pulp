var fs = require("fs");
var path = require("path");
var child = require("child_process");
var log = require("./log");
var index = require("./index");
var Watchpack = require("watchpack");

function stripWatchArg(arg) {
  return arg !== "-w" && arg !== "--watch";
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

  var srcPath = index.opts.opts.srcPath;

  var testPath = index.opts.opts.testPath;

  var dependencyPath = index.opts.opts.dependencyPath;

  var directories = srcPath.concat(testPath).concat(dependencyPath);

  watch(directories, function(){
    p.kill("SIGTERM");
    log("Source tree changed; restarting:");
    p = child.fork(mod, args);
  });
};

module.exports.watch = watch;
