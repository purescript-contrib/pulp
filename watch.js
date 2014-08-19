var fs = require("fs");
var path = require("path");
var child = require("child_process");
var log = require("./log");

function stripWatchArg(arg) {
  return arg !== "-w" && arg !== "--watch";
}

function watch(p, act) {
  require("gaze")(p, function(err, watcher) {
    if (err) {

    }
    watcher.on("all", act);
  });
}

module.exports = function() {
  var mod = path.join(__dirname, "/index"),
      args = process.argv.filter(stripWatchArg).slice(2);
  var p = child.fork(mod, args);
  var change = function() {
    p.kill("SIGTERM");
    log("Source tree changed; restarting:");
    p = child.fork(mod, args);
  };
  watch(["src/**/*", "test/**/*", "bower_components/**/*"], change);
};
