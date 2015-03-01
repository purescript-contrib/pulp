var fs = require("fs");
var path = require("path");
var child = require("child_process");
var log = require("./log");
var Minimatch = require("minimatch").Minimatch;

function stripWatchArg(arg) {
  return arg !== "-w" && arg !== "--watch";
}

var match = new Minimatch("{src,test,bower_components}/**/*");

function watch(act) {
  require("watch").watchTree(".", {
    interval: 1337,
    ignoreDotFiles: true
  }, function(f, curr, prev) {
    if (!(typeof f === "object" && prev === null && curr === null)) {
      if (match.match(f)) {
        act();
      }
    }
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
  watch(change);
};
