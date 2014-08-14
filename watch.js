var fs = require("fs");
var path = require("path");
var child = require("child_process");
var log = require("./log");

function stripWatchArg(arg) {
  return arg !== "-w" && arg !== "--watch";
}

function watch(p, act) {
  require("watch").createMonitor(p, {interval: 1000}, function(mon) {
    mon.on("changed", act);
    mon.on("created", act);
    mon.on("removed", act);
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
  fs.existsSync("src") && watch("src", change);
  fs.existsSync("test") && watch("test", change);
};
