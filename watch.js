var fs = require("fs");

var path = require("path");

var child = require("child_process");

var watchman = require("fb-watchman");

var log = require("./log");

var client = new watchman.Client();

var cwd = process.cwd();

var watchDirectories =  [path.join(cwd, "src"), path.join(cwd, "bower_components"), path.join(cwd, "test")];

var subscriptionPrefix = "pulp-";

function subscribeToWatch(watchDirectory, watch, relativePath, callback) {
  client.command(["clock", watch], function(error, res){
    if (error) callback(error);
    else {
      var sub = {
        expression: ["anyof", ["match", "*.purs"], ["match", "*.js"]],
        fields: ["name", "size", "exists", "type"],
        since: res.clock,
        relative_root: relativePath
      };

      var subscriptionName = subscriptionPrefix + watchDirectory;

      client.command(["subscribe", watch, subscriptionName, sub], function(error, res){
        if (error) callback(error);
        else {
          client.on("subscription", function(res){
            if (res.subscription === subscriptionName) {
              callback(null);
            }
          });
        }
      });
    }
  });
}

function watchProject(watchDirectory, callback) {
  client.command(["watch-project", watchDirectory], function (error, res){
    if (error) callback(error);
    else {
      var watch = res.watch;

      var relativePath = res.relative_path;

      if (res.warning) {
        log.error("Warning: " + res.warning);
      }

      log("Watch established on " + watch + " with relative path " + relativePath);

      callback(null, {watch: watch, relativePath: relativePath});
    }
  });
}

function watch(callback) {
  client.capabilityCheck({optional:[], required:["relative_root"]}, function (error, res){
    if (error) {
      log.error("Watchman capability check failed. " + error);

      client.end();
    }
    else {
      watchDirectories.forEach(function(watchDirectory){
        watchProject(watchDirectory, function(error, res){
          if (error) log.error("Failed to watch project " + watchDirectory + ". " + error);
          else {
            subscribeToWatch(watchDirectory, res.watch, res.relativePath, function(error){
              if (error) log.error("Failed to subscribe to watch. " + error);
              else {
                callback();
              }
            });
          }
        });
      });
    }
  });
}

function stripWatchArg(arg) {
  return arg !== "-w" && arg !== "--watch";
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

module.exports.watch = watch;
