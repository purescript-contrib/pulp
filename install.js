var u = require("underscore");
var bower = require("bower");
var standardRenderer = require("bower/lib/renderers/StandardRenderer");
var log = require("./log");

module.exports = function(pro, args, callback) {
  log("Installing dependencies...");
  var bowerLog = new standardRenderer("update", {});
  var deps = u.keys(pro.devDependencies).concat(u.keys(pro.dependencies));
  bower.commands.install(deps).on("end", function(res) {
    log("Dependencies installed.");
    callback();
  }).on("log", function(data) {
    bowerLog.log(data);
  }).on("error", function(err) {
    callback(err);
  });
}
