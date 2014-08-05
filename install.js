var u = require("underscore");
var bower = require("bower");
var standardRenderer = require("bower/lib/renderers/StandardRenderer");

module.exports = function(pro, args, callback) {
  var log = new standardRenderer("update", {});
  var deps = u.keys(pro.devDependencies).concat(u.keys(pro.dependencies));
  bower.commands.install(deps).on("end", function(res) {
    callback();
  }).on("log", function(data) {
    log.log(data);
  }).on("error", function(err) {
    callback(err);
  });
}
