var exec = require("./exec").exec;
var log = require("./log");
var compare = require("ver-compare").compareVersions;

module.exports = function(callback) {
  exec("psc", true, ["--version"], process.env, function(err, ver) {
    ver = ver.trim();
    if (compare(ver, "0.7.0.0") < 0) {
      log.error("This version of Pulp requires PureScript version 0.7.0.0 or higher.");
      log.error("Your installed version is " + ver + ".");
      log.error("Please either upgrade PureScript or downgrade Pulp to version 3.x.");
      process.exit(1);
    }
    callback();
  });
};
