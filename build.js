var exec = require("./exec");
var log = require("./log");

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  exec.pscMake("{src,bower_components}/**/*.purs", ["--output", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    log("Build successful.");
    callback(null);
  });
};
