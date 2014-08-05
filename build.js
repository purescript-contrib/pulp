var exec = require("./exec");

module.exports = function(pro, args, callback) {
  console.error("* Building project in", process.cwd());
  exec.pscMake("{src,bower_components}/**/*.purs", ["--output", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    console.error("* Build successful.");
    callback(null);
  });
};
