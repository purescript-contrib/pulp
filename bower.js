var path = require("path");
var exec = require("./exec").exec;

module.exports = (function() {
  var exp = function(pro, args, callback) {
    var bowerArgs = args.remainder;
    exp.launchBower(bowerArgs, callback);
  };

  exp.launchBower = function(bowerArgs, callback) {
    var executable = path.join(require.resolve("bower"), "..", "..", "bin", "bower");
    exec(executable, false,
      bowerArgs, process.env, callback);
  };

  return exp;
})();
