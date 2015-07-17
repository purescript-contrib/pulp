var path = require("path");
var exec = require("./exec").exec;

module.exports = (function() {
  var exp = function(pro, args, callback) {
    var bowerArgs = args.remainder;
    exp.launchBower(bowerArgs, callback);
  };

  exp.launchBower = function(bowerArgs, callback) {
    var executable = "bower";
    exec(path.join(__dirname, "node_modules", ".bin", executable), false,
      bowerArgs, process.env, callback);
  };

  return exp;
})();
