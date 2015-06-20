var path = require("path");
var exec = require("./exec").exec;

module.exports = (function() {
  var exp = function(pro, args, callback) {
    var bowerArgs = args.remainder;
    this.launchBower(bowerArgs, callback);
  };

  exp.launchBower = function(bowerArgs, callback) {
    var executable = "bower";
    if (process.platform == "win32") {
      executable += ".cmd";
    }
    exec(path.join(__dirname, "node_modules", ".bin", executable), false,
      bowerArgs, process.env, callback);
  };

  return exp;
})();
