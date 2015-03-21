var path = require("path");
var exec = require("./exec").exec;

module.exports = function(pro, args, callback) {
  var bowerArgs = args.remainder;
  exec(path.join(__dirname, "node_modules", ".bin", "bower"), false,
       bowerArgs, process.env, callback);
};
