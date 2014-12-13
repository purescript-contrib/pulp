var path = require("path");
var exec = require("./exec").exec;

module.exports = function(callback) {
  var bowerArgs = process.argv.slice(3);
  exec(path.join(__dirname, "node_modules", ".bin", "bower"), false,
       bowerArgs, process.env, callback);
}
