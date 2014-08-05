var u = require("underscore");
var c = require("ansi")(process.stderr);
var util = require("util");

function log(severity) {
  var args = u.toArray(arguments).slice(1);
  c[severity]().bold().write("*").reset().write(" ");
  console.error.apply(null, args);
}

module.exports = log.bind(null, "green");
module.exports.error = log.bind(null, "red");
