var u = require("underscore");
var c = require("ansi")(process.stderr);
var util = require("util");

var mono = false;
var fullLine = false;

function log(severity) {
  var args = u.toArray(arguments).slice(1);
  if (mono) {
    c.write("* ");
  } else {
    c.hex(severity).bold().write("*");
    if (fullLine) {
      c.resetBold().write(" ");
    } else {
      c.reset().write(" ");
    }
  }
  console.error.apply(null, args);
  if (fullLine) {
    c.reset();
  }
}

module.exports.message = log.bind(null, "4e9a06");
module.exports.error = log.bind(null, "cc0000");
module.exports.mono = function(flag) { mono = flag; };
module.exports.fullLine = function(flag) { fullLine = flag; };
module.exports.msgColor = function(msgColor) { 
  module.exports.message = log.bind(null, msgColor);
};
module.exports.errColor = function(errColor) { 
  module.exports.error = log.bind(null, errColor);
};
