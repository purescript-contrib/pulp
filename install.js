var pscRelease = require("psc-release");

module.exports = function(args, callback) {
  pscRelease({tag: args.tag}, callback);
};
