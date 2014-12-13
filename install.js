var pscRelease = require("psc-release");
var mkdirp = require("mkdirp");
var exec = require("./exec");

module.exports = function(args, callback) {
  mkdirp(exec.bin, function(e) {
    if (e) callback(e);
    else pscRelease({ tag: args.tag
                    , bin: exec.bin }, callback);
  });
};
