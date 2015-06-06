var exec = require("./exec");
var path = require("path");
var log = require("./log");
var files = require("./files");
var extend = require('util')._extend;
var fs = require("fs");
var temp = require("temp").track();

module.exports = function(pro, args, callback) {
  log("Building project in", process.cwd());
  exec.pscMake([files.src, files.deps], [files.srcForeign, files.depsForeign], ["-o", args.buildPath], null, function(err, rv) {
    if (err) return callback(err);
    log("Build successful.");
    var buildPath = path.resolve(args.buildPath);
    var mainPath = path.resolve(args, buildPath, "Main", "index.js");
    var entryPoint = args.main.replace("\\", "\\\\").replace("'", "\\'");
    var env = extend({}, process.env);
    env.NODE_PATH = buildPath + path.delimiter + process.env.NODE_PATH;
    var src = "require('" + entryPoint + "').main();\n";
    temp.open({prefix: "pulp-run", suffix: ".js"}, function(err, info) {
      if (err) return callback(err);
      fs.write(info.fd, src, 0, "utf-8", function(err) {
        if (err) return callback(err);
        fs.close(info.fd, function(err) {
          if (err) return callback(err);
          exec.exec(
            "node", false, [info.path].concat(args.remainder),
            env, callback
          );
        });
      });
    });
  });
};
