var log = require("./log");
var files = require("./files");
var child = require("child_process");
var glob = require("glob");
var fs = require("fs");

module.exports = function(pro, args, callback) {
  log("Generating documentation in", process.cwd());
  var src = [files.src, files.deps];
  var gen = [files.src];
  if (args.withTests) {
    src.push(files.test);
    gen.push(files.test)
  }
  files.resolve(src, function(err, srcFiles) {
    files.resolve(gen, function(err, genFiles) {
      var docgen = [].concat.apply([], genFiles.map(function(path) {
        var moduleName = path.match("([A-Z][^\/\.]*(\/|\.))+");
        var docPath = path.replace(/^(src|test)/, "docs").replace(/.purs$/, ".md");
        if (moduleName && moduleName[0]) {
          return ["--docgen", moduleName[0].replace(/\.$/, "").replace(/(\/|\\)/g, ".") + ":" + docPath]; 
        } else {
          log("Unable to generate documentation for " + path + ". Please make sure your module names are consistent with your file paths.");
          return [];
        }
      }));
      child.spawn("psc-docs", args.remainder.concat(srcFiles).concat(docgen), {
        stdio: [process.stdin, "pipe", process.stderr]
      }).on("exit", function(code, signal) {
        if (code) {
          callback(new Error("Subcommand terminated with error code " + code), code);
        } else {
          log("Documentation generated.");
          callback(null, 0);
        }
      }).on("error", function(err) {
        if (err.code === "ENOENT") {
          callback(new Error("`psc-docs` executable not found."));
        }
      });
    });
  });
};
