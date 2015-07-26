var log = require("./log");
var files = require("./files");
var exec = require("./exec");

module.exports = function(pro, args, callback) {
  log("Generating documentation in", process.cwd());
  var globSrc = files.defaultGlobs;
  var globGen = files.localGlobs;
  if (args.withTests) {
    globSrc = globSrc.union(files.testGlobs);
    globGen = globGen.union(files.testGlobs);
  }

  files.resolveGlobs(globGen.sources(), function(err, genFiles) {
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
    exec.exec(
      "psc-docs", true, args.remainder.concat(globSrc.sources()).concat(docgen),
      null, function(err, code) {
        if(err) return callback(err, code);
        if(code) return callback(new Error("Subcommand terminated with error code " + code), code);
        log("Documentation generated.");
        callback(null, 0);
      }
    );
  });
};
