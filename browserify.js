var exec = require("./exec");
var log = require("./log");
var files = require("./files");
var build = require("./build");
var browserify = require("browserify");
var path = require("path");
var fs = require("fs");
var stringStream = require("string-stream");

function optimising(pro, args, callback) {
  log("Compiling...");
  var globSet = files.defaultGlobs.union(files.SourceFileGlobSet(args.includePaths));

  exec.psc(
    globSet.sources(),
    globSet.ffis(),
    ["--module=" + args.main, "--main=" + args.main].concat(args.remainder),
    null, function(err, src) {
      if (err) return callback(err);
      log("Compilation successful.");
      log("Browserifying...");
      var nodePath = process.env.NODE_PATH;
      var buildPath = path.resolve(args.buildPath);
      process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
      var b = browserify({
        basedir: buildPath,
        entries: new stringStream(src)
      });
      if (args.transform) b.transform(args.transform);
      b.bundle().pipe(args.to ? fs.createWriteStream(args.to) : process.stdout)
        .on("close", callback);
    }
  );
}

function incremental(pro, args, callback) {
  var fn = function(err) {
    if (err) return callback(err);
    log("Browserifying...");
    var nodePath = process.env.NODE_PATH;
    var buildPath = path.resolve(args.buildPath);
    process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
    var b = browserify({
      basedir: buildPath
    });
    if (args.skipEntryPoint) {
      b.add(path.join(buildPath, args.main));
    } else {
      var entryPoint = args.main.replace("\\", "\\\\").replace("'", "\\'");
      var src = "require('" + entryPoint + "').main();\n";
      var entryPath = path.join(buildPath, "browserify.js");
      fs.writeFileSync(entryPath, src, "utf-8");
      b.add(entryPath);
    }
    if (args.transform) b.transform(args.transform);
    b.bundle().pipe(args.to ? fs.createWriteStream(args.to) : process.stdout)
      .on("close", callback);
  };
  if (args.skipCompile) {
    fn(null);
  } else {
    build(pro, args, fn);
  }
}

module.exports = function(pro, args, callback) {
  log("Browserifying project in", process.cwd());
  (args.optimise ? optimising : incremental)(pro, args, function(err) {
    if (err) {
      callback(err);
    } else {
      log("Browserified.");
      callback();
    }
  });
};
