var build = require("./build");
var browserify = require("browserify");
var path = require("path");
var fs = require("fs");

module.exports = function(pro, args, callback) {
  build(pro, args, function(err) {
    if (err) return callback(err);
    var nodePath = process.env.NODE_PATH;
    var buildPath = path.resolve(args.buildPath);
    process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
    var b = browserify({
      basedir: buildPath
    });
    b.add(args.main);
    if (args.transform) b.transform(args.transform);
    b.bundle().pipe(args.to ? fs.createWriteStream(args.to) : process.stdout);
  });
}
