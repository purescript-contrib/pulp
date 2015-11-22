var fs = require("fs");
var p = require("path");
var mkdirp = require("mkdirp");

function findIn(path, file, callback) {
  var fullPath = p.join(path, file);
  fs.exists(fullPath, function(exists) {
    if (exists) return callback(null, fullPath);
    if (path === "/") return callback(null, null);
    findIn(p.dirname(path), file, callback);
  });
}

function readConfig(configFilePath, callback) {
  fs.readFile(configFilePath, "utf-8", function(err, data) {
    var pro;
    if (err) return callback(err);
    try {
      pro = JSON.parse(data);
    } catch(e) {
      return callback(e);
    }
    var path = p.dirname(configFilePath);
    var cachePath = p.resolve(path, ".pulp-cache");
    process.chdir(path);
    mkdirp(cachePath, function(err) {
      if (err) return callback(err);
      callback(null, {
        path: path,
        cache: cachePath,
        bowerfile: pro
      });
    });
  });
}

module.exports = function(args, callback) {
  if (args.bowerFile) return readConfig(args.bowerFile, callback);
  findIn(process.cwd(), "bower.json", function(err, path) {
    if (err) return callback(err);
    if (path === null) return callback(new Error("No bower.json found in current or parent directories. Are you in a PureScript project?"));
    readConfig(path, callback);
  });
};
