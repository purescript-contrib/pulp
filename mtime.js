var Rx = require("rx");
var fs = require("fs");
var p = require("path");
var Promise = require("bluebird");

var fsReaddir = Promise.promisify(fs.readdir);
function readdir(path) {
  return Rx.Observable.fromPromise(fsReaddir(path))
    .flatMap(function(files) {
      return Rx.Observable.from(files);
    });
}

var fsStat = Promise.promisify(fs.stat);
function stat(path) {
  return Rx.Observable.fromPromise(fsStat(path).then(function(stats) {
    stats.path = path;
    return stats;
  }));
}

function scanDir(path) {
  return readdir(path).map(function(file) {
    return p.resolve(path, file);
  }).flatMap(stat).flatMap(function(stats) {
    if (stats.isDirectory()) {
      return scanDir(stats.path);
    } else {
      return Rx.Observable.return(stats);
    }
  });
}

function maxMtime(path) {
  return fsStat(path).then(function(stats) {
    if (stats.isFile()) {
      return stats.mtime.getTime();
    } else {
      return scanDir(path).map(function(stats) {
        return stats.mtime.getTime();
      }).max().toPromise(Promise);
    }
  });
}
module.exports = maxMtime;
