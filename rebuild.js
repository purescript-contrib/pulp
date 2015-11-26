var Promise = require("bluebird");
var path = require("path");
var fs = require("fs");
var crypto = require("crypto");
var mtime = require("./mtime");

var readFile = Promise.promisify(fs.readFile);
var writeFile = Promise.promisify(fs.writeFile);

function argHash(pro, args) {
  var hash = crypto.createHash("md5");
  hash.update(JSON.stringify(pro));
  hash.update(JSON.stringify(args));
  return hash.digest("hex");
}

function sameArgs(pro, args) {
  return readFile(path.resolve(pro.cache, "build-stamp"), "utf-8").then(function(file) {
    return file === argHash(pro, args);
  }, function() {
    return false;
  });
}

function needsRebuild(pro, args, paths) {
  if (args.force) {
    return Promise.resolve(true);
  }
  var liveStamp = Promise.reduce(paths.map(mtime), function(acc, next) {
    return acc > next ? acc : next;
  });
  var cacheStamp = mtime(path.resolve(pro.cache, "build-stamp")).catch(function(err) {
    return 0;
  });
  var stampChanged = Promise.all([liveStamp, cacheStamp]).spread(function(live, cache) {
    return live > cache;
  });
  return Promise.all([stampChanged, sameArgs(pro, args)]).spread(function(stamp, args) {
    return stamp || !args;
  });
}

function touchRebuild(pro, args) {
  var p = path.resolve(pro.cache, "build-stamp");
  return writeFile(p, argHash(pro, args), "utf-8");
}

module.exports = {
  needs: needsRebuild,
  touch: touchRebuild
};
