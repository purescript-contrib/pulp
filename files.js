var glob = require("glob");
var fs = require("fs");
var path = require("path");


function readJson(fn) {
  try {
    return JSON.parse(fs.readFileSync(fn).toString());
  } catch (e) {
    return {};
  }
}

var bowerDirs = path.join(readJson(".bowerrc").directory || "bower_components", "purescript-*/");

exports.srcGlob = "src/**/*.purs";
exports.depsGlob = bowerDirs + exports.srcGlob;
exports.testGlob = "test/**/*.purs";
exports.ffiGlobs = ["src/**/*.js", bowerDirs + "src/**/*.js", "test/**/*.js"];

function deps(callback) {
  var bowerrc = readJson(".bowerrc");
  if (bowerrc.directory) {
    glob(bowerrc.directory + "/purescript-*/src/**/*.purs", {}, callback);
  } else {
    glob("bower_components/purescript-*/src/**/*.purs", {}, callback);
  }
}
module.exports.deps = deps;

function depsForeign(callback) {
  var bowerrc = readJson(".bowerrc");
  if (bowerrc.directory) {
    glob(bowerrc.directory + "/purescript-*/src/**/*.js", {}, callback);
  } else {
    glob("bower_components/purescript-*/src/**/*.js", {}, callback);
  }
}
module.exports.depsForeign = depsForeign;

function src(callback) {
  glob("src/**/*.purs", {}, callback);
}
module.exports.src = src;

function srcForeign(callback) {
  glob("src/**/*.js", {}, callback);
}
module.exports.srcForeign = srcForeign;

function test(callback) {
  glob("test/**/*.purs", {}, callback);
}
module.exports.test = test;

function testForeign(callback) {
  glob("test/**/*.js", {}, callback);
}
module.exports.testForeign = testForeign;

function outputModules(buildPath) {
  return function(callback) {
    glob(buildPath + "/*/@(index.js|foreign.js)", {}, callback);
  };
}
module.exports.outputModules = outputModules;

function resolve(fns, callback) {
  function it(acc, fns, callback) {
    if (!fns.length) {
      callback(null, acc);
    } else {
      fns[0](function(err, res) {
        if (err) {
          callback(err);
        } else {
          it(acc.concat(res), fns.slice(1), callback);
        }
      });
    }
  }
  it([], fns, callback);
}
module.exports.resolve = resolve;
