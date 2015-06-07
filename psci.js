var log = require("./log");
var files = require("./files");
var fs = require("fs");
var child = require("child_process");

function updateConfig(callback) {
  fs.readFile(".psci", function(err, data) {
    if (err && err.code !== "ENOENT") {
      return callback(err);
    }
    var entries = err ? [] : data.toString().split("\n").filter(function(e) { 
      return e.indexOf(":load ") !== 0 &&
             e.indexOf(":foreign ") !== 0 &&
             e.trim().length; 
    });
    files.resolve([files.src, files.test, files.deps], function(err, deps) {
      files.resolve([files.srcForeign, files.testForeign, files.depsForeign], function(err, ffi) {
        var psci = entries.concat(deps.map(function(path) { 
          return ":load " + path; 
        })).concat(ffi.map(function(path) {
          return ":foreign " + path;
        }));
        fs.writeFile(".psci", psci.join("\n") + "\n", "utf-8", function(err) {
          callback(err);
        }); 
      });
    });
  });
}

module.exports = function(pro, args, callback) {
  updateConfig(function(err) {
    if (err) {
      return callback(err);
    }
    child.spawn("psci", args.remainder, {
      stdio: "inherit"
    }).on("exit", function(code, signal) {
      callback();
    });
  });
};
