var temp = require("temp").track();
var fs = require("fs");
var exec = require("./exec").exec;
var log = require("./log");

module.exports = function(cmd, callback) {
  log("Executing " + cmd);
  temp.open({"prefix": "pulp-cmd-", "suffix": ".sh"}, function(err, info) {
    fs.write(info.fd, cmd, 0, encoding="utf-8", function(err) {
      if (err) {
        callback(err);
      } else {
        exec("sh", false, [info.path], process.env, function(err, code) {
          log("Done.");
          callback();
        });
      }
    });
  });
};
