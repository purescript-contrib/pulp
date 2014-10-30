var log = require("./log");
var glob = require("glob");
var fs = require("fs");

module.exports = function(pro, args, callback) {
  "use strict";

  var entries = [];
  var isNew = true;

  var pushEntry = function(entry) {
    entries.push(":m " + entry);
  };

  try {
    entries = fs.readFileSync(".psci").toString().split("\n");
    isNew = false;
  } catch (err) {
      if (err.code !== "ENOENT") {
        log(err);
        return;
      }
  }

  entries = entries.filter(function (entry) {
      return entry.indexOf(":m") !== 0;
  });

  glob.sync("src/**/*.purs").map(pushEntry);
  glob.sync("bower_components/purescript-*/src/**/*.purs").map(pushEntry);
  glob.sync("bower_components/purescript-*/src/**/*.purs.hs").map(pushEntry);
  fs.writeFileSync(".psci", entries.join("\n"), "utf8");
  log((isNew ? "Created" : "Updated") + " .psci file");
};
