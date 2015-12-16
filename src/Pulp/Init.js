// module Pulp.Init
"use strict";

exports.bowerFile = function bowerFile(name) {
  return JSON.stringify({
    name: name,
    version: "1.0.0",
    moduleType: ["node"],
    ignore: [
      "**/.*",
      "node_modules",
      "bower_components",
      "output"
    ],
    dependencies: {
      "purescript-console": "^0.1.0"
    }
  }, null, 2) + "\n";
};
