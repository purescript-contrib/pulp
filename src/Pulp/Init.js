// module Pulp.Init
"use strict";

exports.bowerFile = function bowerFile(name) {
  return JSON.stringify({
    name: name,
    ignore: [
      "**/.*",
      "node_modules",
      "bower_components",
      "output"
    ],
    dependencies: {
      "purescript-console": "^2.0.0"
    },
    devDependencies: {
      "purescript-psci-support": "^2.0.0"
    },
  }, null, 2) + "\n";
};
