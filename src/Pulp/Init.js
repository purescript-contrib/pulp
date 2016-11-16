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
    },
    devDependencies: {
    },
  }, null, 2) + "\n";
};
