#!/usr/bin/env node

var path = require("path");
var fs = require("fs");
var log = require("./log");

var commands = {
  "install": {
    description: "Download and install project dependencies",
    action: require("./install")
  },
  "build": {
    description: "Build the project",
    action: require("./build")
  },
  "test": {
    description: "Run project tests",
    action: require("./test")
  }
  // "watch": {
  //   description: "Watch project for changes and rebuild as appropriate",
  //   action: require("./watch")
  // }
};

function printCommands() {
  console.error("\nAvailable commands:\n");
  for (var key in commands) {
    console.error("  " + key + " \t- " + commands[key].description);
  }
  console.error("");
}

var args = require("raptor-args").createParser({
  "--help -h": {
    description: "Show this help message"
  },
  "--command *": {
    type: "string"
  },
  "--bower-file -b": {
    type: "string",
    description: "Read this bower.json file instead of autodetecting it"
  },
  "--build-path -o": {
    type: "string",
    description: "Path for compiler output (default './build')"
  }
}).validate(function(result) {
  if (result.help) {
    this.printUsage();
    printCommands();
    process.exit(0);
  }
  var command = result.command;
  result.command = commands[command];
  if (!result.command) {
    console.error(command ? ("Unknown command '" + command + "'.")
                  : "No command specified.");
    printCommands();
    process.exit(1);
  }
}).onError(function(err) {
  this.printUsage();
  console.error(err);
  process.exit(1);
}).usage("Usage: cyan <command> [options]").parse();

if (!args.buildPath) args.buildPath = "./build";

require("./project")(args, function(err, pro) {
  if (err) {
    log.error("ERROR:", err.message);
    process.exit(1);
  } else {
    args.command.action(pro, args, function(err) {
      if (err) {
        log.error("ERROR:", err.message);
        process.exit(1);
      } else {
        process.exit(0);
      }
    });
  }
});
