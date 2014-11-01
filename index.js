#!/usr/bin/env node

var path = require("path");
var fs = require("fs");
var log = require("./log");

var commands = {
  "init": {
    description: "Generate an example PureScript project",
    action: require("./init"),
    noProject: true
  },
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
  },
  "browserify": {
    description: "Produce a deployable bundle using Browserify",
    action: require("./browserify")
  },
  "run": {
    description: "Compile and run the project",
    action: require("./run")
  },
  "docgen": {
    description: "Generate project documentation",
    action: require("./docgen")
  },
  "psci": {
    description: "Create or update the .psci file",
    action: require("./psci")
  }
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
  },
  "--main -m": {
    type: "string",
    description: "Application's entry point"
  },
  "--to -t": {
    type: "string",
    description: "Output file name for bundle (default stdout)"
  },
  "--transform": {
    type: "string",
    description: "Apply a Browserify transform"
  },
  "--source-map": {
    type: "string",
    description: "Tell Browserify to generate source maps"
  },
  "--watch -w": {
    type: "boolean",
    description: "Watch source directories and re-run command if something changes"
  },
  "--force": {
    type: "boolean",
    description: "For 'pulp init', overwrite any project found in the current directory"
  },
  "--optimise -O": {
    type: "boolean",
    description: "Perform dead code elimination when browserifying"
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

if (!args.buildPath) args.buildPath = "./output";

if (!args.main) args.main = "Main";

function done(err) {
  if (err) {
    log.error("ERROR:", err.message);
    process.exit(1);
  } else {
    process.exit(0);
  }
}

if (args.command.noProject) {
  args.command.action(args, done);
} else {
  require("./project")(args, function(err, pro) {
    if (err) {
      log.error("ERROR:", err.message);
      process.exit(1);
    } else {
      if (args.watch) {
        require("./watch")();
      } else {
        args.command.action(pro, args, done);
      }
    }
  });
}
