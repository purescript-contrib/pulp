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
  "dep": {
    description: "Invoke Bower for package management",
    action: require("./bower"),
    noParse: true
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
  "docs": {
    description: "Generate project documentation",
    action: require("./docs")
  },
  "psci": {
    description: "Launch a PureScript REPL configured for the project",
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

function done(args) {
  return function doneFunc(err) {
    if (err) {
      log.error("ERROR:", err.message);
      process.exit(1);
    } else {
      if (args.then) {
        require("./shell")(args.then, done({}));
      } else {
        process.exit(0);
      }
    }
  }
}

function runNoParseCmd() {
  var cmd = commands[process.argv[2]];
  if (cmd && cmd.noParse) {
    cmd.action(done({}));
    return true;
  } else {
    return false;
  }
}

if (!runNoParseCmd()) {
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
    },
    "--monochrome": {
      type: "boolean",
      description: "Don't colourise log output"
    },
    "--skip-entry-point": {
      type: "boolean",
      description: "Don't add code to automatically invoke Main when browserifying."
    },
    "--then": {
      type: "string",
      description: "Run a shell command after the operation finishes. Useful with `--watch`."
    }
  }).validate(function(result) {
    if (result.help) {
      this.printUsage();
      printCommands();
      process.exit(0);
    }
    if (result.monochrome) {
      log.mono(true);
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
  }).usage("Usage: pulp <command> [options]").parse();

  if (!args.buildPath) args.buildPath = "./output";

  if (!args.main) args.main = "Main";

  if (args.command.noProject) {
    args.command.action(args, done(args));
  } else {
    require("./project")(args, function(err, pro) {
      if (err) {
        log.error("ERROR:", err.message);
        process.exit(1);
      } else {
        if (args.watch) {
          require("./watch")();
        } else {
          args.command.action(pro, args, done(args));
        }
      }
    });
  }
}
