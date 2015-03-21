#!/usr/bin/env node

var args = require("./args");
var log = require("./log");
var merge = require("merge");

var globals = [
  args.option(
    "bowerFile", ["--bower-file", "-b"], args.file,
    "Read this bower.json file instead of autodetecting it."
  ),
  args.option(
    "watch", ["--watch", "-w"], args.flag,
    "Watch source directories and re-run command if something changes."
  ),
  args.option(
    "monochrome", ["--monochrome"], args.flag,
    "Don't colourise log output."
  ),
  args.option(
    "then", ["--then"], args.string,
    "Run a shell command after the operation finishes. Useful with `--watch`."
  )
];

var buildArgs = [
  args.option(
    "buildPath", ["--build-path", "-o"], args.string,
    "Path for compiler output.", "./output"
  ),
  args.option(
    "main", ["--main", "-m"], args.string,
    "Application's entry point.", "Main"
  )
];

var commands = [
  args.command(
    "init", "Generate an example PureScript project.", function() {
      return require("./init").apply(this, arguments);
    }, [
      args.option(
        "force", ["--force"], args.flag,
        "Overwrite any project found in the current directory."
      )
    ]
    // noProject: true
  ),
  args.command(
    "dep", "Invoke Bower for package management.", function() {
      return require("./bower").apply(this, arguments);
    }
  ),
  args.command(
    "build", "Build the project.", function() {
      return require("./build").apply(this, arguments);
    }, buildArgs
  ),
  args.command(
    "test", "Run project tests.", function() {
      return require("./test").apply(this, arguments);
    }, [
      args.option(
        "main", ["--main", "-m"], args.string,
        "Test entry point.", "Test.Main"
      ),
      args.option(
        "testRuntime", ["--runtime", "-r"], args.string,
        "Run test script using this command instead of Node."
      )
    ].concat([buildArgs[0]])
  ),
  args.command(
    "browserify", "Produce a deployable bundle using Browserify.",
    function() {
      return require("./browserify").apply(this, arguments);
    }, buildArgs.concat([
      args.option(
        "to", ["--to", "-t"], args.string,
        "Output file name for bundle (stdout if not specified)."
      ),
      args.option(
        "transform", ["--transform"], args.string,
        "Apply a Browserify transform."
      ),
      args.option(
        "sourceMap", ["--source-map"], args.string,
        "Generate source maps."
      ),
      args.option(
        "optimise", ["--optimise", "-O"], args.flag,
        "Perform dead code elimination."
      ),
      args.option(
        "skipEntryPoint", ["--skip-entry-point"], args.flag,
        "Don't add code to automatically invoke Main."
      ),
      args.option(
        "skipCompile", ["--skip-compile"], args.flag,
        "Don't run `pulp build` before browserifying."
      )
    ])
  ),
  args.command(
    "run", "Compile and run the project.", function() {
      return require("./run").apply(this, arguments);
    }, buildArgs
  ),
  args.command(
    "docs", "Generate project documentation.", function() {
      return require("./docs").apply(this, arguments);
    }, [
      args.option(
        "withTests", ["--with-tests", "-t"], args.flag,
        "Include tests."
      ),
      args.option(
        "withDeps", ["--with-deps", "-d"], args.flag,
        "Include external dependencies."
      )
    ]
  ),
  args.command(
    "psci", "Launch a PureScript REPL configured for the project.",
    function() {
      return require("./psci").apply(this, arguments);
    }
  )
];

var opts = args.parse(globals, commands, process.argv.slice(2));

if (args.isError(opts)) {
  if (!opts.help) {
    var ansi = require("ansi")(process.stderr);
    ansi.red().bold().write("Error:").reset().write(" ");
    console.error(opts.message, "\n");
  }
  args.help(globals, commands, opts.context, process.stderr);
  process.exit(1);
}

opts = merge(opts.opts, opts.commandOpts, {
  command: opts.command,
  remainder: opts.remainder
});

function done(opts) {
  return function doneFunc(err) {
    if (err) {
      log.error("ERROR:", err.message);
      process.exit(1);
    } else {
      if (opts.then) {
        require("./shell")(opts.then, done({}));
      } else {
        process.exit(0);
      }
    }
  };
}

if (opts.monochrome) {
  log.mono(true);
}

var command = opts.command;

if (command.name === "init") {
  command.action(opts, done(opts));
} else {
  require("./project")(opts, function(err, pro) {
    if (err) {
      log.error("ERROR:", err.message);
      process.exit(1);
    } else {
      if (opts.watch) {
        require("./watch")();
      } else {
        command.action(pro, opts, done(opts));
      }
    }
  });
}
