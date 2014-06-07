#!/usr/bin/env node

var fs = require("fs");
var child = require("child_process");
var glob = require("glob");
var u = require("underscore");
var bower = require("bower");
var standardRenderer = require("bower/lib/renderers/StandardRenderer");
var yargs = require("yargs")
      .usage("Usage: $0 <command>");
var argv = yargs.argv;

var cmds = {
  "install": install,
  "build": build,
  "test": test
};

function help() {
  yargs.showHelp();
  var key;
  console.error("Available commands:");
  for (key in cmds) {
    console.error("  " + key);
  }
}

var pro;
try {
  pro = JSON.parse(fs.readFileSync("bower.json", "utf-8"));
} catch (e) {
  console.error("ERROR: No bower.json found in current directory. Too stupid to handle this anon.\n");
  help();
  process.exit(1);
}

function install(cb) {
  var log = new standardRenderer("update", {});
  var deps = u.keys(pro.devDependencies).concat(u.keys(pro.dependencies));
  bower.commands.install(deps).on("end", function(res) {
    cb();
  }).on("log", function(data) {
    log.log(data);
  }).on("error", function(err) {
    console.error(err);
    process.exit(1);
  });
}

function exec(cmd, args, cb) {
  child.spawn(cmd, args, {
    stdio: "inherit"
  }).on("exit", function(code, signal) {
    if (code > 0) {
      console.error("Subcommand terminated with error code", code);
      process.exit(code);
    }
    cb();
  });
}

function psc(match, args, cb) {
  glob(match, {}, function(err, files) {
    if (err) {
      console.error(err);
      process.exit(1);
    }
    exec("psc", args.concat(files), cb);
  });
}

function build(cb) {
  psc("{src,bower_components}/**/*.purs", ["--output", "out.js"], cb);
}

function test(cb) {
  psc("{test,src,bower_components}/**/*.purs", ["--output=test.js", "--main=Main"], function() {
    exec("node", ["test.js"], cb);
  });
}

var cmd = cmds[argv._[0]];
if (!cmd) {
  help();
  process.exit(1);
} else {
  cmd(function() {
    process.exit(0);
  });
}
