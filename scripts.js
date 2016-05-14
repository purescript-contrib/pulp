// Pulp build scripts
//
// Sometimes we need to use one version of the PureScript compiler to compile
// Pulp with, and a different version to test with. For example, this is
// usually necessary after a compiler release with breaking changes, because
// the PureScript libraries which Pulp depends on will not yet have been
// updated.
//
// This script enables us to switch versions of psc for different build
// scripts, via psvm (https://github.com/ThomasCrvsr/psvm-js).
const child_process = require("child_process");
const mkdirp = require("mkdirp");
const path = require("path");

const scripts = {
  "lint": "jshint src",
  "compile": "psa -c -f \"src/**/*.js\" -f \"bower_components/purescript-*/src/**/*.js\" \"src/**/*.purs\" \"bower_components/purescript-*/src/**/*.purs\" --censor-lib --censor-codes=ImplicitImport",
  "bundle": "psc-bundle \"output/*/*.js\" --output pulp.js --module Main --main Main",
  "test": "mocha test-js --compilers js:babel/register",
};

const subcommand = process.argv[2];
if (!subcommand) {
  console.error("Expected a subcommand.");
  console.error("Usage: ");
  console.error("  node scripts.js [build|test|prepublish]");
  process.exit(1);
}

switch (subcommand) {
  case "build":
    build();
    break;
  case "test":
    test();
    break;
  case "prepublish":
    prepublish();
    break;
  default:
    throw new Error("Unrecognised subcommand: " + subcommand);
    break;
}

function build() {
  spawnSync("psvm use " + getConfig("psc_build_version"));
  ["lint", "compile", "bundle"].forEach(execScript);
}

function test() {
  spawnSync("psvm use " + getConfig("psc_test_version"));
  execScript("test");
}

function prepublish() {
  ensurePscVersionsInstalled();
  spawnSync("bower install");
  build();
}

// Construct and return an environment to run subcommands in.
//
// We set the PSVM_HOME environment variable to ensure that psvm only installs
// stuff in the current directory.
function getSubcommandEnv() {
  var env = Object.assign({}, process.env);
  env.PSVM_HOME = path.join(__dirname, ".psvm");
  prependPath(env, path.join(env.PSVM_HOME, "current", "bin"));
  return env;
}

function execScript(script) {
  spawnSync(scripts[script]);
}


function prependPath(env, newDirectory) {
  const pathVar = process.platform === "win32" ? "Path" : "PATH";
  env[pathVar] = newDirectory + path.delimiter + env[pathVar];
}

function ensurePscVersionsInstalled() {
  var subcommandEnv = getSubcommandEnv();

  // Workaround for https://github.com/ThomasCrvsr/psvm-js/issues/13
  mkdirp.sync(path.join(subcommandEnv.PSVM_HOME, 'versions'));

  const newlines = /\r?\n/g;
  installedVersions = spawnSync("psvm ls", { quiet: true })
    .split(newlines).map((s) => s.trim());

  const versions = ['psc_build_version', 'psc_test_version'].map(getConfig);

  versions.forEach((version) => {
    if (!installedVersions.includes(version)) {
      spawnSync("psvm install " + version);
    }
  });
}

function getConfig(variable) {
  var result = process.env['npm_package_config_' + variable];
  if (result) {
    return result;
  } else {
    throw new Error("Missing configuration: " + variable);
  }
}

function spawnSync(command, opts) {
  opts = opts || {};
  console.log(">> " + command);
  var result = child_process.spawnSync(command, {
    cwd: process.cwd(),
    env: getSubcommandEnv(),
    stdio: ["inherit", opts.quiet ? "pipe" : "inherit", "inherit"],
    encoding: "utf-8",
    shell: true
  });

  if (result.status !== 0) {
    throw new Error("shell command exited with: " + result.status);
  }

  return result.stdout;
}
