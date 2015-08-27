var log = require("./log");
var path = require("path");
var fs = require("fs");
var bower = require("./bower");

function bowerFile(name) {
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
}

var gitignore = [
  "/bower_components/",
  "/node_modules/",
  "/output/",
  "/.psci*",
  "/src/.webpack.js"
].join("\n") + "\n";

var mainFile = [
  "module Main where",
  "",
  "import Prelude",
  "import Control.Monad.Eff.Console",
  "",
  "main = do",
  "  log \"Hello sailor!\""
].join("\n") + "\n";

var testFile = [
  "module Test.Main where",
  "",
  "import Prelude",
  "import Control.Monad.Eff.Console",
  "",
  "main = do",
  "  log \"You should add some tests.\""
].join("\n") + "\n";

function init(callback) {
  var name = path.basename(process.cwd());
  log("Generating project skeleton in", process.cwd());
  fs.writeFileSync(path.join(process.cwd(), ".gitignore"), gitignore, "utf-8");
  fs.writeFileSync(path.join(process.cwd(), "bower.json"), bowerFile(name), "utf-8");
  if (!fs.existsSync("src")) {
    fs.mkdirSync("src");
  }
  fs.writeFileSync(path.join(process.cwd(), "src", "Main.purs"), mainFile, "utf-8");
  if (!fs.existsSync("test")) {
    fs.mkdirSync("test");
  }
  fs.writeFileSync(path.join(process.cwd(), "test", "Main.purs"), testFile, "utf-8");
  bower.launchBower(["update"], callback);
}

module.exports = function(args, callback) {
  if (fs.existsSync(path.join(process.cwd(), "bower.json")) && !args.force) {
    callback(new Error("There's already a project here. Run `pulp init --force` if you're sure you want to overwrite it."));
  } else {
    init(callback);
  }
};
