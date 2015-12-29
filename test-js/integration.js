import fs from "fs";
import path from "path";
import run from "./sh";
import semver from "semver";
import touch from "touch";

const hello = "Hello sailor!";
const test = "You should add some tests.";
const docLine1 = "## Module Main"
const bowerMissing = "* ERROR: No bower.json found in current or parent directories. Are you in a PureScript project?";
const initWithoutForce = f => new RegExp('\\* ERROR: Found .*'+f+': There\'s already a project here. Run `pulp init --force` if you\'re sure you want to overwrite it.');
const filesToOverwrite = ['./bower.json', './.gitignore', 'src/Main.purs', 'test/Main.purs'];
const testDocLine1 = "## Module Test.Main"
const consoleDocLine1 = "## Module Control.Monad.Eff.Console"
const buildHelp = ["Command: build", "Build the project."]
const docsHelp = ["Command: docs", "Generate project documentation."]
const skipped = "* Project unchanged; skipping build step."

const newlines = /\r?\n/g

function sleep(ms) {
  return new Promise((resolve, reject) => {
    setTimeout(resolve, ms);
  });
}

describe("integration tests", function() {
  this.timeout(60000);

  it("pulp --version", run(function*(sh, pulp, assert) {
    const [out] = yield pulp("--version");
    assert.ok(semver.valid(out.trim()), out + " is not a valid version.");
  }));

  it("pulp -v", run(function*(sh, pulp, assert) {
    const [out] = yield pulp("-v");
    assert.ok(semver.valid(out.trim()), out + " is not a valid version.");
  }));

  it("errors when bower.json is missing", run(function*(sh, pulp, assert) {
    const [_, err] = yield pulp("build", null, { expectedExitCode: 1 });
    assert.equal(err.trim(), bowerMissing);
  }));

  it("pulp build --help", run(function*(sh, pulp, assert) {
    const [_, err] = yield pulp("build --help");
    buildHelp.forEach(function(h) {
      assert.ok(err.indexOf(h) > -1,
          "output did not contain '" + h + "'\noutput was:\n" + err)
    });
  }));

  it("pulp docs --help", run(function*(sh, pulp, assert) {
    const [_, err] = yield pulp("docs --help");
    docsHelp.forEach(function(h) {
      assert.ok(err.indexOf(h) > -1,
          "output did not contain '" + h + "'\noutput was:\n" + err)
    });
  }));

  it("refuses to init without --force", run(function*(sh, pulp, assert, temp) {
    for (var idx in filesToOverwrite) {
      let file = path.join(temp, filesToOverwrite[idx])
        , dir  = path.dirname(file)
      if (!fs.existsSync(dir)) { fs.mkdirSync(dir); }
      touch.sync(file);
      const [_, err] = yield pulp("init", null, { expectedExitCode: 1 });
      assert.match(err.trim(), initWithoutForce(path.basename(file)));
      const rm = process.platform === "win32" ? "del" : "rm"
      fs.unlinkSync(file);
    }
  }));

  it("init overwrites existing files with --force", run(function*(sh, pulp, assert, temp) {
    for (var idx in filesToOverwrite) {
      let file = path.join(temp, filesToOverwrite[idx]);
      fs.writeFileSync(file, "hello");
      yield pulp("init --force");
      const out = fs.readFileSync(file);
      assert.notEqual(out, "hello");
    }
  }));

  it("pulp build", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp("build");

    assert.exists(path.join("output", "Main", "index.js"));
  }));

  it("finds bower.json in parent directories", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp("build", null, { cwd: path.join(temp, "src") });

    assert.exists(path.join("output", "Main", "index.js"));
  }));

  it("handles .pulp-cache already existing", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield sh("mkdir .pulp-cache");
    yield pulp("build");
  }));

  it("pulp build --include", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield sh("mkdir extras");

    fs.writeFileSync(path.join(temp, "extras", "Extras.purs"),
                     "module Extras where\n\nfoo = 1");

    yield pulp("build --include extras");

    assert.exists(path.join("output", "Extras", "index.js"));
  }));

  it("pulp run", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("run");
    assert.equal(out.trim(), hello);
  }));

  it("pulp --bower-file FILE run", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    fs.renameSync(path.join(temp, "bower.json"), path.join(temp, "lol.json"));
    const [out] = yield pulp("--bower-file lol.json run");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build --to", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build --to out.js");
    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build -O", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [src] = yield pulp("build -O");
    const [out] = yield sh("node", src);
    assert.equal(out.trim(), hello);
  }));

  it("pulp build -O --to", run(function*(sh, pulp, assert) {
    // Should be identical to `pulp build --to`.
    yield pulp("init");
    yield pulp("build -O --to out.js");
    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build -O --src-path alt", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    fs.renameSync(path.join(temp, "src"), path.join(temp, "alt"));
    const [src] = yield pulp("build -O --src-path alt");
    const [out] = yield sh("node", src);
    assert.equal(out.trim(), hello);
  }));

  it("pulp test", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("test");
    assert.equal(out.trim(), test);
  }));

  it("pulp test --test-path alt", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    fs.renameSync(path.join(temp, "test"), path.join(temp, "alt"));
    const [out] = yield pulp("test --src-path alt");
    assert.equal(out.trim(), test);
  }));

  it("pulp browserify", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [src] = yield pulp("browserify");
    const [out] = yield sh("node", src);
    assert.equal(out.trim(), hello);
  }));

  it("pulp browserify --to", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("browserify --to out.js");
    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp browserify -O", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [src] = yield pulp("browserify -O");
    const [out] = yield sh("node", src);
    assert.equal(out.trim(), hello);
  }));

  it("pulp browserify -O --to", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("browserify -O --to out.js");
    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp docs", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs");
    assert.file("docs/Main.md", (c) =>
      assert.equal(c.split(newlines)[0], docLine1));
  }));

  it("pulp docs --with-tests", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs --with-tests");
    assert.file("docs/Test/Main.md", (c) =>
      assert.equal(c.split(newlines)[0], testDocLine1));
  }));

  it("pulp docs --with-dependencies", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs --with-dependencies");
    assert.file("docs/Control/Monad/Eff/Console.md", (c) =>
      assert.equal(c.split(newlines)[0], consoleDocLine1));
  }));

  it("pulp psci includes dependencies", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("psci");

    assert.file(".psci", (c) => true);
    const [out] = yield pulp("psci", "import Prelude\n\"hello, \" ++ \"world\"");
    assert.ok(out.indexOf("\"hello, world\"") > -1,
      "output did not contain \"hello, world\"");
  }));

  it("pulp psci includes local files", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("psci");

    assert.file(".psci", (c) => true);
    const [out] = yield pulp("psci", "import Main as Main\nMain.main");
    assert.ok(out.indexOf(hello) > -1,
      "output did not contain \"" + hello + "\"");
  }));

  it("pulp --before something build", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    // In reality, this is likely to be a "--before clear" or something, but
    // that's nightmarish to actually test.
    touch.sync(path.join(temp, "before.txt"))
    const mv = process.platform === "win32" ? "rename" : "mv"
    yield pulp(`--before "${mv} before.txt after.txt" build --to out.js`);

    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
    assert.ok(fs.existsSync(path.join(temp, "after.txt")),
      "test file before.txt was not found as after.txt");
  }));

  it("pulp --then something build", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const mv = process.platform === "win32" ? "rename" : "mv"
    yield pulp(`--then "${mv} out.js out2.js" build --to out.js`);

    const [out] = yield sh("node out2.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp --else something build", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    // Deliberately cause a build failure.
    const mainPath = path.join(temp, 'src', 'Main.purs')
    fs.writeFileSync(
      mainPath, fs.readFileSync(mainPath).toString().concat("\ninvalidThing")
    );

    touch.sync(path.join(temp, "before.txt"))
    const mv = process.platform === "win32" ? "rename" : "mv"
    const [_, err] = yield pulp(
      `--else "${mv} before.txt afterFailed.txt" build --to out.js`,
      null, { expectedExitCode: 1 }
    );
    assert.match(err.trim(), /Unable to parse/); // Expected error
    assert.exists(path.join(temp, "afterFailed.txt")); // --else has run
  }));

  it("pulp build skips when possible", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build");
    const [_, err] = yield pulp("build");
    assert.equal(err.trim(), skipped);
  }));

  it("changed args force rebuild", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp("build");
    const [_, err] = yield pulp("build --to out.js");
    assert.notEqual(err.trim(), skipped);
  }));

  it("changed files force rebuild", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp("build");
    yield sleep(1000);
    touch.sync(path.join(temp, "src", "Main.purs"));
    const [_, err] = yield pulp("build");
    assert.notEqual(err.trim(), skipped);
  }));

  it("--force flag forces rebuild", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build --force");
    const [_, err] = yield pulp("build --force");
    assert.notEqual(err.trim(), skipped);
  }));
});
