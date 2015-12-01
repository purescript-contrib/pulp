import fs from "fs";
import path from "path";
import run from "./sh";
import semver from "semver";

const hello = "Hello sailor!";
const test = "You should add some tests.";
const doc = "## Module Main\n\n#### `main`\n\n``` purescript\nmain :: forall e. Eff (console :: CONSOLE | e) Unit\n```";
const bowerMissing = "* ERROR: No bower.json found in current or parent directories. Are you in a PureScript project?";
const initWithoutForce = "* ERROR: There's already a project here. Run `pulp init --force` if you're sure you want to overwrite it."
const testDocLine1 = "## Module Test.Main"
const consoleDocLine1 = "## Module Control.Monad.Eff.Console"
const buildHelp = ["Command: build", "Build the project."]
const docsHelp = ["Command: docs", "Generate project documentation."]
const skipped = "* Project unchanged; skipping build step."

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

  it("refuses to init without --force", run(function*(sh, pulp, assert) {
    yield sh("touch bower.json");
    const [_, err] = yield pulp("init", null, { expectedExitCode: 1 });
    assert.equal(err.trim(), initWithoutForce);
  }));

  it("init overwrites existing files with --force", run(function*(sh, pulp, assert) {
    yield sh("echo 'hello' > bower.json");
    yield pulp("init --force");
    const [out] = yield sh("cat bower.json");
    assert.notEqual(out.trim(), "hello");
  }));

  it("pulp build", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build");
    yield sh("test -f output/Main/index.js");
  }));

  it("handles .pulp-cache already existing", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield sh("mkdir .pulp-cache");
    yield pulp("build");
  }));

  it("pulp build --include", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield sh("mkdir extras");
    yield sh("echo 'module Extras where\n\nfoo = 1' > extras/Extras.purs");
    yield pulp("build --include extras");
    yield sh("test -f output/Extras/index.js");
  }));

  it("pulp run", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("run");
    assert.equal(out.trim(), hello);
  }));

  it("pulp --bower-file FILE run", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield sh("mv bower.json lol.json");
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
    yield sh("mv src alt");
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
    yield sh("mv test alt");
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
      assert.equal(c.trim(), doc));
  }));

  it("pulp docs --with-tests", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs --with-tests");
    assert.file("docs/Test/Main.md", (c) =>
      assert.equal(c.split("\n")[0], testDocLine1));
  }));

  it("pulp docs --with-dependencies", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs --with-dependencies");
    assert.file("docs/Control/Monad/Eff/Console.md", (c) =>
      assert.equal(c.split("\n")[0], consoleDocLine1));
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

  it("pulp --then something build", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("--then 'mv out.js out2.js' build --to out.js");

    const [out] = yield sh("node out2.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build skips when possible", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build");
    const [_, err] = yield pulp("build");
    assert.equal(err.trim(), skipped);
  }));

  it("changed args force rebuild", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build");
    const [_, err] = yield pulp("build --to out.js");
    assert.notEqual(err.trim(), skipped);
  }));

  it("changed files force rebuild", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build");
    yield sh("touch src/Main.purs");
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
