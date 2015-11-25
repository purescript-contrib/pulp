import fs from "fs";
import path from "path";
import run from "./sh";

const hello = "Hello sailor!";
const test = "You should add some tests.";
const doc = "## Module Main\n\n#### `main`\n\n``` purescript\nmain :: forall e. Eff (console :: CONSOLE | e) Unit\n```";
const bowerMissing = "* ERROR: No bower.json found in current or parent directories. Are you in a PureScript project?";

describe("integration tests", function() {
  this.timeout(60000);

  it("errors when bower.json is missing", run(function*(sh, pulp, assert) {
    const [_, err] = yield pulp("build", null, { expectedExitCode: 1 });
    assert.equal(err.trim(), bowerMissing);
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
});
