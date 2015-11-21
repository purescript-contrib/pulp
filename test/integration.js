import fs from "fs";
import path from "path";
import run from "./sh";

const hello = "Hello sailor!";
const test = "You should add some tests.";
const doc = "## Module Main\n\n#### `main`\n\n``` purescript\nmain :: forall e. Eff (console :: CONSOLE | e) Unit\n```";

describe("integration tests", function() {
  this.timeout(60000);

  it("pulp run", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("run");
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
    fs.mkdirSync(path.join(temp, "alt"));
    fs.writeFileSync(path.join(temp, "bower.json"), "{}");
    fs.writeFileSync(path.join(temp, "alt", "Alt.purs"), "module Alt where\n");
    const [src] = yield pulp("build -O --src-path alt");
    assert.ok(src.indexOf("var PS") !== -1);
  }));

  it("pulp test", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("test");
    assert.equal(out.trim(), test);
  }));

  it("pulp test --test-path alt", run(function*(sh, pulp, assert, temp) {
    var test = "module Test.Main where\nimport Prelude\nimport Control.Monad.Eff\nmain :: Eff () Unit\nmain = pure unit";
    yield pulp("init");
    fs.mkdirSync(path.join(temp, "alt"));
    fs.writeFileSync(path.join(temp, "bower.json"), "{}");
    fs.writeFileSync(path.join(temp, "alt", "Test.purs"), test);
    const [out] = yield pulp("test --test-path alt");
    assert.equal(out, "");
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
