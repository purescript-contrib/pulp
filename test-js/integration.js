import fs from "fs-promise";
import path from "path";
import run from "./sh";
import semver from "semver";
import touch from "touch";
import which from "which";

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

function resolvePath(cmd) {
  return new Promise((resolve, reject) => {
    which(cmd, (err, res) => err ? reject(err) : resolve(res));
  });
}

function sleep(ms) {
  return new Promise((resolve, reject) => {
    setTimeout(resolve, ms);
  });
}

function windowsify(script) {
  return process.platform === "win32" ? `${script}.bat` : script;
}

describe("integration tests", function() {
  this.timeout(60000);

  it("pulp --version", run(function*(sh, pulp, assert) {
    const [out] = yield pulp("--version");
    assert.ok(semver.valid(out.split(/\s/)[2]), out + " is not a valid version.");
  }));

  it("pulp -v", run(function*(sh, pulp, assert) {
    const [out] = yield pulp("-v");
    assert.ok(semver.valid(out.split(/\s/)[2]), out + " is not a valid version.");
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
        , dir  = path.dirname(file);
      if (!(yield fs.exists(dir))) {
        yield fs.mkdir(dir);
      }
      touch.sync(file);
      const [_, err] = yield pulp("init", null, { expectedExitCode: 1 });
      assert.match(err.trim(), initWithoutForce(path.basename(file)));
      const rm = process.platform === "win32" ? "del" : "rm"
      yield fs.unlink(file);
    }
  }));

  it("init overwrites existing files with --force", run(function*(sh, pulp, assert, temp) {
    for (var idx in filesToOverwrite) {
      let file = path.join(temp, filesToOverwrite[idx]);
      yield fs.writeFile(file, "hello");
      yield pulp("init --force");
      const out = yield fs.readFile(file);
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

    yield fs.writeFile(path.join(temp, "extras", "Extras.purs"),
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
    yield fs.rename(path.join(temp, "bower.json"), path.join(temp, "lol.json"));
    const [out] = yield pulp("--bower-file lol.json run");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build --to", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build --to out.js");
    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build --skip-entry-point --to", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp(`--then "echo module.exports = PS >> out.js" build --skip-entry-point --to out.js`);
    const [out] = yield sh(`node -e "var o = require('./out'); o.Main.main();"`);
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
    yield fs.rename(path.join(temp, "src"), path.join(temp, "alt"));
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
    yield fs.rename(path.join(temp, "test"), path.join(temp, "alt"));
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
    assert.match(out, /hello, world/);
  }));

  it("pulp psci includes local files", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("psci");

    assert.file(".psci", (c) => true);
    const [out] = yield pulp("psci", "import Main as Main\nMain.main");
    assert.match(out, new RegExp(hello));
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
    assert.ok(yield fs.exists(path.join(temp, "after.txt")),
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
    yield fs.writeFile(
      mainPath,
      (yield fs.readFile(mainPath)).toString().concat("\ninvalidThing")
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

  it("pulp test --runtime", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("test --runtime cat");
    const [out2] = yield sh("node", out);
    assert.equal(out2.trim(), test);
  }));

  it("pulp chooses psa over psc when available", run(function*(sh, pulp, assert, temp) {
    const node = yield resolvePath("node");
    const prog = (name) => `
if (process.argv[2] === "--version") {
  process.stdout.write("1.0.0.0\\n");
} else {
  process.stderr.write("assert ${name}\\n");
}
`;
    const p = path.resolve(temp, "bin");
    const psc = path.resolve(p, windowsify("psc"));
    const psa = path.resolve(p, windowsify("psa"));
    const pscJs = path.resolve(p, "psc.js");
    const psaJs = path.resolve(p, "psa.js");
    const args = process.platform === "win32" ? "%*" : "$@";

    yield fs.mkdir(p);
    yield fs.writeFile(psc, `"${node}" "${pscJs}" ${args}`, "utf-8");
    yield fs.chmod(psc, 0o755);
    yield fs.writeFile(pscJs, prog("psc"), "utf-8");

    yield pulp("init");

    const [out1, err1] = yield pulp("build", undefined, {path: p});
    assert.match(err1, /assert psc/);

    yield fs.writeFile(psa, `${node} ${psaJs} ${args}`, "utf-8");
    yield fs.chmod(psa, 0o755);
    yield fs.writeFile(psaJs, prog("psa"), "utf-8");

    const [out2, err2] = yield pulp("build --force", undefined, {path: p});
    assert.match(err2, /assert psa/);

    const [out3, err3] = yield pulp("build --force --no-psa", undefined, {path: p});
    assert.match(err3, /assert psc/);
  }));
});
