import fs from "fs-promise";
import co from "co";
import path from "path";
import run from "./sh";
import semver from "semver";
import touch from "touch";
import which from "which";

const hello = "Hello sailor!";
const test = "You should add some tests.";
const docLine1 = "## Module Main";
const bowerMissing = "* ERROR: No bower.json or psc-package.json found in current or parent directories. Are you in a PureScript project?";
const initWithoutForce = f => new RegExp('\\* ERROR: Found .*'+f+': There\'s already a project here. Run `pulp init --force` if you\'re sure you want to overwrite it.');
const filesToOverwrite = ['./bower.json', './.gitignore', 'src/Main.purs', 'test/Main.purs'];
const testDocLine1 = "## Module Test.Main";
const consoleDocLine1 = "## Module Control.Monad.Eff.Console";
const buildHelp = ["Command: build", "Build the project."];
const docsHelp = ["Command: docs", "Generate project documentation."];
const skipped = "* Project unchanged; skipping build step.";

const newlines = /\r?\n/g;

const testBowerJson = JSON.stringify({
  "name": "test-package-for-pulp-tests",
  "license": "MIT",
  "ignore": [
    "**/.*",
    "node_modules",
    "bower_components",
    "output"
  ],
  "repository": {
    "type": "git",
    "url": "git://github.com/not-real/not-real.git"
  },
  "dependencies": {
    "purescript-console": "^0.1.0"
  }
});

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
  return process.platform === "win32" ? `${script}.cmd` : script;
}

function assertThrows(promise) {
  return promise.then(
      () => { throw new Error("Expected the promise to throw") },
      () => {}
    );
}

function notWindowsIt(name, fn) {
  if (process.platform === "win32") {
    it("Skipping: " + name + " (since we are on windows)",
        function() { return true; });
  } else {
    it(name, fn);
  }
}

const psaHelper = co.wrap(function*(temp) {
  const node = yield resolvePath("node");
  const prog = (name, version) => `
if (process.argv[2] === "--version") {
  process.stdout.write("${version}\\n");
} else {
  process.stderr.write("assert ${name}\\n");
  process.stderr.write(process.argv.join(" ") + "\\n");
}
`;

  const p = path.resolve(temp, "bin");
  yield fs.mkdir(p);

  const writeProg = co.wrap(function* (name, version) {
    const fullProgPath = path.resolve(p, windowsify(name));
    const progJs = path.resolve(p, name + ".js");
    const echoOff = process.platform === "win32" ? "@echo off\r\n" : "";
    const args = process.platform === "win32" ? "%*" : "$@";

    yield fs.writeFile(fullProgPath,
        `${echoOff}"${node}" "${progJs}" ${args}`, "utf-8");
    yield fs.chmod(fullProgPath, 0o755);
    yield fs.writeFile(progJs, prog(name, version), "utf-8");
  });

  return {
    binPath: p,
    writeProg
  };
});

function setupPackage(cwd, sh) {
  return sh("git init")
    .then(() => fs.writeFile(path.join(cwd, "LICENSE"),
                             "do whatever u want lol",
                             {encoding: 'utf8'}))
    .then(() => sh("git add ."))
    .then(() => sh("git config user.name \"Test User\""))
    .then(() => sh("git config user.email test@test.com"))
    .then(() => sh("git commit -m \"initial commit\""))
    .then(() => sh("git tag v1.0.0"));
}

function* createModule(sh, temp, name) {
  yield sh("mkdir " + name);

  yield fs.writeFile(path.join(temp, name, name + ".purs"),
                     "module " + name + " where\n\nfoo = 1");

  return path.join("output", name, "index.js");
}

describe("integration tests", function() {
  // This is, unfortunately, required, as CI is horrendously slow.
  this.timeout(90000);

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

  it("Bower has precedence over psc-package", run(function*(sh, pulp, assert) {
    yield pulp("init");

    // Create psc-package.json without installing any dependencies
    yield sh("psc-package init");

    // In the presence of both bower.json and psc-package.json, pulp should default to Bower
    yield pulp("build");
    assert.exists(path.join("output", "Main", "index.js"));

    // The --psc-package flag can override that, in this case the build should fail because
    // we haven't installed any dependencies with psc-package
    assertThrows(pulp("--psc-package build"));
  }));
  
  ["build --help", "build -h"].forEach((cmdline) => {
    it("pulp " + cmdline, run(function*(sh, pulp, assert) {
      const [_, err] = yield pulp(cmdline);
      buildHelp.forEach(function(h) {
        assert.ok(err.indexOf(h) > -1,
            "output did not contain '" + h + "'\noutput was:\n" + err)
      });
    }));
  });

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

  it("pulp build with psc-package", run(function*(sh, pulp, assert, temp) {
    yield pulp("--psc-package init");
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
    var extras = yield createModule(sh, temp, "Extras");

    yield pulp("build --include Extras");

    assert.exists(extras);
  }));

  notWindowsIt("handles empty string after --include", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp("build --include ''");
  }));

  it("handles consecutive delimiters for --include", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    var extras = yield createModule(sh, temp, "Extras");
    var extras2 = yield createModule(sh, temp, "Extras2");

    var includeArg = ["Extras", "", "Extras2"].join(path.delimiter);
    yield pulp("build --include " + includeArg);

    assert.exists(extras);
    assert.exists(extras2);
  }));

  it("handles delimiter as last character for --include", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    var extras = yield createModule(sh, temp, "Extras");

    yield pulp("build --include Extras" + path.delimiter);

    assert.exists(extras);
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

  it("pulp build --to creates parent directories", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("build --to nonexistent/out.js");
    const [out] = yield sh("node nonexistent/out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp build --skip-entry-point --to", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp(`--then "echo module.exports = PS >> out.js" build --skip-entry-point --to out.js`);
    const [out] = yield sh(`node -e "var o = require('./out'); o.Main.main();"`);
    assert.equal(out.trim(), hello);
  }));

  it("pulp build --to checks entry point module exists", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    const [_, err] = yield pulp("build --to out.js --main DoesntExist", null, { expectedExitCode: 1 });
    assert.match(err, /Entry point module \(DoesntExist\) not found/);
  }));

  it("pulp build --to checks entry point module exports `main`", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    const mainPath = path.join(temp, 'src', 'Main.purs');
    yield fs.writeFile(
      mainPath,
      "module Main where\nmain2 = 0\n"
    );

    const [_, err] = yield pulp("build --to out.js", null, { expectedExitCode: 1 });
    assert.match(err, /does not export a `main` value/);
  }));

  it("pulp build --to checks `main` has appropriate type", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    const mainPath = path.join(temp, 'src', 'Main.purs');
    yield fs.writeFile(
      mainPath,
      "module Main where\nmain = 0\n"
    );

    const [_, err] = yield pulp("build --to out.js", null, { expectedExitCode: 1 });
    assert.match(err, /is not of type Control.Monad.Eff.Eff/);
  }));

  it("pulp build --check-main-type Prim.Int", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    const mainPath = path.join(temp, 'src', 'Main.purs');
    yield fs.writeFile(
      mainPath,
      "module Main where\nmain = 0\n"
    );

    yield pulp("build --to out.js --check-main-type Prim.Int");
    assert.exists("output/Main/index.js");
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

  it("pulp test --test-path test2 -- --something-node-wouldnt-like", run(function*(sh, pulp, assert, temp) {
    const newTest = path.join(temp, "test2");
    yield pulp("init");
    yield fs.mkdir(newTest);
    yield fs.copy(path.resolve(__dirname, "resources/Main.purs"), path.join(newTest, "Main.purs"));
    yield fs.copy(path.resolve(__dirname, "resources/Main.js"), path.join(newTest, "Main.js"));
    const [out] = yield pulp("test --test-path test2 -- --something-node-wouldnt-like");
    assert.equal(out.trim(), "--something-node-wouldnt-like");
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

  it("pulp browserify --skip-compile", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield fs.rename(path.join(temp, "src"), path.join(temp, "alt"));
    yield pulp("build --src-path alt");
    yield pulp("browserify --to out.js --skip-compile");
    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp docs", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs");
    assert.file("generated-docs/Main.md", (c) =>
      assert.equal(c.split(newlines)[0], docLine1));
  }));

  it("pulp docs --with-tests", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs --with-tests");
    assert.file("generated-docs/Test/Main.md", (c) =>
      assert.equal(c.split(newlines)[0], testDocLine1));
  }));

  it("pulp docs --with-dependencies", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("docs --with-dependencies");
    assert.file("generated-docs/Control/Monad/Eff/Console.md", (c) =>
      assert.equal(c.split(newlines)[0], consoleDocLine1));
  }));

  it("pulp docs --with-dependencies with psc-package", run(function*(sh, pulp, assert) {
    yield pulp("--psc-package init");
    yield pulp("docs --with-dependencies");
    assert.file("generated-docs/Control/Monad/Eff/Console.md", (c) =>
      assert.equal(c.split(newlines)[0], consoleDocLine1));
  }));

  it("pulp psci includes dependencies", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("psci");

    const [out] = yield pulp("psci", "import Prelude\n\"hello, \" <> \"world\"");
    assert.match(out, /hello, world/);
  }));

  it("pulp psci includes dependencies with psc-package", run(function*(sh, pulp, assert) {
    yield pulp("--psc-package init");
    yield pulp("psci");

    const [out] = yield pulp("psci", "import Prelude\n\"hello, \" <> \"world\"");
    assert.match(out, /hello, world/);
  }));

  it("pulp psci includes local files", run(function*(sh, pulp, assert) {
    yield pulp("init");
    yield pulp("psci");

    const [out] = yield pulp("psci", "import Main as Main\nMain.main");
    assert.match(out, new RegExp(hello));
  }));

  it("pulp --before something build", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    // In reality, this is likely to be a "--before clear" or something, but
    // that's nightmarish to actually test.
    touch.sync(path.join(temp, "before.txt"));
    const mv = process.platform === "win32" ? "rename" : "mv";
    yield pulp(`--before "${mv} before.txt after.txt" build --to out.js`);

    const [out] = yield sh("node out.js");
    assert.equal(out.trim(), hello);
    assert.ok(yield fs.exists(path.join(temp, "after.txt")),
      "test file before.txt was not found as after.txt");
  }));

  it("pulp --then something build", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const mv = process.platform === "win32" ? "rename" : "mv";
    yield pulp(`--then "${mv} out.js out2.js" build --to out.js`);

    const [out] = yield sh("node out2.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp --else something build", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");

    // Deliberately cause a build failure.
    const mainPath = path.join(temp, 'src', 'Main.purs');
    yield fs.writeFile(
      mainPath,
      (yield fs.readFile(mainPath)).toString().concat("\ninvalidThing")
    );

    touch.sync(path.join(temp, "before.txt"));
    const mv = process.platform === "win32" ? "rename" : "mv";
    const [_, err] = yield pulp(
      `--else "${mv} before.txt afterFailed.txt" build --to out.js`,
      null, { expectedExitCode: 1 }
    );
    assert.match(err.trim(), /Unable to parse/); // Expected error
    assert.exists(path.join(temp, "afterFailed.txt")); // --else has run
  }));

  it("pulp --then something browserify", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    const mv = process.platform === "win32" ? "rename" : "mv";
    yield pulp(`--then "echo lol > out.txt" browserify`);
    assert.equal((yield fs.readFile(path.join(temp, "out.txt"), "utf-8")).trim(), "lol");
  }));

  it("pulp --then something browserify --to", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const mv = process.platform === "win32" ? "rename" : "mv";
    yield pulp(`--then "${mv} out.js out2.js" browserify --to out.js`);

    const [out] = yield sh("node out2.js");
    assert.equal(out.trim(), hello);
  }));

  it("pulp test --runtime", run(function*(sh, pulp, assert) {
    yield pulp("init");
    const [out] = yield pulp("test --runtime cat");
    const [out2] = yield sh("node", out);
    assert.equal(out2.trim(), test);
  }));

  it("pulp uses purs when psa is not available", run(function*(sh, pulp, assert, temp) {
    const h = yield psaHelper(temp);
    yield h.writeProg("purs", "0.11.4");
    yield pulp("init");

    const [out, err] = yield pulp("build", undefined, {path: h.binPath});
    assert.match(err, /assert purs/);
  }));

  it("pulp ignores psa when it is too old", run(function*(sh, pulp, assert, temp) {
    const h = yield psaHelper(temp);
    yield h.writeProg("purs", "0.11.4");
    yield h.writeProg("psa", "0.4.0");
    yield pulp("init");

    const [out, err] = yield pulp("build", undefined, {path: h.binPath});
    assert.match(err, /assert purs/);
  }));

  it("pulp uses psa when it is available", run(function*(sh, pulp, assert, temp) {
    const h = yield psaHelper(temp);
    yield h.writeProg("purs", "0.11.4");
    yield h.writeProg("psa", "0.5.0");
    yield pulp("init");

    const [out, err] = yield pulp("build", undefined, {path: h.binPath});
    assert.match(err, /assert psa/);
  }));

  it("pulp ignores psa when --no-psa is passed", run(function*(sh, pulp, assert, temp) {
    const h = yield psaHelper(temp);
    yield h.writeProg("purs", "0.11.4");
    yield h.writeProg("psa", "0.5.0");
    yield pulp("init");

    const [out, err] = yield pulp("build --no-psa", undefined, {path: h.binPath});
    assert.match(err, /assert purs/);
  }));

  it("pulp passes arguments through to psa", run(function*(sh, pulp, assert, temp) {
    const h = yield psaHelper(temp);
    yield h.writeProg("purs", "0.11.4");
    yield h.writeProg("psa", "0.5.0");
    yield pulp("init");

    const [out, err] = yield pulp("build -- --monochrome", undefined, {path: h.binPath});
    assert.match(err, /--monochrome/);
    assert.match(err, /--is-lib=bower_components/);
  }));

  it("pulp version requires a clean working tree", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield setupPackage(temp, sh);
    yield fs.writeFile(path.join(temp, "hello.txt"), "hello");

    // TODO: check the output.
    yield assertThrows(pulp("version"));
  }));

  it("pulp version checks using purs publish", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield setupPackage(temp, sh);

    // The bower.json file should be invalid because the repository key is
    // missing. TODO: Check that we actually get this error.
    yield assertThrows(pulp("version minor"));
  }));

  [["major", /v3.0.0/], ["minor", /v2.1.0/], ["patch", /v2.0.1/]].forEach((params) => {
    const [ bumpType, pattern ] = params;
    it("pulp version applies the specified bump: " + bumpType,
        run(function*(sh, pulp, assert, temp) {
      yield pulp("init");
      yield setupPackage(temp, sh);

      yield fs.writeFile(path.join(temp, "bower.json"), testBowerJson);
      yield sh("git commit -am \"updating bower.json\"");
      yield sh("git tag v2.0.0");
      yield sh("git commit --allow-empty -m \"an empty commit for the new tag\"");

      const [out, err] = yield pulp("version " + bumpType);
      assert.match(err, pattern);
    }));
  });

  it("pulp build -j 4", run(function*(sh, pulp, assert, temp) {
    yield pulp("init");
    yield pulp("build -j 4");

    assert.exists(path.join("output", "Main", "index.js"));
  }));

  it("exits 1 on invalid commands", run(function*(sh, pulp) {
    yield pulp("blah", null, { expectedExitCode: 1 });
  }));

  it("exits 1 on invalid options", run(function*(sh, pulp) {
    yield pulp("build --blah", null, { expectedExitCode: 1 });
  }));
});
