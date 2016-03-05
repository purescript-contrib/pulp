import co from "co";
import { exec } from "child_process";
import _temp from "temp";
import { resolve, join } from "path";
import { assert } from "chai";
import fs from "fs";
import which from "which";
import tar from "tar-fs";
import zlib from "zlib";

const temp = _temp.track();

function sh(cwd, cmd, input, opts) {
  opts = opts || {};
  return new Promise((resolve, reject) => {
    const procOpts = { cwd: opts.cwd || cwd };
    if (opts.path) {
      procOpts.env = {...process.env};
      const pathVar = process.platform === "win32" ? "Path" : "PATH";
      procOpts.env[pathVar] = opts.path;
    }
    const proc = exec(cmd, procOpts, (error, stdout, stderr) => {
      resolve({ error, stdout, stderr });
    });
    proc.stdin.end(input || "");
  }).then(function(r) {
    const expectedExitCode = (opts && opts.expectedExitCode) || 0;
    const exitCode = (r.error && r.error.code) || 0;
    if (expectedExitCode !== exitCode) {
      let msg = (r.error && r.error.message) || "";
      msg += "Expected exit code " + expectedExitCode +
             " but got " + exitCode + ".";
      const newErr = new Error(msg);
      newErr.innerError = r.error;
      throw newErr;
    }

    return [r.stdout, r.stderr];
  });
}

function asserts(path) {
  const file = (filename, pred) => {
    const data = fs.readFileSync(resolve(path, filename), "utf-8");
    pred(data);
  };

  const exists = (filename) => file(filename, (data) => true);

  return Object.assign({}, assert, { file, exists });
}

function resolvePath(cmd) {
  return new Promise((resolve, reject) => {
    which(cmd, (err, res) => err ? reject(err) : resolve(res));
  });
}

// Has the same effect as running 'pulp init', but uses a pre-prepared tar.gz
// file, which is stored in the Pulp code repository. Returns a promise.
function pulpFastInit(destDirectory) {
  const tarballPath = resolve(__dirname, "pulp-init.tar.gz");
  return new Promise((resolve, reject) => {
    const stream = fs
      .createReadStream(tarballPath)
      .pipe(zlib.createGunzip())
      .pipe(tar.extract(destDirectory, { strict: false }));

    stream.on("finish", resolve);
    stream.on("error", reject);
  });
}

function pulpFn(path, pulpPath) {
  return (cmd, input, opts) =>
    resolvePath("node").then((node) =>
      sh(path, `"${node}" "${pulpPath}" ${cmd}`, input, opts));
}

export default function run(fn) {
  return function(done) {
    temp.mkdir("pulp-test-", (err, path) => {
      if (err) {
        throw err;
      } else {
        const pulpPath = resolve(__dirname, "..", "index.js");
        let pulp = pulpFn(path, pulpPath);
        pulp.fastInit = pulpFastInit;
        co(fn(sh.bind(null, path), pulp, asserts(path), path)).then(done, done);
      }
    });
  };
}
