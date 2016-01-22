import co from "co";
import { exec } from "child_process";
import _temp from "temp";
import { resolve } from "path";
import { assert } from "chai";
import fs from "fs";
import which from "which";

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
        const pulp = pulpFn(path, pulpPath);
        co(fn(sh.bind(null, path), pulp, asserts(path), path)).then(done, done);
      }
    });
  };
}
