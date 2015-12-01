import co from "co";
import { exec } from "child_process";
import _temp from "temp";
import { resolve } from "path";
import assert from "assert";
import fs from "fs";

const temp = _temp.track();

function sh(cwd, cmd, input, opts) {
  return new Promise((resolve, reject) => {
    const proc = exec(cmd, { cwd }, (error, stdout, stderr) => {
      resolve({ error, stdout, stderr });
    });
    proc.stdin.end(input || "");
  }).then(function(r) {
    var expectedExitCode = (opts && opts.expectedExitCode) || 0;
    var exitCode = (r.error && r.error.code) || 0;
    if (expectedExitCode !== exitCode) {
      var msg = r.error.message + "Expected exit code " + expectedExitCode +
                " but got " + exitCode + ".";
      var newErr = new Error(msg);
      newErr.innerError = r.error;
      throw newErr;
    }

    return [r.stdout, r.stderr];
  });
}

function asserts(path) {
  return Object.assign({}, assert, {
    file(filename, pred) {
      const data = fs.readFileSync(resolve(path, filename), "utf-8");
      pred(data);
    }
  });
}

function pulpFn(path, pulpPath) {
  return (cmd, input, opts) =>
    sh(path, `node "${pulpPath}" ${cmd}`, input, opts);
}

export default function run(fn) {
  return function(done) {
    temp.mkdir("pulp-test-", (err, path) => {
      if (err) {
        throw err;
      } else {
        const pulpPath = resolve(__dirname, "..", "output", "pulp.js");
        const pulp = pulpFn(path, pulpPath);
        co(fn(sh.bind(null, path), pulp, asserts(path), path)).then(done, done);
      }
    });
  };
}
