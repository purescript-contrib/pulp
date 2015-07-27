import co from "co";
import { exec } from "child_process";
import _temp from "temp";
import { resolve } from "path";
import assert from "assert";
import fs from "fs";

const temp = _temp.track();

function sh(cwd, cmd, input) {
  return new Promise((resolve, reject) => {
    const proc = exec(cmd, { cwd }, (err, stdout, stderr) => {
      if (err) {
        reject(err);
      } else {
        resolve([stdout, stderr]);
      }
    });
    if (input) {
      proc.stdin.end(input);
    }
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

export default function run(fn) {
  return function(done) {
    temp.mkdir("pulp-test-", (err, path) => {
      if (err) {
        throw err;
      } else {
        const pulpPath = resolve(__dirname, "..", "index.js");
        const pulp = (cmd) => sh(path, `node ${pulpPath} ${cmd}`);
        co(fn(sh.bind(null, path), pulp, asserts(path))).then(done, done);
      }
    });
  };
}
