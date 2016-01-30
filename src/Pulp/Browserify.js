// module Pulp.Browserify

"use strict";

function write(input, output, callback) {
  var pipe = require("through")();
  input.pipe(pipe);
  pipe.pipe(output, {end: false});
  pipe.on("end", callback);
}

exports["browserifyBundle'"] = function browserifyBundle$prime(opts, callback) {
  var StringStream = require("string-stream");
  var browserify = require("browserify");

  var b = browserify({
    basedir: opts.basedir,
    entries: new StringStream(opts.src),
    standalone: opts.standalone
  });
  if (opts.transform) b.transform(opts.transform);
  write(b.bundle(), opts.out, callback);
};

exports["browserifyIncBundle'"] = function browserifyIncBundle$prime(opts, callback) {
  var browserifyInc = require("browserify-incremental");

  var b = browserifyInc({
    basedir: opts.buildPath,
    cacheFile: opts.cachePath,
    standalone: opts.standalone
  });
  b.add(opts.path);
  if (opts.transform) b.transform(opts.transform);
  write(b.bundle(), opts.out, callback);
};
