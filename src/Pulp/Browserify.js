// module Pulp.Browserify

"use strict";

exports["browserifyBundle'"] = function browserifyBundle$prime(opts, callback) {
  var StringStream = require("string-stream");
  var browserify = require("browserify");

  var b = browserify({
    basedir: opts.basedir,
    entries: new StringStream(opts.src)
  });
  if (opts.transform) b.transform(opts.transform);
  b.bundle().pipe(opts.out).on("close", callback);
};

exports["browserifyIncBundle'"] = function browserifyIncBundle$prime(opts, callback) {
  var browserifyInc = require("browserify-incremental");

  var b = browserifyInc({
    basedir: opts.buildPath,
    cacheFile: opts.cachePath
  });
  b.add(opts.path);
  if (opts.transform) b.transform(opts.transform);
  b.bundle().pipe(opts.out).on("close", callback);
};
