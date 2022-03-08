// module Pulp.Browserify

"use strict";

function write(input, output, callback) {
  var pipe = require("through")();
  input.pipe(pipe);
  pipe.pipe(output, {end: false});
  pipe.on("end", callback);
}

exports.browserifyBundleImpl = function browserifyBundle$prime(opts, callback) {
  var stream = new require("stream").Readable();
  var browserify = require("browserify");
  var mold = require("mold-source-map");
  var path = require("path");
  stream.push(opts.src);
  stream.push(null);
  var b = browserify({
    basedir: opts.basedir,
    entries: stream,
    standalone: opts.standalone,
    debug: opts.debug
  });
  if (opts.transform) {
    b.transform(opts.transform);
  }
  var bundle = b.bundle();
  if (opts.debug) {
    var tmpRoot = path.dirname(opts.tmpFilePath);
    bundle = bundle
     .pipe(mold.transformSourcesContent(function (s, i) {
        if (i === 1) {
          return s.replace('//# sourceMappingURL=', "$&" + tmpRoot + "/");
        }
        return s;
      })
    );
  }
  write(bundle, opts.out, callback);
};

exports.browserifyIncBundleImpl = function browserifyIncBundle$prime(opts, callback) {
  var browserifyInc = require("browserify-incremental");
  var mold = require("mold-source-map");
  var path = require('path');
  var b = browserifyInc({
    basedir: opts.buildPath,
    cacheFile: opts.cachePath,
    standalone: opts.standalone,
    debug: opts.debug
  });
  b.add(opts.path);
  if (opts.transform) b.transform(opts.transform);
  var bundle = b.bundle();
  if (opts.debug) {
    bundle = bundle.pipe(mold.transform(function (map) {
      map.sourceRoot(path.resolve());
      return map.toComment();
    }));
  }
  write(bundle, opts.out, callback);
};
