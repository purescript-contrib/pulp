
// module Main

"use strict";

exports.unsafeInspect = function unsafeInspect(obj) {
  return require('util').inspect(obj);
};
