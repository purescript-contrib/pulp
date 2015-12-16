// module Main

"use strict";

exports.logStack = function logStack(err) {
  return function() {
    console.log(err.stack);
  };
};
