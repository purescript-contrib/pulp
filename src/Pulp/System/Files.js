// module Pulp.System.Files

"use strict";

exports.isEEXIST = function isEEXIST(err) {
  return err && err.code === 'EEXIST';
};
