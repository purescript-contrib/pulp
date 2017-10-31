"use strict";

exports.sorceryImpl = function sorceryImpl(file, succ, err) {
  var sorcery = require('sorcery');
  sorcery.load(file).then(function (chain) {
    if (!chain) {
      err(new Error("Sorcery did not resolve chain for " + file));
      return;
    }
    chain.write().then(succ, err);
  }, err);
};
