"use strict";

// module Test.Main

exports.argv = function argv() {
    return process.argv;
};

exports.slice = function slice(n) {
    return function (arr) {
        return arr.slice(n);
    };
};

exports.length = function length(arr) {
    return arr.length;
};
