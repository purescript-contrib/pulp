// module Test.Main

export const argv = function argv() {
    return process.argv;
};

export const slice = function slice(n) {
    return function (arr) {
        return arr.slice(n);
    };
};

export const length = function length(arr) {
    return arr.length;
};
