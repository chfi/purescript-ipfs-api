"use strict";

exports.catImpl = function(ipfs, path) {
    ipfs.files.cat(path, function(err, str) {
        console.log("reading from files");
        console.log(str.isPaused());
        console.log(str);
    });

    var f = ipfs.files.cat(path);
    return f;
};
