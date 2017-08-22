"use strict";

exports.addImpl = function(ipfs, objs) {
    return ipfs.files.add(objs);
};


exports.catImpl = function(ipfs, path) {
    return ipfs.files.cat(path);
};
