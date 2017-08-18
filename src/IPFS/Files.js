"use strict";

exports.addImpl = function(ipfs, objs) {
    return ipfs.files.add(objs);
};

exports.createAddStreamImpl = function(ipfs) {
    return ipfs.files.createAddStream();
};

exports.catImpl = function(ipfs, path) {
    return ipfs.files.cat(path);
};
