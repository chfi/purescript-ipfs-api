"use strict";

exports.catImpl = function(ipfs, path) {
    return ipfs.files.cat(path);
};
