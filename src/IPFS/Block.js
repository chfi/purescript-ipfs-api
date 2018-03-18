"use strict";

exports.putImpl = function(ipfs, buf, opts) {
    // extract the data at the end so we're not returning a Block object
    return ipfs.block.put(buf, opts).then(function(b) { return {data: b.data, cid: b.cid.toBaseEncodedString('base58btc')}; });
};


exports.getImpl = function(ipfs, path) {
    // extract the data at the end so we're not returning a Block object
    return ipfs.block.get(path).then(function(b) { return {data: b.data, cid: b.cid.toBaseEncodedString('base58btc')}; });
};


exports.statImpl = function(ipfs, path) {
    return ipfs.block.stat(path);
};
