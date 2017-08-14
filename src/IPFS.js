"use strict";

var ipfsAPI = require("ipfs-api");

exports.connectImpl = function(host, port) {
    return ipfsAPI(host, port, {protocol: "http"});
};


exports.identityImpl = function(ipfs) {
    return ipfs.id();
};

exports.versionImpl = function(ipfs) {
    return ipfs.version();
};
