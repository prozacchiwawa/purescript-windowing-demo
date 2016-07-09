/* global exports */
"use strict";

// module Debug

// Simple passthrough log value
exports.log = function(str) {
    return function(v) {
        console.log(str,v);
        return v;
    }
}
