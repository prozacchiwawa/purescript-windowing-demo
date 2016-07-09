/* global exports */
"use strict";

// module Belgium

exports.nativeToString = function(v) {
    if (v === undefined) {
        return 'undefined';
    } else if (v === null) {
        return 'null';
    } else {
        return JSON.stringify(v);
    }
}
