'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.default = parse;

var _lib = require('./lib');

var lib = _interopRequireWildcard(_lib);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function parse(src, map) {

    return lib.convert(lib.parse(src), map);
}
//# sourceMappingURL=index.js.map