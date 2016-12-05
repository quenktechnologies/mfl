'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _Policy2 = require('./Policy');

var _Policy3 = _interopRequireDefault(_Policy2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

/**
 * BlackList discards any fields in the query that have a truthy
 * value in the 'fields' object. Any unspecified fields are kept
 * @param {object} fields A map of fields
 */
var BlackList = function (_Policy) {
    _inherits(BlackList, _Policy);

    function BlackList() {
        _classCallCheck(this, BlackList);

        return _possibleConstructorReturn(this, (BlackList.__proto__ || Object.getPrototypeOf(BlackList)).apply(this, arguments));
    }

    _createClass(BlackList, [{
        key: 'willEliminate',
        value: function willEliminate(field, op, value) {

            if (this.__fields[field] === true) return true;
        }
    }]);

    return BlackList;
}(_Policy3.default);

exports.default = BlackList;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9CbGFja0xpc3QuanMiXSwibmFtZXMiOlsiQmxhY2tMaXN0IiwiZmllbGQiLCJvcCIsInZhbHVlIiwiX19maWVsZHMiXSwibWFwcGluZ3MiOiI7Ozs7Ozs7O0FBQUE7Ozs7Ozs7Ozs7OztBQUVBOzs7OztJQUtxQkEsUzs7Ozs7Ozs7Ozs7c0NBRUhDLEssRUFBT0MsRSxFQUFJQyxLLEVBQU87O0FBRTVCLGdCQUFJLEtBQUtDLFFBQUwsQ0FBY0gsS0FBZCxNQUF5QixJQUE3QixFQUNJLE9BQU8sSUFBUDtBQUVQOzs7Ozs7a0JBUGdCRCxTIiwiZmlsZSI6IkJsYWNrTGlzdC5qcyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCBQb2xpY3kgZnJvbSAnLi9Qb2xpY3knO1xuXG4vKipcbiAqIEJsYWNrTGlzdCBkaXNjYXJkcyBhbnkgZmllbGRzIGluIHRoZSBxdWVyeSB0aGF0IGhhdmUgYSB0cnV0aHlcbiAqIHZhbHVlIGluIHRoZSAnZmllbGRzJyBvYmplY3QuIEFueSB1bnNwZWNpZmllZCBmaWVsZHMgYXJlIGtlcHRcbiAqIEBwYXJhbSB7b2JqZWN0fSBmaWVsZHMgQSBtYXAgb2YgZmllbGRzXG4gKi9cbmV4cG9ydCBkZWZhdWx0IGNsYXNzIEJsYWNrTGlzdCBleHRlbmRzIFBvbGljeSB7XG5cbiAgICB3aWxsRWxpbWluYXRlKGZpZWxkLCBvcCwgdmFsdWUpIHtcblxuICAgICAgICBpZiAodGhpcy5fX2ZpZWxkc1tmaWVsZF0gPT09IHRydWUpXG4gICAgICAgICAgICByZXR1cm4gdHJ1ZTtcblxuICAgIH1cblxufVxuIl19