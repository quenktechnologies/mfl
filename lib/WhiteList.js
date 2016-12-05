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
 * WhiteList discards any fields that have not been explicitly decleared.
 */
var WhiteList = function (_Policy) {
    _inherits(WhiteList, _Policy);

    function WhiteList() {
        _classCallCheck(this, WhiteList);

        return _possibleConstructorReturn(this, (WhiteList.__proto__ || Object.getPrototypeOf(WhiteList)).apply(this, arguments));
    }

    _createClass(WhiteList, [{
        key: 'willEliminate',
        value: function willEliminate(field, op, value) {

            if (!this.__fields.hasOwnProperty(field)) return true;

            if (this.__fields[field] === false) return true;
        }
    }]);

    return WhiteList;
}(_Policy3.default);

exports.default = WhiteList;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9XaGl0ZUxpc3QuanMiXSwibmFtZXMiOlsiV2hpdGVMaXN0IiwiZmllbGQiLCJvcCIsInZhbHVlIiwiX19maWVsZHMiLCJoYXNPd25Qcm9wZXJ0eSJdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7QUFBQTs7Ozs7Ozs7Ozs7O0FBRUE7OztJQUdxQkEsUzs7Ozs7Ozs7Ozs7c0NBRUhDLEssRUFBT0MsRSxFQUFJQyxLLEVBQU87O0FBRTVCLGdCQUFHLENBQUMsS0FBS0MsUUFBTCxDQUFjQyxjQUFkLENBQTZCSixLQUE3QixDQUFKLEVBQ0ksT0FBTyxJQUFQOztBQUVKLGdCQUFHLEtBQUtHLFFBQUwsQ0FBY0gsS0FBZCxNQUF5QixLQUE1QixFQUNJLE9BQU8sSUFBUDtBQUVQOzs7Ozs7a0JBVmdCRCxTIiwiZmlsZSI6IldoaXRlTGlzdC5qcyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCBQb2xpY3kgZnJvbSAnLi9Qb2xpY3knO1xuXG4vKipcbiAqIFdoaXRlTGlzdCBkaXNjYXJkcyBhbnkgZmllbGRzIHRoYXQgaGF2ZSBub3QgYmVlbiBleHBsaWNpdGx5IGRlY2xlYXJlZC5cbiAqL1xuZXhwb3J0IGRlZmF1bHQgY2xhc3MgV2hpdGVMaXN0IGV4dGVuZHMgUG9saWN5IHtcblxuICAgIHdpbGxFbGltaW5hdGUoZmllbGQsIG9wLCB2YWx1ZSkge1xuXG4gICAgICAgIGlmKCF0aGlzLl9fZmllbGRzLmhhc093blByb3BlcnR5KGZpZWxkKSlcbiAgICAgICAgICAgIHJldHVybiB0cnVlO1xuXG4gICAgICAgIGlmKHRoaXMuX19maWVsZHNbZmllbGRdID09PSBmYWxzZSlcbiAgICAgICAgICAgIHJldHVybiB0cnVlO1xuXG4gICAgfVxuXG59XG5cbiJdfQ==