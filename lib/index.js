'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.JisonError = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _beof = require('beof');

var _beof2 = _interopRequireDefault(_beof);

var _lib = require('./lib');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Filter represents the filter to be applied to a query.
 * This class api provides methods for manipulating the criteria
 * outside of this library so additional fields etc. can be added.
 * @param {object} fields
 */
var Filter = function () {
  function Filter() {
    var _this = this;

    var fields = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

    _classCallCheck(this, Filter);

    Object.keys(fields).forEach(function (k) {

      if (_this[k] == null) _this[k] = fields[k];
    });
  }

  _createClass(Filter, [{
    key: 'asValue',
    value: function asValue() {

      return this;
    }
  }, {
    key: 'toJSON',
    value: function toJSON() {

      return this.asValue();
    }

    /**
     * set a field value
     * @param {string} field
     * @param {*} value
     */

  }, {
    key: 'set',
    value: function set(field, value) {

      this[field] = value;
      return this;
    }
  }], [{
    key: 'fromString',
    value: function fromString() {
      var string = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
      var allowed = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
      var defaults = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};


      (0, _beof2.default)({
        string: string
      }).string();
      (0, _beof2.default)({
        allowed: allowed
      }).object();
      (0, _beof2.default)({
        defaults: defaults
      }).optional().object();

      try {

        return new this((0, _lib.convert)((0, _lib.parse)(string), allowed) || defaults);
      } catch (e) {

        if (e instanceof _lib.JisonError) return new this(defaults);

        throw e;
      }
    }

    /**
     * or constructs a Filter using a sequence of or conditions.
     * @param {object} ...filter
     */

  }, {
    key: 'or',
    value: function or() {

      return new Filter({
        $or: [].concat(Array.prototype.slice.call(arguments))
      });
    }

    /**
     * and constructs a Filter using a sequence of and conditions.
     * @param {object} ...filter
     */

  }, {
    key: 'and',
    value: function and() {

      return new Filter({
        $and: [].concat(Array.prototype.slice.call(arguments))
      });
    }

    /**
     * where constructs a Filter using a key value pair.
     * @param {string} field
     * @param {*} filter
     */

  }, {
    key: 'where',
    value: function where(field, filter) {

      var o = {};
      o[field] = filter;
      return new Filter(o);
    }
  }]);

  return Filter;
}();

exports.JisonError = _lib.JisonError;
exports.default = Filter;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9pbmRleC5qcyJdLCJuYW1lcyI6WyJGaWx0ZXIiLCJmaWVsZHMiLCJPYmplY3QiLCJrZXlzIiwiZm9yRWFjaCIsImsiLCJhc1ZhbHVlIiwiZmllbGQiLCJ2YWx1ZSIsInN0cmluZyIsImFsbG93ZWQiLCJkZWZhdWx0cyIsIm9iamVjdCIsIm9wdGlvbmFsIiwiZSIsIiRvciIsImFyZ3VtZW50cyIsIiRhbmQiLCJmaWx0ZXIiLCJvIiwiSmlzb25FcnJvciJdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7O0FBQUE7Ozs7QUFDQTs7Ozs7O0FBTUE7Ozs7OztJQU1NQSxNO0FBRUosb0JBQXlCO0FBQUE7O0FBQUEsUUFBYkMsTUFBYSx1RUFBSixFQUFJOztBQUFBOztBQUV2QkMsV0FBT0MsSUFBUCxDQUFZRixNQUFaLEVBQW9CRyxPQUFwQixDQUE0QixhQUFLOztBQUUvQixVQUFJLE1BQUtDLENBQUwsS0FBVyxJQUFmLEVBQ0UsTUFBS0EsQ0FBTCxJQUFVSixPQUFPSSxDQUFQLENBQVY7QUFFSCxLQUxEO0FBT0Q7Ozs7OEJBbUVTOztBQUVSLGFBQU8sSUFBUDtBQUVEOzs7NkJBRVE7O0FBRVAsYUFBTyxLQUFLQyxPQUFMLEVBQVA7QUFFRDs7QUFFRDs7Ozs7Ozs7d0JBS0lDLEssRUFBT0MsSyxFQUFPOztBQUVoQixXQUFLRCxLQUFMLElBQWNDLEtBQWQ7QUFDQSxhQUFPLElBQVA7QUFFRDs7O2lDQXZGeUQ7QUFBQSxVQUF4Q0MsTUFBd0MsdUVBQS9CLEVBQStCO0FBQUEsVUFBM0JDLE9BQTJCLHVFQUFuQixFQUFtQjtBQUFBLFVBQWZDLFFBQWUsdUVBQUosRUFBSTs7O0FBRXhELDBCQUFLO0FBQ0hGO0FBREcsT0FBTCxFQUVHQSxNQUZIO0FBR0EsMEJBQUs7QUFDSEM7QUFERyxPQUFMLEVBRUdFLE1BRkg7QUFHQSwwQkFBSztBQUNIRDtBQURHLE9BQUwsRUFFR0UsUUFGSCxHQUVjRCxNQUZkOztBQUlBLFVBQUk7O0FBRUYsZUFBTyxJQUFJLElBQUosQ0FBUyxrQkFBUSxnQkFBTUgsTUFBTixDQUFSLEVBQXVCQyxPQUF2QixLQUFtQ0MsUUFBNUMsQ0FBUDtBQUVELE9BSkQsQ0FJRSxPQUFPRyxDQUFQLEVBQVU7O0FBRVYsWUFBSUEsNEJBQUosRUFDRSxPQUFPLElBQUksSUFBSixDQUFTSCxRQUFULENBQVA7O0FBRUYsY0FBTUcsQ0FBTjtBQUVEO0FBRUY7O0FBRUQ7Ozs7Ozs7eUJBSVk7O0FBRVYsYUFBTyxJQUFJZCxNQUFKLENBQVc7QUFDaEJlLGtEQUFTQyxTQUFUO0FBRGdCLE9BQVgsQ0FBUDtBQUlEOztBQUdEOzs7Ozs7OzBCQUlhOztBQUVYLGFBQU8sSUFBSWhCLE1BQUosQ0FBVztBQUNoQmlCLG1EQUFVRCxTQUFWO0FBRGdCLE9BQVgsQ0FBUDtBQUlEOztBQUVEOzs7Ozs7OzswQkFLYVQsSyxFQUFPVyxNLEVBQVE7O0FBRTFCLFVBQUlDLElBQUksRUFBUjtBQUNBQSxRQUFFWixLQUFGLElBQVdXLE1BQVg7QUFDQSxhQUFPLElBQUlsQixNQUFKLENBQVdtQixDQUFYLENBQVA7QUFFRDs7Ozs7O1FBNkJEQyxVO2tCQUVhcEIsTSIsImZpbGUiOiJpbmRleC5qcyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCBiZW9mIGZyb20gJ2Jlb2YnO1xuaW1wb3J0IHtcbiAgcGFyc2UsXG4gIGNvbnZlcnQsXG4gIEppc29uRXJyb3Jcbn0gZnJvbSAnLi9saWInO1xuXG4vKipcbiAqIEZpbHRlciByZXByZXNlbnRzIHRoZSBmaWx0ZXIgdG8gYmUgYXBwbGllZCB0byBhIHF1ZXJ5LlxuICogVGhpcyBjbGFzcyBhcGkgcHJvdmlkZXMgbWV0aG9kcyBmb3IgbWFuaXB1bGF0aW5nIHRoZSBjcml0ZXJpYVxuICogb3V0c2lkZSBvZiB0aGlzIGxpYnJhcnkgc28gYWRkaXRpb25hbCBmaWVsZHMgZXRjLiBjYW4gYmUgYWRkZWQuXG4gKiBAcGFyYW0ge29iamVjdH0gZmllbGRzXG4gKi9cbmNsYXNzIEZpbHRlciB7XG5cbiAgY29uc3RydWN0b3IoZmllbGRzID0ge30pIHtcblxuICAgIE9iamVjdC5rZXlzKGZpZWxkcykuZm9yRWFjaChrID0+IHtcblxuICAgICAgaWYgKHRoaXNba10gPT0gbnVsbClcbiAgICAgICAgdGhpc1trXSA9IGZpZWxkc1trXTtcblxuICAgIH0pO1xuXG4gIH1cblxuICBzdGF0aWMgZnJvbVN0cmluZyhzdHJpbmcgPSAnJywgYWxsb3dlZD17fSwgZGVmYXVsdHMgPSB7fSkge1xuXG4gICAgYmVvZih7XG4gICAgICBzdHJpbmdcbiAgICB9KS5zdHJpbmcoKTtcbiAgICBiZW9mKHtcbiAgICAgIGFsbG93ZWRcbiAgICB9KS5vYmplY3QoKTtcbiAgICBiZW9mKHtcbiAgICAgIGRlZmF1bHRzXG4gICAgfSkub3B0aW9uYWwoKS5vYmplY3QoKTtcblxuICAgIHRyeSB7XG5cbiAgICAgIHJldHVybiBuZXcgdGhpcyhjb252ZXJ0KHBhcnNlKHN0cmluZyksIGFsbG93ZWQpIHx8IGRlZmF1bHRzKTtcblxuICAgIH0gY2F0Y2ggKGUpIHtcblxuICAgICAgaWYgKGUgaW5zdGFuY2VvZiBKaXNvbkVycm9yKVxuICAgICAgICByZXR1cm4gbmV3IHRoaXMoZGVmYXVsdHMpO1xuXG4gICAgICB0aHJvdyBlO1xuXG4gICAgfVxuXG4gIH1cblxuICAvKipcbiAgICogb3IgY29uc3RydWN0cyBhIEZpbHRlciB1c2luZyBhIHNlcXVlbmNlIG9mIG9yIGNvbmRpdGlvbnMuXG4gICAqIEBwYXJhbSB7b2JqZWN0fSAuLi5maWx0ZXJcbiAgICovXG4gIHN0YXRpYyBvcigpIHtcblxuICAgIHJldHVybiBuZXcgRmlsdGVyKHtcbiAgICAgICRvcjogWy4uLmFyZ3VtZW50c11cbiAgICB9KTtcblxuICB9XG5cblxuICAvKipcbiAgICogYW5kIGNvbnN0cnVjdHMgYSBGaWx0ZXIgdXNpbmcgYSBzZXF1ZW5jZSBvZiBhbmQgY29uZGl0aW9ucy5cbiAgICogQHBhcmFtIHtvYmplY3R9IC4uLmZpbHRlclxuICAgKi9cbiAgc3RhdGljIGFuZCgpIHtcblxuICAgIHJldHVybiBuZXcgRmlsdGVyKHtcbiAgICAgICRhbmQ6IFsuLi5hcmd1bWVudHNdXG4gICAgfSk7XG5cbiAgfVxuXG4gIC8qKlxuICAgKiB3aGVyZSBjb25zdHJ1Y3RzIGEgRmlsdGVyIHVzaW5nIGEga2V5IHZhbHVlIHBhaXIuXG4gICAqIEBwYXJhbSB7c3RyaW5nfSBmaWVsZFxuICAgKiBAcGFyYW0geyp9IGZpbHRlclxuICAgKi9cbiAgc3RhdGljIHdoZXJlKGZpZWxkLCBmaWx0ZXIpIHtcblxuICAgIHZhciBvID0ge307XG4gICAgb1tmaWVsZF0gPSBmaWx0ZXI7XG4gICAgcmV0dXJuIG5ldyBGaWx0ZXIobyk7XG5cbiAgfVxuXG4gIGFzVmFsdWUoKSB7XG5cbiAgICByZXR1cm4gdGhpcztcblxuICB9XG5cbiAgdG9KU09OKCkge1xuXG4gICAgcmV0dXJuIHRoaXMuYXNWYWx1ZSgpO1xuXG4gIH1cblxuICAvKipcbiAgICogc2V0IGEgZmllbGQgdmFsdWVcbiAgICogQHBhcmFtIHtzdHJpbmd9IGZpZWxkXG4gICAqIEBwYXJhbSB7Kn0gdmFsdWVcbiAgICovXG4gIHNldChmaWVsZCwgdmFsdWUpIHtcblxuICAgIHRoaXNbZmllbGRdID0gdmFsdWU7XG4gICAgcmV0dXJuIHRoaXM7XG5cbiAgfVxuXG59XG5cbmV4cG9ydCB7XG4gIEppc29uRXJyb3Jcbn1cbmV4cG9ydCBkZWZhdWx0IEZpbHRlclxuXG4iXX0=