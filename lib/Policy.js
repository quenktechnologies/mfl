'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Policy provides an api for specifying how each parsed field should
 * be treated, as well as what to do with fields not specified (discard or keep?).
 *
 * The actual policy is specified by an map of key value pairs where each key represents
 * a field and its value should be one of boolean, string, function.
 *
 * Boolean values are decided by child classes of this Policy.
 * String values must be one of the recongnized filter operators and will replace whatever
 * was parsed.
 * Functions are applied to the value allowing validation, transformation etc before the final
 * query is used.
 *
 * @abstract
 * @param {object} fields
 */
var Policy = function () {
    function Policy(fields) {
        _classCallCheck(this, Policy);

        this.__fields = fields;
    }

    _createClass(Policy, [{
        key: '_getFilter',
        value: function _getFilter(field, op, value) {

            var clause = Object.create(null);

            switch (op) {

                case '=':
                    clause[field] = value;
                    break;

                case '>':
                    clause[field] = {
                        $gt: value
                    };
                    break;

                case '>=':
                    clause[field] = {
                        $gte: value
                    };
                    break;

                case '<':
                    clause[field] = {
                        $lt: value
                    };
                    break;

                case '<=':
                    clause[field] = {
                        $lte: value
                    };
                    break;

                case '$in':
                    clause[field] = {
                        $in: value
                    };
                    break;

                case '?':

                    clause[field] = {
                        $regex: value.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"),
                        $options: 'i'
                    };
                    break;
                default:
                    break;

            }

            return clause;
        }

        /**
         * willEliminate is implemented by child Policy's to
         * determine whether a field should be kept or dismissed.
         * @abstract
         * @param {string} field
         * @param {string} op
         * @param {*} value
         */

    }, {
        key: 'willEliminate',
        value: function willEliminate(field, op, value) {}

        /**
         * enforce this policy
         * @param {string} field
         * @param {string} op
         * @param {*} value
         * @return {object}
         */

    }, {
        key: 'enforce',
        value: function enforce(field, op, value) {

            var spec;

            if (this.willEliminate(field, op, value)) return null;

            spec = this.__fields[field];

            if ((typeof spec === 'undefined' ? 'undefined' : _typeof(spec)) === 'object') {

                op = spec.op || op;
                value = spec.value ? spec.value(value) : value;
            } else if (typeof spec === 'function') {

                value = spec(value);
            } else if (typeof spec === 'string') {

                op = spec;
            }

            return this._getFilter(field, op, value);
        }
    }]);

    return Policy;
}();

exports.default = Policy;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9Qb2xpY3kuanMiXSwibmFtZXMiOlsiUG9saWN5IiwiZmllbGRzIiwiX19maWVsZHMiLCJmaWVsZCIsIm9wIiwidmFsdWUiLCJjbGF1c2UiLCJPYmplY3QiLCJjcmVhdGUiLCIkZ3QiLCIkZ3RlIiwiJGx0IiwiJGx0ZSIsIiRpbiIsIiRyZWdleCIsInJlcGxhY2UiLCIkb3B0aW9ucyIsInNwZWMiLCJ3aWxsRWxpbWluYXRlIiwiX2dldEZpbHRlciJdLCJtYXBwaW5ncyI6Ijs7Ozs7Ozs7Ozs7O0FBQUE7Ozs7Ozs7Ozs7Ozs7Ozs7SUFnQnFCQSxNO0FBRWpCLG9CQUFZQyxNQUFaLEVBQW9CO0FBQUE7O0FBRWhCLGFBQUtDLFFBQUwsR0FBZ0JELE1BQWhCO0FBRUg7Ozs7bUNBRVVFLEssRUFBT0MsRSxFQUFJQyxLLEVBQU87O0FBRXpCLGdCQUFJQyxTQUFTQyxPQUFPQyxNQUFQLENBQWMsSUFBZCxDQUFiOztBQUVBLG9CQUFRSixFQUFSOztBQUVJLHFCQUFLLEdBQUw7QUFDSUUsMkJBQU9ILEtBQVAsSUFBZ0JFLEtBQWhCO0FBQ0E7O0FBRUoscUJBQUssR0FBTDtBQUNJQywyQkFBT0gsS0FBUCxJQUFnQjtBQUNaTSw2QkFBS0o7QUFETyxxQkFBaEI7QUFHQTs7QUFFSixxQkFBSyxJQUFMO0FBQ0lDLDJCQUFPSCxLQUFQLElBQWdCO0FBQ1pPLDhCQUFNTDtBQURNLHFCQUFoQjtBQUdBOztBQUVKLHFCQUFLLEdBQUw7QUFDSUMsMkJBQU9ILEtBQVAsSUFBZ0I7QUFDWlEsNkJBQUtOO0FBRE8scUJBQWhCO0FBR0E7O0FBRUoscUJBQUssSUFBTDtBQUNJQywyQkFBT0gsS0FBUCxJQUFnQjtBQUNaUyw4QkFBTVA7QUFETSxxQkFBaEI7QUFHQTs7QUFFSixxQkFBSyxLQUFMO0FBQ0lDLDJCQUFPSCxLQUFQLElBQWdCO0FBQ1pVLDZCQUFLUjtBQURPLHFCQUFoQjtBQUdBOztBQUVKLHFCQUFLLEdBQUw7O0FBRUlDLDJCQUFPSCxLQUFQLElBQWdCO0FBQ1pXLGdDQUFRVCxNQUFNVSxPQUFOLENBQWMsd0JBQWQsRUFBd0MsTUFBeEMsQ0FESTtBQUVaQyxrQ0FBVTtBQUZFLHFCQUFoQjtBQUlBO0FBQ0o7QUFDSTs7QUE1Q1I7O0FBZ0RBLG1CQUFPVixNQUFQO0FBRUg7O0FBRUQ7Ozs7Ozs7Ozs7O3NDQVFjSCxLLEVBQU9DLEUsRUFBSUMsSyxFQUFPLENBRS9COztBQUVEOzs7Ozs7Ozs7O2dDQU9RRixLLEVBQU9DLEUsRUFBSUMsSyxFQUFPOztBQUV0QixnQkFBSVksSUFBSjs7QUFFQSxnQkFBSSxLQUFLQyxhQUFMLENBQW1CZixLQUFuQixFQUEwQkMsRUFBMUIsRUFBOEJDLEtBQTlCLENBQUosRUFDSSxPQUFPLElBQVA7O0FBRUpZLG1CQUFPLEtBQUtmLFFBQUwsQ0FBY0MsS0FBZCxDQUFQOztBQUVBLGdCQUFJLFFBQU9jLElBQVAseUNBQU9BLElBQVAsT0FBZ0IsUUFBcEIsRUFBK0I7O0FBRTNCYixxQkFBS2EsS0FBS2IsRUFBTCxJQUFXQSxFQUFoQjtBQUNBQyx3QkFBUVksS0FBS1osS0FBTCxHQUFZWSxLQUFLWixLQUFMLENBQVdBLEtBQVgsQ0FBWixHQUFnQ0EsS0FBeEM7QUFFSCxhQUxELE1BS00sSUFBSSxPQUFPWSxJQUFQLEtBQWdCLFVBQXBCLEVBQWdDOztBQUVsQ1osd0JBQVFZLEtBQUtaLEtBQUwsQ0FBUjtBQUVILGFBSkssTUFJQyxJQUFJLE9BQU9ZLElBQVAsS0FBZ0IsUUFBcEIsRUFBOEI7O0FBRWpDYixxQkFBS2EsSUFBTDtBQUVIOztBQUVELG1CQUFPLEtBQUtFLFVBQUwsQ0FBZ0JoQixLQUFoQixFQUF1QkMsRUFBdkIsRUFBMkJDLEtBQTNCLENBQVA7QUFFSDs7Ozs7O2tCQTdHZ0JMLE0iLCJmaWxlIjoiUG9saWN5LmpzIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBQb2xpY3kgcHJvdmlkZXMgYW4gYXBpIGZvciBzcGVjaWZ5aW5nIGhvdyBlYWNoIHBhcnNlZCBmaWVsZCBzaG91bGRcbiAqIGJlIHRyZWF0ZWQsIGFzIHdlbGwgYXMgd2hhdCB0byBkbyB3aXRoIGZpZWxkcyBub3Qgc3BlY2lmaWVkIChkaXNjYXJkIG9yIGtlZXA/KS5cbiAqXG4gKiBUaGUgYWN0dWFsIHBvbGljeSBpcyBzcGVjaWZpZWQgYnkgYW4gbWFwIG9mIGtleSB2YWx1ZSBwYWlycyB3aGVyZSBlYWNoIGtleSByZXByZXNlbnRzXG4gKiBhIGZpZWxkIGFuZCBpdHMgdmFsdWUgc2hvdWxkIGJlIG9uZSBvZiBib29sZWFuLCBzdHJpbmcsIGZ1bmN0aW9uLlxuICpcbiAqIEJvb2xlYW4gdmFsdWVzIGFyZSBkZWNpZGVkIGJ5IGNoaWxkIGNsYXNzZXMgb2YgdGhpcyBQb2xpY3kuXG4gKiBTdHJpbmcgdmFsdWVzIG11c3QgYmUgb25lIG9mIHRoZSByZWNvbmduaXplZCBmaWx0ZXIgb3BlcmF0b3JzIGFuZCB3aWxsIHJlcGxhY2Ugd2hhdGV2ZXJcbiAqIHdhcyBwYXJzZWQuXG4gKiBGdW5jdGlvbnMgYXJlIGFwcGxpZWQgdG8gdGhlIHZhbHVlIGFsbG93aW5nIHZhbGlkYXRpb24sIHRyYW5zZm9ybWF0aW9uIGV0YyBiZWZvcmUgdGhlIGZpbmFsXG4gKiBxdWVyeSBpcyB1c2VkLlxuICpcbiAqIEBhYnN0cmFjdFxuICogQHBhcmFtIHtvYmplY3R9IGZpZWxkc1xuICovXG5leHBvcnQgZGVmYXVsdCBjbGFzcyBQb2xpY3kge1xuXG4gICAgY29uc3RydWN0b3IoZmllbGRzKSB7XG5cbiAgICAgICAgdGhpcy5fX2ZpZWxkcyA9IGZpZWxkcztcblxuICAgIH1cblxuICAgIF9nZXRGaWx0ZXIoZmllbGQsIG9wLCB2YWx1ZSkge1xuXG4gICAgICAgIHZhciBjbGF1c2UgPSBPYmplY3QuY3JlYXRlKG51bGwpO1xuXG4gICAgICAgIHN3aXRjaCAob3ApIHtcblxuICAgICAgICAgICAgY2FzZSAnPSc6XG4gICAgICAgICAgICAgICAgY2xhdXNlW2ZpZWxkXSA9IHZhbHVlO1xuICAgICAgICAgICAgICAgIGJyZWFrO1xuXG4gICAgICAgICAgICBjYXNlICc+JzpcbiAgICAgICAgICAgICAgICBjbGF1c2VbZmllbGRdID0ge1xuICAgICAgICAgICAgICAgICAgICAkZ3Q6IHZhbHVlXG4gICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgICAgICBicmVhaztcblxuICAgICAgICAgICAgY2FzZSAnPj0nOlxuICAgICAgICAgICAgICAgIGNsYXVzZVtmaWVsZF0gPSB7XG4gICAgICAgICAgICAgICAgICAgICRndGU6IHZhbHVlXG4gICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgICAgICBicmVhaztcblxuICAgICAgICAgICAgY2FzZSAnPCc6XG4gICAgICAgICAgICAgICAgY2xhdXNlW2ZpZWxkXSA9IHtcbiAgICAgICAgICAgICAgICAgICAgJGx0OiB2YWx1ZVxuICAgICAgICAgICAgICAgIH07XG4gICAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgICAgIGNhc2UgJzw9JzpcbiAgICAgICAgICAgICAgICBjbGF1c2VbZmllbGRdID0ge1xuICAgICAgICAgICAgICAgICAgICAkbHRlOiB2YWx1ZVxuICAgICAgICAgICAgICAgIH07XG4gICAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgICAgIGNhc2UgJyRpbic6XG4gICAgICAgICAgICAgICAgY2xhdXNlW2ZpZWxkXSA9IHtcbiAgICAgICAgICAgICAgICAgICAgJGluOiB2YWx1ZVxuICAgICAgICAgICAgICAgIH07XG4gICAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgICAgIGNhc2UgJz8nOlxuXG4gICAgICAgICAgICAgICAgY2xhdXNlW2ZpZWxkXSA9IHtcbiAgICAgICAgICAgICAgICAgICAgJHJlZ2V4OiB2YWx1ZS5yZXBsYWNlKC9bLVxcL1xcXFxeJCorPy4oKXxbXFxde31dL2csIFwiXFxcXCQmXCIpLFxuICAgICAgICAgICAgICAgICAgICAkb3B0aW9uczogJ2knXG4gICAgICAgICAgICAgICAgfTtcbiAgICAgICAgICAgICAgICBicmVhaztcbiAgICAgICAgICAgIGRlZmF1bHQ6XG4gICAgICAgICAgICAgICAgYnJlYWs7XG5cbiAgICAgICAgfVxuXG4gICAgICAgIHJldHVybiBjbGF1c2U7XG5cbiAgICB9XG5cbiAgICAvKipcbiAgICAgKiB3aWxsRWxpbWluYXRlIGlzIGltcGxlbWVudGVkIGJ5IGNoaWxkIFBvbGljeSdzIHRvXG4gICAgICogZGV0ZXJtaW5lIHdoZXRoZXIgYSBmaWVsZCBzaG91bGQgYmUga2VwdCBvciBkaXNtaXNzZWQuXG4gICAgICogQGFic3RyYWN0XG4gICAgICogQHBhcmFtIHtzdHJpbmd9IGZpZWxkXG4gICAgICogQHBhcmFtIHtzdHJpbmd9IG9wXG4gICAgICogQHBhcmFtIHsqfSB2YWx1ZVxuICAgICAqL1xuICAgIHdpbGxFbGltaW5hdGUoZmllbGQsIG9wLCB2YWx1ZSkge1xuXG4gICAgfVxuXG4gICAgLyoqXG4gICAgICogZW5mb3JjZSB0aGlzIHBvbGljeVxuICAgICAqIEBwYXJhbSB7c3RyaW5nfSBmaWVsZFxuICAgICAqIEBwYXJhbSB7c3RyaW5nfSBvcFxuICAgICAqIEBwYXJhbSB7Kn0gdmFsdWVcbiAgICAgKiBAcmV0dXJuIHtvYmplY3R9XG4gICAgICovXG4gICAgZW5mb3JjZShmaWVsZCwgb3AsIHZhbHVlKSB7XG5cbiAgICAgICAgdmFyIHNwZWM7XG5cbiAgICAgICAgaWYgKHRoaXMud2lsbEVsaW1pbmF0ZShmaWVsZCwgb3AsIHZhbHVlKSlcbiAgICAgICAgICAgIHJldHVybiBudWxsO1xuXG4gICAgICAgIHNwZWMgPSB0aGlzLl9fZmllbGRzW2ZpZWxkXTtcblxuICAgICAgICBpZiggdHlwZW9mIHNwZWMgPT09ICdvYmplY3QnICkge1xuXG4gICAgICAgICAgICBvcCA9IHNwZWMub3AgfHwgb3A7XG4gICAgICAgICAgICB2YWx1ZSA9IHNwZWMudmFsdWU/IHNwZWMudmFsdWUodmFsdWUpIDogdmFsdWU7XG5cbiAgICAgICAgfWVsc2UgaWYgKHR5cGVvZiBzcGVjID09PSAnZnVuY3Rpb24nKSB7XG5cbiAgICAgICAgICAgIHZhbHVlID0gc3BlYyh2YWx1ZSk7XG5cbiAgICAgICAgfSBlbHNlIGlmICh0eXBlb2Ygc3BlYyA9PT0gJ3N0cmluZycpIHtcblxuICAgICAgICAgICAgb3AgPSBzcGVjO1xuXG4gICAgICAgIH1cblxuICAgICAgICByZXR1cm4gdGhpcy5fZ2V0RmlsdGVyKGZpZWxkLCBvcCwgdmFsdWUpO1xuXG4gICAgfVxuXG59XG4iXX0=