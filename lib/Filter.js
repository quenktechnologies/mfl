'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.Filter = exports.WhiteList = exports.BlackList = exports.Policy = exports.JisonError = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

/* jshint ignore:start */


exports.parse = parse;
exports.compile = compile;

var _beof = require('beof');

var _beof2 = _interopRequireDefault(_beof);

var _ParserImpl = require('./ParserImpl');

var _JisonError2 = require('./JisonError');

var _JisonError3 = _interopRequireDefault(_JisonError2);

var _Policy2 = require('./Policy');

var _Policy3 = _interopRequireDefault(_Policy2);

var _BlackList2 = require('./BlackList');

var _BlackList3 = _interopRequireDefault(_BlackList2);

var _WhiteList2 = require('./WhiteList');

var _WhiteList3 = _interopRequireDefault(_WhiteList2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

exports.JisonError = _JisonError3.default;
exports.Policy = _Policy3.default;
exports.BlackList = _BlackList3.default;
exports.WhiteList = _WhiteList3.default;
/* jshint ignore:end */

/**
 *
 * parse a string into an AST
 * @throws {JisonError}
 * @return {object} The parsed AST.
 */

function parse(string) {

    try {

        return _ParserImpl.parser.parse(string);
    } catch (e) {

        throw new _JisonError3.default(e);
    }
}

/**
 * compile the parsed AST.
 * @param {object} ast
 * @param {Policy} policy
 */
function compile(ast, policy) {

    var q = Object.create(null);

    if (ast.filters.AND.length > 0) q.$and = ast.filters.AND.map(function (clause) {
        return policy.enforce(clause.field, clause.operator, clause.value);
    }).filter(function (v) {
        return v;
    });

    if (ast.filters.OR.length > 1) q.$or = ast.filters.OR.map(function (clause) {
        return policy.enforce(clause.field, clause.operator, clause.value);
    }).filter(function (v) {
        return v;
    });

    //Can't remember why this is here? Eliminate empty results or something? Legacy.
    q = Object.keys(q).reduce(function (pre, cur) {

        if (!Array.isArray(q[cur])) return pre;

        if (q[cur].length === 0) return pre;

        pre[cur] = q[cur];

        return pre;
    }, {});

    if (Object.keys(q).length === 0) return null;

    return q;
}

/**
 * Filter represents the filter to be passed to mongodb to perform a query.
 *
 * Rather than passing an anonymous object, we provide an api that could be
 * used to further manipulate the query.
 * @param {object} fields
 */

var Filter = exports.Filter = function () {
    function Filter() {
        var fields = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

        _classCallCheck(this, Filter);

        for (var key in fields) {
            if (fields.hasOwnProperty(key)) if (this[k] == null) this[k] = fields[k];
        }
    }

    /**
     *
     * formObject creates a new Filter using the passed object
     * as the initial filters.
     * @param {object} o
     * @returns {Filter}
     */


    _createClass(Filter, [{
        key: 'set',


        /**
         * set a field value
         * @param {string} field
         * @param {*} value
         */
        value: function set(field, value) {

            this[field] = value;
            return this;
        }
    }], [{
        key: 'fromObject',
        value: function fromObject(o) {

            return new Filter(o);
        }

        /**
         *
         * formString parses a string and turns it into a Filter.
         *
         * The policy helps the compile function decide what to include or exclude
         * and the defaults options is used where parsing the string fails.
         * @param {string} source
         * @param {Policy} policy
         * @param {*} [defaults={}]
         */

    }, {
        key: 'fromString',
        value: function fromString(string, policy) {

            (0, _beof2.default)({ string: string }).string();
            (0, _beof2.default)({ policy: policy }).instance(Policy);

            var ast;

            try {

                ast = parse(string);

                return new Filter(convert(parse(string), allowed) || defaults);
            } catch (e) {

                if (e instanceof _JisonError3.default) {

                    if (defaults) {

                        ast = defaults;
                    } else {

                        throw e;
                    }
                }
            }

            return new Filter(compile(ast, policy));
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

exports.default = Filter;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9GaWx0ZXIuanMiXSwibmFtZXMiOlsicGFyc2UiLCJjb21waWxlIiwiSmlzb25FcnJvciIsIlBvbGljeSIsIkJsYWNrTGlzdCIsIldoaXRlTGlzdCIsInN0cmluZyIsImUiLCJhc3QiLCJwb2xpY3kiLCJxIiwiT2JqZWN0IiwiY3JlYXRlIiwiZmlsdGVycyIsIkFORCIsImxlbmd0aCIsIiRhbmQiLCJtYXAiLCJlbmZvcmNlIiwiY2xhdXNlIiwiZmllbGQiLCJvcGVyYXRvciIsInZhbHVlIiwiZmlsdGVyIiwidiIsIk9SIiwiJG9yIiwia2V5cyIsInJlZHVjZSIsInByZSIsImN1ciIsIkFycmF5IiwiaXNBcnJheSIsIkZpbHRlciIsImZpZWxkcyIsImtleSIsImhhc093blByb3BlcnR5IiwiayIsIm8iLCJpbnN0YW5jZSIsImNvbnZlcnQiLCJhbGxvd2VkIiwiZGVmYXVsdHMiXSwibWFwcGluZ3MiOiI7Ozs7Ozs7OztBQUlBOzs7UUFhZ0JBLEssR0FBQUEsSztRQW1CQUMsTyxHQUFBQSxPOztBQXBDaEI7Ozs7QUFDQTs7QUFDQTs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7UUFHT0MsVTtRQUNBQyxNO1FBQ0FDLFM7UUFDQUMsUztBQUNQOztBQUVBOzs7Ozs7O0FBTU8sU0FBU0wsS0FBVCxDQUFlTSxNQUFmLEVBQXVCOztBQUUxQixRQUFJOztBQUVBLGVBQU8sbUJBQU9OLEtBQVAsQ0FBYU0sTUFBYixDQUFQO0FBRUgsS0FKRCxDQUlFLE9BQU9DLENBQVAsRUFBVTs7QUFFUixjQUFNLHlCQUFlQSxDQUFmLENBQU47QUFFSDtBQUVKOztBQUVEOzs7OztBQUtPLFNBQVNOLE9BQVQsQ0FBaUJPLEdBQWpCLEVBQXNCQyxNQUF0QixFQUE4Qjs7QUFFakMsUUFBSUMsSUFBSUMsT0FBT0MsTUFBUCxDQUFjLElBQWQsQ0FBUjs7QUFFQSxRQUFJSixJQUFJSyxPQUFKLENBQVlDLEdBQVosQ0FBZ0JDLE1BQWhCLEdBQXlCLENBQTdCLEVBQ0lMLEVBQUVNLElBQUYsR0FB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LGdCQUFJZSxJQUFJLEVBQVI7QUFDQUEsY0FBRWxCLEtBQUYsSUFBV0csTUFBWDtBQUNBLG1CQUFPLElBQUlVLE1BQUosQ0FBV0ssQ0FBWCxDQUFQO0FBRUg7Ozs7OztrQkFnQlVMLE0iLCJmaWxlIjoiRmlsdGVyLmpzIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0IGJlb2YgZnJvbSAnYmVvZic7XG5pbXBvcnQgeyBwYXJzZXIgYXMgUGFyc2VyIH0gZnJvbSAnLi9QYXJzZXJJbXBsJztcbmltcG9ydCBKaXNvbkVycm9yIGZyb20gJy4vSmlzb25FcnJvcic7XG5cbi8qIGpzaGludCBpZ25vcmU6c3RhcnQgKi9cbmV4cG9ydCBKaXNvbkVycm9yIGZyb20gJy4vSmlzb25FcnJvcic7XG5leHBvcnQgUG9saWN5IGZyb20gJy4vUG9saWN5JztcbmV4cG9ydCBCbGFja0xpc3QgZnJvbSAnLi9CbGFja0xpc3QnO1xuZXhwb3J0IFdoaXRlTGlzdCBmcm9tICcuL1doaXRlTGlzdCc7XG4vKiBqc2hpbnQgaWdub3JlOmVuZCAqL1xuXG4vKipcbiAqXG4gKiBwYXJzZSBhIHN0cmluZyBpbnRvIGFuIEFTVFxuICogQHRocm93cyB7Smlzb25FcnJvcn1cbiAqIEByZXR1cm4ge29iamVjdH0gVGhlIHBhcnNlZCBBU1QuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBwYXJzZShzdHJpbmcpIHtcblxuICAgIHRyeSB7XG5cbiAgICAgICAgcmV0dXJuIFBhcnNlci5wYXJzZShzdHJpbmcpO1xuXG4gICAgfSBjYXRjaCAoZSkge1xuXG4gICAgICAgIHRocm93IG5ldyBKaXNvbkVycm9yKGUpO1xuXG4gICAgfVxuXG59XG5cbi8qKlxuICogY29tcGlsZSB0aGUgcGFyc2VkIEFTVC5cbiAqIEBwYXJhbSB7b2JqZWN0fSBhc3RcbiAqIEBwYXJhbSB7UG9saWN5fSBwb2xpY3lcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGNvbXBpbGUoYXN0LCBwb2xpY3kpIHtcblxuICAgIHZhciBxID0gT2JqZWN0LmNyZWF0ZShudWxsKTtcblxuICAgIGlmIChhc3QuZmlsdGVycy5BTkQubGVuZ3RoID4gMClcbiAgICAgICAgcS4kYW5kID0gYXN0LmZpbHRlcnMuQU5ELm1hcChcbiAgICAgICAgICAgIGNsYXVzZSA9PiBwb2xpY3kuZW5mb3JjZShjbGF1c2UuZmllbGQsIGNsYXVzZS5vcGVyYXRvciwgY2xhdXNlLnZhbHVlKSkuZmlsdGVyKHYgPT4gdik7XG5cbiAgICBpZiAoYXN0LmZpbHRlcnMuT1IubGVuZ3RoID4gMSlcbiAgICAgICAgcS4kb3IgPSBhc3QuZmlsdGVycy5PUi5tYXAoXG4gICAgICAgICAgICBjbGF1c2UgPT4gcG9saWN5LmVuZm9yY2UoY2xhdXNlLmZpZWxkLCBjbGF1c2Uub3BlcmF0b3IsIGNsYXVzZS52YWx1ZSkpLmZpbHRlcih2ID0+IHYpO1xuXG4gICAgLy9DYW4ndCByZW1lbWJlciB3aHkgdGhpcyBpcyBoZXJlPyBFbGltaW5hdGUgZW1wdHkgcmVzdWx0cyBvciBzb21ldGhpbmc/IExlZ2FjeS5cbiAgICBxID0gT2JqZWN0LmtleXMocSkucmVkdWNlKChwcmUsIGN1cikgPT4ge1xuXG4gICAgICAgIGlmICghQXJyYXkuaXNBcnJheShxW2N1cl0pKVxuICAgICAgICAgICAgcmV0dXJuIHByZTtcblxuICAgICAgICBpZiAocVtjdXJdLmxlbmd0aCA9PT0gMClcbiAgICAgICAgICAgIHJldHVybiBwcmU7XG5cbiAgICAgICAgcHJlW2N1cl0gPSBxW2N1cl07XG5cbiAgICAgICAgcmV0dXJuIHByZTtcblxuICAgIH0sIHt9KTtcblxuICAgIGlmIChPYmplY3Qua2V5cyhxKS5sZW5ndGggPT09IDApXG4gICAgICAgIHJldHVybiBudWxsO1xuXG4gICAgcmV0dXJuIHE7XG5cbn1cblxuXG4vKipcbiAqIEZpbHRlciByZXByZXNlbnRzIHRoZSBmaWx0ZXIgdG8gYmUgcGFzc2VkIHRvIG1vbmdvZGIgdG8gcGVyZm9ybSBhIHF1ZXJ5LlxuICpcbiAqIFJhdGhlciB0aGFuIHBhc3NpbmcgYW4gYW5vbnltb3VzIG9iamVjdCwgd2UgcHJvdmlkZSBhbiBhcGkgdGhhdCBjb3VsZCBiZVxuICogdXNlZCB0byBmdXJ0aGVyIG1hbmlwdWxhdGUgdGhlIHF1ZXJ5LlxuICogQHBhcmFtIHtvYmplY3R9IGZpZWxkc1xuICovXG5leHBvcnQgY2xhc3MgRmlsdGVyIHtcblxuICAgIGNvbnN0cnVjdG9yKGZpZWxkcyA9IHt9KSB7XG5cbiAgICAgICAgZm9yICh2YXIga2V5IGluIGZpZWxkcylcbiAgICAgICAgICAgIGlmIChmaWVsZHMuaGFzT3duUHJvcGVydHkoa2V5KSlcbiAgICAgICAgICAgICAgICBpZiAodGhpc1trXSA9PSBudWxsKVxuICAgICAgICAgICAgICAgICAgICB0aGlzW2tdID0gZmllbGRzW2tdO1xuXG4gICAgfVxuXG4gICAgLyoqXG4gICAgICpcbiAgICAgKiBmb3JtT2JqZWN0IGNyZWF0ZXMgYSBuZXcgRmlsdGVyIHVzaW5nIHRoZSBwYXNzZWQgb2JqZWN0XG4gICAgICogYXMgdGhlIGluaXRpYWwgZmlsdGVycy5cbiAgICAgKiBAcGFyYW0ge29iamVjdH0gb1xuICAgICAqIEByZXR1cm5zIHtGaWx0ZXJ9XG4gICAgICovXG4gICAgc3RhdGljIGZyb21PYmplY3Qobykge1xuXG4gICAgICAgIHJldHVybiBuZXcgRmlsdGVyKG8pO1xuXG4gICAgfVxuXG4gICAgLyoqXG4gICAgICpcbiAgICAgKiBmb3JtU3RyaW5nIHBhcnNlcyBhIHN0cmluZyBhbmQgdHVybnMgaXQgaW50byBhIEZpbHRlci5cbiAgICAgKlxuICAgICAqIFRoZSBwb2xpY3kgaGVscHMgdGhlIGNvbXBpbGUgZnVuY3Rpb24gZGVjaWRlIHdoYXQgdG8gaW5jbHVkZSBvciBleGNsdWRlXG4gICAgICogYW5kIHRoZSBkZWZhdWx0cyBvcHRpb25zIGlzIHVzZWQgd2hlcmUgcGFyc2luZyB0aGUgc3RyaW5nIGZhaWxzLlxuICAgICAqIEBwYXJhbSB7c3RyaW5nfSBzb3VyY2VcbiAgICAgKiBAcGFyYW0ge1BvbGljeX0gcG9saWN5XG4gICAgICogQHBhcmFtIHsqfSBbZGVmYXVsdHM9e31dXG4gICAgICovXG4gICAgc3RhdGljIGZyb21TdHJpbmcoc3RyaW5nLCBwb2xpY3kpIHtcblxuICAgICAgICBiZW9mKHsgc3RyaW5nIH0pLnN0cmluZygpO1xuICAgICAgICBiZW9mKHsgcG9saWN5IH0pLmluc3RhbmNlKFBvbGljeSk7XG5cbiAgICAgICAgdmFyIGFzdDtcblxuICAgICAgICB0cnkge1xuXG4gICAgICAgICAgICBhc3QgPSBwYXJzZShzdHJpbmcpO1xuXG4gICAgICAgICAgICByZXR1cm4gbmV3IEZpbHRlcihjb252ZXJ0KHBhcnNlKHN0cmluZyksIGFsbG93ZWQpIHx8IGRlZmF1bHRzKTtcblxuICAgICAgICB9IGNhdGNoIChlKSB7XG5cbiAgICAgICAgICAgIGlmIChlIGluc3RhbmNlb2YgSmlzb25FcnJvcikge1xuXG4gICAgICAgICAgICAgICAgaWYgKGRlZmF1bHRzKSB7XG5cbiAgICAgICAgICAgICAgICAgICAgYXN0ID0gZGVmYXVsdHM7XG5cbiAgICAgICAgICAgICAgICB9IGVsc2Uge1xuXG4gICAgICAgICAgICAgICAgICAgIHRocm93IGU7XG5cbiAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgIH1cblxuICAgICAgICB9XG5cbiAgICAgICAgcmV0dXJuIG5ldyBGaWx0ZXIoY29tcGlsZShhc3QsIHBvbGljeSkpO1xuICAgIH1cblxuICAgIC8qKlxuICAgICAqIHdoZXJlIGNvbnN0cnVjdHMgYSBGaWx0ZXIgdXNpbmcgYSBrZXkgdmFsdWUgcGFpci5cbiAgICAgKiBAcGFyYW0ge3N0cmluZ30gZmllbGRcbiAgICAgKiBAcGFyYW0geyp9IGZpbHRlclxuICAgICAqL1xuICAgIHN0YXRpYyB3aGVyZShmaWVsZCwgZmlsdGVyKSB7XG5cbiAgICAgICAgdmFyIG8gPSB7fTtcbiAgICAgICAgb1tmaWVsZF0gPSBmaWx0ZXI7XG4gICAgICAgIHJldHVybiBuZXcgRmlsdGVyKG8pO1xuXG4gICAgfVxuXG4gICAgLyoqXG4gICAgICogc2V0IGEgZmllbGQgdmFsdWVcbiAgICAgKiBAcGFyYW0ge3N0cmluZ30gZmllbGRcbiAgICAgKiBAcGFyYW0geyp9IHZhbHVlXG4gICAgICovXG4gICAgc2V0KGZpZWxkLCB2YWx1ZSkge1xuXG4gICAgICAgIHRoaXNbZmllbGRdID0gdmFsdWU7XG4gICAgICAgIHJldHVybiB0aGlzO1xuXG4gICAgfVxuXG59XG5cbmV4cG9ydCBkZWZhdWx0IEZpbHRlclxuIl19