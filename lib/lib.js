'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.JisonError = JisonError;
exports.convert = convert;
exports.parse = parse;

var _parser = require('./parser');

function JisonError(e) {
  var _this = this;

  Object.keys(e).forEach(function (k) {
    return _this[k] = e[k];
  });

  this.stack = new Error(e.message).stack;

  if (Error.hasOwnProperty('captureStackTrace')) Error.captureStackTrace(this, this.constructor);

  Object.defineProperties(this, {

    message: {
      configurable: true,
      enumerable: true,
      writable: true,
      value: e.message
    }
  });
}

JisonError.prototype = Object.create(Error.prototype);
JisonError.prototype.name = 'JisonError';
JisonError.prototype.message = '';
JisonError.prototype.constructor = JisonError;

function get_clause(field, symbol, value) {

  var clause = Object.create(null);

  switch (symbol) {

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
 * filter_conversion filters out the unknown fields before converting them.
 * @return {array}
 */
function filter_conversion(list, map) {

  return list.map(function (clause) {

    if (!map.hasOwnProperty(clause.field)) return null;

    return get_clause(clause.field, clause.operator, typeof map[clause.field] === 'function' ? map[clause.field](clause.value) : clause.value);
  }).filter(function (clause) {
    return clause;
  });
}

/**
 * convert is a function that takes the AST
 * and turns it into a query. Currently preforms no optimisations
 * so expensive queries are possible. Also this does no
 * real validation of filter criteria, use a map of functions
 * to intercept potentially dangerous values.
 *
 * @todo Perhaps provide a syntax to declare what operators
 * are valid for a particular key? Maybe in the map like so:
 *  ```javascript {
 *       name: {valid_ops:['?', '='], check:x=>}
 *     }
 *  ```
 *
 * Be careful...
 * @param {AST} ast The result of a successful parse call.
 * @param {object} map A map that indicates what are valid fields.
 *                     If the value the value of the map keys are functions
 *                     they will each be called when their field is detected.
 */
function convert(ast, map) {

  var q = Object.create(null);

  if (ast.filters.AND.length > 0) q.$and = filter_conversion(ast.filters.AND, map);

  if (ast.filters.OR.length > 1) q.$or = filter_conversion(ast.filters.OR, map);

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
 * parse parses a string into an
 * abstract syntax tree (ast)
 * @param {string} source
 * @returns {object}
 */
function parse(source) {

  try {

    return _parser.parser.parse(source);
  } catch (e) {

    throw new JisonError(e);
  }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uL3NyYy9saWIuanMiXSwibmFtZXMiOlsiSmlzb25FcnJvciIsImNvbnZlcnQiLCJwYXJzZSIsImUiLCJPYmplY3QiLCJrZXlzIiwiZm9yRWFjaCIsImsiLCJzdGFjayIsIkVycm9yIiwibWVzc2FnZSIsImhhc093blByb3BlcnR5IiwiY2FwdHVyZVN0YWNrVHJhY2UiLCJjb25zdHJ1Y3RvciIsImRlZmluZVByb3BlcnRpZXMiLCJjb25maWd1cmFibGUiLCJlbnVtZXJhYmxlIiwid3JpdGFibGUiLCJ2YWx1ZSIsInByb3RvdHlwZSIsImNyZWF0ZSIsIm5hbWUiLCJnZXRfY2xhdXNlIiwiZmllbGQiLCJzeW1ib2wiLCJjbGF1c2UiLCIkZ3QiLCIkZ3RlIiwiJGx0IiwiJGx0ZSIsIiRpbiIsIiRyZWdleCIsInJlcGxhY2UiLCIkb3B0aW9ucyIsImZpbHRlcl9jb252ZXJzaW9uIiwibGlzdCIsIm1hcCIsIm9wZXJhdG9yIiwiZmlsdGVyIiwiYXN0IiwicSIsImZpbHRlcnMiLCJBTkQiLCJsZW5ndGgiLCIkYW5kIiwiT1IiLCIkb3IiLCJyZWR1Y2UiLCJwcmUiLCJjdXIiLCJBcnJheSIsImlzQXJyYXkiLCJzb3VyY2UiXSwibWFwcGluZ3MiOiI7Ozs7O1FBSWdCQSxVLEdBQUFBLFU7UUEwSEFDLE8sR0FBQUEsTztRQW9DQUMsSyxHQUFBQSxLOztBQWxLaEI7O0FBSU8sU0FBU0YsVUFBVCxDQUFvQkcsQ0FBcEIsRUFBdUI7QUFBQTs7QUFHNUJDLFNBQU9DLElBQVAsQ0FBWUYsQ0FBWixFQUFlRyxPQUFmLENBQXVCO0FBQUEsV0FBSyxNQUFLQyxDQUFMLElBQVVKLEVBQUVJLENBQUYsQ0FBZjtBQUFBLEdBQXZCOztBQUVBLE9BQUtDLEtBQUwsR0FBYyxJQUFJQyxLQUFKLENBQVVOLEVBQUVPLE9BQVosQ0FBRCxDQUF1QkYsS0FBcEM7O0FBRUEsTUFBSUMsTUFBTUUsY0FBTixDQUFxQixtQkFBckIsQ0FBSixFQUNFRixNQUFNRyxpQkFBTixDQUF3QixJQUF4QixF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QUVGSSxNQUFJcEMsT0FBT0MsSUFBUCxDQUFZbUMsQ0FBWixFQUFlTyxNQUFmLENBQXNCLFVBQUNDLEdBQUQsRUFBTUMsR0FBTixFQUFjOztBQUV0QyxRQUFJLENBQUNDLE1BQU1DLE9BQU4sQ0FBY1gsRUFBRVMsR0FBRixDQUFkLENBQUwsRUFDRSxPQUFPRCxHQUFQOztBQUVGLFFBQUlSLEVBQUVTLEdBQUYsRUFBT04sTUFBUCxLQUFrQixDQUF0QixFQUNFLE9BQU9LLEdBQVA7O0FBRUZBLFFBQUlDLEdBQUosSUFBV1QsRUFBRVMsR0FBRixDQUFYOztBQUVBLFdBQU9ELEdBQVA7QUFFRCxHQVpHLEVBWUQsRUFaQyxDQUFKOztBQWNBLE1BQUk1QyxPQUFPQyxJQUFQLENBQVltQyxDQUFaLEVBQWVHLE1BQWYsS0FBMEIsQ0FBOUIsRUFDRSxPQUFPLElBQVA7QUFDRixTQUFPSCxDQUFQO0FBRUQ7O0FBRUQ7Ozs7OztBQU1PLFNBQVN0QyxLQUFULENBQWVrRCxNQUFmLEVBQXVCOztBQUU1QixNQUFJOztBQUVGLFdBQU8sZUFBT2xELEtBQVAsQ0FBYWtELE1BQWIsQ0FBUDtBQUVELEdBSkQsQ0FJRSxPQUFPakQsQ0FBUCxFQUFVOztBQUVWLFVBQU0sSUFBSUgsVUFBSixDQUFlRyxDQUFmLENBQU47QUFFRDtBQUNGIiwiZmlsZSI6ImxpYi5qcyIsInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB7XG4gIHBhcnNlclxufSBmcm9tICcuL3BhcnNlcic7XG5cbmV4cG9ydCBmdW5jdGlvbiBKaXNvbkVycm9yKGUpIHtcblxuXG4gIE9iamVjdC5rZXlzKGUpLmZvckVhY2goayA9PiB0aGlzW2tdID0gZVtrXSk7XG5cbiAgdGhpcy5zdGFjayA9IChuZXcgRXJyb3IoZS5tZXNzYWdlKSkuc3RhY2s7XG5cbiAgaWYgKEVycm9yLmhhc093blByb3BlcnR5KCdjYXB0dXJlU3RhY2tUcmFjZScpKVxuICAgIEVycm9yLmNhcHR1cmVTdGFja1RyYWNlKHRoaXMsIHRoaXMuY29uc3RydWN0b3IpO1xuXG4gIE9iamVjdC5kZWZpbmVQcm9wZXJ0aWVzKHRoaXMsIHtcblxuICAgIG1lc3NhZ2U6IHtcbiAgICAgIGNvbmZpZ3VyYWJsZTogdHJ1ZSxcbiAgICAgIGVudW1lcmFibGU6IHRydWUsXG4gICAgICB3cml0YWJsZTogdHJ1ZSxcbiAgICAgIHZhbHVlOiBlLm1lc3NhZ2VcbiAgICB9XG4gIH0pO1xuXG5cbn1cblxuSmlzb25FcnJvci5wcm90b3R5cGUgPSBPYmplY3QuY3JlYXRlKEVycm9yLnByb3RvdHlwZSk7XG5KaXNvbkVycm9yLnByb3RvdHlwZS5uYW1lID0gJ0ppc29uRXJyb3InO1xuSmlzb25FcnJvci5wcm90b3R5cGUubWVzc2FnZSA9ICcnO1xuSmlzb25FcnJvci5wcm90b3R5cGUuY29uc3RydWN0b3IgPSBKaXNvbkVycm9yO1xuXG5mdW5jdGlvbiBnZXRfY2xhdXNlKGZpZWxkLCBzeW1ib2wsIHZhbHVlKSB7XG5cbiAgdmFyIGNsYXVzZSA9IE9iamVjdC5jcmVhdGUobnVsbCk7XG5cbiAgc3dpdGNoIChzeW1ib2wpIHtcblxuICAgIGNhc2UgJz0nOlxuICAgICAgY2xhdXNlW2ZpZWxkXSA9IHZhbHVlO1xuICAgICAgYnJlYWs7XG5cbiAgICBjYXNlICc+JzpcbiAgICAgIGNsYXVzZVtmaWVsZF0gPSB7XG4gICAgICAgICRndDogdmFsdWVcbiAgICAgIH07XG4gICAgICBicmVhaztcblxuICAgIGNhc2UgJz49JzpcbiAgICAgIGNsYXVzZVtmaWVsZF0gPSB7XG4gICAgICAgICRndGU6IHZhbHVlXG4gICAgICB9O1xuICAgICAgYnJlYWs7XG5cbiAgICBjYXNlICc8JzpcbiAgICAgIGNsYXVzZVtmaWVsZF0gPSB7XG4gICAgICAgICRsdDogdmFsdWVcbiAgICAgIH07XG4gICAgICBicmVhaztcblxuICAgIGNhc2UgJzw9JzpcbiAgICAgIGNsYXVzZVtmaWVsZF0gPSB7XG4gICAgICAgICRsdGU6IHZhbHVlXG4gICAgICB9O1xuICAgICAgYnJlYWs7XG5cbiAgICBjYXNlICckaW4nOlxuICAgICAgY2xhdXNlW2ZpZWxkXSA9IHtcbiAgICAgICAgJGluOiB2YWx1ZVxuICAgICAgfTtcbiAgICAgIGJyZWFrO1xuXG4gICAgY2FzZSAnPyc6XG4gICAgICBjbGF1c2VbZmllbGRdID0ge1xuICAgICAgICAkcmVnZXg6IHZhbHVlLnJlcGxhY2UoL1stXFwvXFxcXF4kKis/LigpfFtcXF17fV0vZywgXCJcXFxcJCZcIiksXG4gICAgICAgICRvcHRpb25zOiAnaSdcbiAgICAgIH07XG4gICAgICBicmVhaztcbiAgICBkZWZhdWx0OlxuICAgICAgYnJlYWs7XG5cbiAgfVxuXG4gIHJldHVybiBjbGF1c2U7XG5cbn1cblxuLyoqXG4gKiBmaWx0ZXJfY29udmVyc2lvbiBmaWx0ZXJzIG91dCB0aGUgdW5rbm93biBmaWVsZHMgYmVmb3JlIGNvbnZlcnRpbmcgdGhlbS5cbiAqIEByZXR1cm4ge2FycmF5fVxuICovXG5mdW5jdGlvbiBmaWx0ZXJfY29udmVyc2lvbihsaXN0LCBtYXApIHtcblxuICByZXR1cm4gbGlzdC5tYXAoY2xhdXNlID0+IHtcblxuICAgIGlmICghbWFwLmhhc093blByb3BlcnR5KGNsYXVzZS5maWVsZCkpXG4gICAgICByZXR1cm4gbnVsbDtcblxuICAgIHJldHVybiBnZXRfY2xhdXNlKGNsYXVzZS5maWVsZCwgY2xhdXNlLm9wZXJhdG9yLFxuICAgICAgKHR5cGVvZiBtYXBbY2xhdXNlLmZpZWxkXSA9PT0gJ2Z1bmN0aW9uJykgP1xuICAgICAgbWFwW2NsYXVzZS5maWVsZF0oY2xhdXNlLnZhbHVlKSA6IGNsYXVzZS52YWx1ZSk7XG5cbiAgfSkuZmlsdGVyKGNsYXVzZSA9PiBjbGF1c2UpO1xuXG59XG5cbi8qKlxuICogY29udmVydCBpcyBhIGZ1bmN0aW9uIHRoYXQgdGFrZXMgdGhlIEFTVFxuICogYW5kIHR1cm5zIGl0IGludG8gYSBxdWVyeS4gQ3VycmVudGx5IHByZWZvcm1zIG5vIG9wdGltaXNhdGlvbnNcbiAqIHNvIGV4cGVuc2l2ZSBxdWVyaWVzIGFyZSBwb3NzaWJsZS4gQWxzbyB0aGlzIGRvZXMgbm9cbiAqIHJlYWwgdmFsaWRhdGlvbiBvZiBmaWx0ZXIgY3JpdGVyaWEsIHVzZSBhIG1hcCBvZiBmdW5jdGlvbnNcbiAqIHRvIGludGVyY2VwdCBwb3RlbnRpYWxseSBkYW5nZXJvdXMgdmFsdWVzLlxuICpcbiAqIEB0b2RvIFBlcmhhcHMgcHJvdmlkZSBhIHN5bnRheCB0byBkZWNsYXJlIHdoYXQgb3BlcmF0b3JzXG4gKiBhcmUgdmFsaWQgZm9yIGEgcGFydGljdWxhciBrZXk/IE1heWJlIGluIHRoZSBtYXAgbGlrZSBzbzpcbiAqICBgYGBqYXZhc2NyaXB0IHtcbiAqICAgICAgIG5hbWU6IHt2YWxpZF9vcHM6Wyc/JywgJz0nXSwgY2hlY2s6eD0+fVxuICogICAgIH1cbiAqICBgYGBcbiAqXG4gKiBCZSBjYXJlZnVsLi4uXG4gKiBAcGFyYW0ge0FTVH0gYXN0IFRoZSByZXN1bHQgb2YgYSBzdWNjZXNzZnVsIHBhcnNlIGNhbGwuXG4gKiBAcGFyYW0ge29iamVjdH0gbWFwIEEgbWFwIHRoYXQgaW5kaWNhdGVzIHdoYXQgYXJlIHZhbGlkIGZpZWxkcy5cbiAqICAgICAgICAgICAgICAgICAgICAgSWYgdGhlIHZhbHVlIHRoZSB2YWx1ZSBvZiB0aGUgbWFwIGtleXMgYXJlIGZ1bmN0aW9uc1xuICogICAgICAgICAgICAgICAgICAgICB0aGV5IHdpbGwgZWFjaCBiZSBjYWxsZWQgd2hlbiB0aGVpciBmaWVsZCBpcyBkZXRlY3RlZC5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGNvbnZlcnQoYXN0LCBtYXApIHtcblxuICB2YXIgcSA9IE9iamVjdC5jcmVhdGUobnVsbCk7XG5cbiAgaWYgKGFzdC5maWx0ZXJzLkFORC5sZW5ndGggPiAwKVxuICAgIHEuJGFuZCA9IGZpbHRlcl9jb252ZXJzaW9uKGFzdC5maWx0ZXJzLkFORCwgbWFwKTtcblxuICBpZiAoYXN0LmZpbHRlcnMuT1IubGVuZ3RoID4gMSlcbiAgICBxLiRvciA9IGZpbHRlcl9jb252ZXJzaW9uKGFzdC5maWx0ZXJzLk9SLCBtYXApO1xuXG4gIHEgPSBPYmplY3Qua2V5cyhxKS5yZWR1Y2UoKHByZSwgY3VyKSA9PiB7XG5cbiAgICBpZiAoIUFycmF5LmlzQXJyYXkocVtjdXJdKSlcbiAgICAgIHJldHVybiBwcmU7XG5cbiAgICBpZiAocVtjdXJdLmxlbmd0aCA9PT0gMClcbiAgICAgIHJldHVybiBwcmU7XG5cbiAgICBwcmVbY3VyXSA9IHFbY3VyXTtcblxuICAgIHJldHVybiBwcmU7XG5cbiAgfSwge30pO1xuXG4gIGlmIChPYmplY3Qua2V5cyhxKS5sZW5ndGggPT09IDApXG4gICAgcmV0dXJuIG51bGw7XG4gIHJldHVybiBxO1xuXG59XG5cbi8qKlxuICogcGFyc2UgcGFyc2VzIGEgc3RyaW5nIGludG8gYW5cbiAqIGFic3RyYWN0IHN5bnRheCB0cmVlIChhc3QpXG4gKiBAcGFyYW0ge3N0cmluZ30gc291cmNlXG4gKiBAcmV0dXJucyB7b2JqZWN0fVxuICovXG5leHBvcnQgZnVuY3Rpb24gcGFyc2Uoc291cmNlKSB7XG5cbiAgdHJ5IHtcblxuICAgIHJldHVybiBwYXJzZXIucGFyc2Uoc291cmNlKTtcblxuICB9IGNhdGNoIChlKSB7XG5cbiAgICB0aHJvdyBuZXcgSmlzb25FcnJvcihlKTtcblxuICB9XG59XG5cbiJdfQ==