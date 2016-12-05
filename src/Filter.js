import beof from 'beof';
import { parser as Parser } from './ParserImpl';
import JisonError from './JisonError';

/* jshint ignore:start */
export JisonError from './JisonError';
export Policy from './Policy';
export BlackList from './BlackList';
export WhiteList from './WhiteList';
/* jshint ignore:end */

/**
 *
 * parse a string into an AST
 * @throws {JisonError}
 * @return {object} The parsed AST.
 */
export function parse(string) {

    try {

        return Parser.parse(string);

    } catch (e) {

        throw new JisonError(e);

    }

}

/**
 * compile the parsed AST.
 * @param {object} ast
 * @param {Policy} policy
 */
export function compile(ast, policy) {

    var q = Object.create(null);

    if (ast.filters.AND.length > 0)
        q.$and = ast.filters.AND.map(
            clause => policy.enforce(clause.field, clause.operator, clause.value)).filter(v => v);

    if (ast.filters.OR.length > 1)
        q.$or = ast.filters.OR.map(
            clause => policy.enforce(clause.field, clause.operator, clause.value)).filter(v => v);

    //Can't remember why this is here? Eliminate empty results or something? Legacy.
    q = Object.keys(q).reduce((pre, cur) => {

        if (!Array.isArray(q[cur]))
            return pre;

        if (q[cur].length === 0)
            return pre;

        pre[cur] = q[cur];

        return pre;

    }, {});

    if (Object.keys(q).length === 0)
        return null;

    return q;

}


/**
 * Filter represents the filter to be passed to mongodb to perform a query.
 *
 * Rather than passing an anonymous object, we provide an api that could be
 * used to further manipulate the query.
 * @param {object} fields
 */
export class Filter {

    constructor(fields = {}) {

        for (var key in fields)
            if (fields.hasOwnProperty(key))
                if (this[k] == null)
                    this[k] = fields[k];

    }

    /**
     *
     * formObject creates a new Filter using the passed object
     * as the initial filters.
     * @param {object} o
     * @returns {Filter}
     */
    static fromObject(o) {

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
    static fromString(string, policy) {

        beof({ string }).string();
        beof({ policy }).instance(Policy);

        var ast;

        try {

            ast = parse(string);

            return new Filter(convert(parse(string), allowed) || defaults);

        } catch (e) {

            if (e instanceof JisonError) {

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
    static where(field, filter) {

        var o = {};
        o[field] = filter;
        return new Filter(o);

    }

    /**
     * set a field value
     * @param {string} field
     * @param {*} value
     */
    set(field, value) {

        this[field] = value;
        return this;

    }

}

export default Filter
