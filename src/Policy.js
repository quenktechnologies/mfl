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
export default class Policy {

    constructor(fields) {

        this.__fields = fields;

    }

    _getFilter(field, op, value) {

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
    willEliminate(field, op, value) {

    }

    /**
     * enforce this policy
     * @param {string} field
     * @param {string} op
     * @param {*} value
     * @return {object}
     */
    enforce(field, op, value) {

        var spec;

        if (this.willEliminate(field, op, value))
            return null;

        spec = this.__fields[field];

        if( typeof spec === 'object' ) {

            op = spec.op || op;
            value = spec.value? spec.value(value) : value;

        }else if (typeof spec === 'function') {

            value = spec(value);

        } else if (typeof spec === 'string') {

            op = spec;

        }

        return this._getFilter(field, op, value);

    }

}
