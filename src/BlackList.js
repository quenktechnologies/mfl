import Policy from './Policy';

/**
 * BlackList discards any fields in the query that have a truthy
 * value in the 'fields' object. Any unspecified fields are kept
 * @param {object} fields A map of fields
 */
export default class BlackList extends Policy {

    willEliminate(field, op, value) {

        if (this.__fields[field] === true)
            return true;

    }

}
