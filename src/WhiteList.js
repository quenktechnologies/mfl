import Policy from './Policy';

/**
 * WhiteList discards any fields that have not been explicitly decleared.
 */
export default class WhiteList extends Policy {

    willEliminate(field, op, value) {

        if(!this.__fields.hasOwnProperty(field))
            return true;

        if(this.__fields[field] === false)
            return true;

    }

}

