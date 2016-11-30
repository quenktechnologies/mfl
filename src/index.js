import beof from 'beof';
import {
  parse,
  convert,
  JisonError
} from './lib';

/**
 * Filter represents the filter to be applied to a query.
 * This class api provides methods for manipulating the criteria
 * outside of this library so additional fields etc. can be added.
 * @param {object} fields
 */
class Filter {

  constructor(fields = {}) {

    Object.keys(fields).forEach(k => {

      if (this[k] == null)
        this[k] = fields[k];

    });

  }

  static fromString(string = '', allowed={}, defaults = {}) {

    beof({
      string
    }).string();
    beof({
      allowed
    }).object();
    beof({
      defaults
    }).optional().object();

    try {

      return new this(convert(parse(string), allowed) || defaults);

    } catch (e) {

      if (e instanceof JisonError)
        return new this(defaults);

      throw e;

    }

  }

  /**
   * or constructs a Filter using a sequence of or conditions.
   * @param {object} ...filter
   */
  static or() {

    return new Filter({
      $or: [...arguments]
    });

  }


  /**
   * and constructs a Filter using a sequence of and conditions.
   * @param {object} ...filter
   */
  static and() {

    return new Filter({
      $and: [...arguments]
    });

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

  asValue() {

    return this;

  }

  toJSON() {

    return this.asValue();

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

export {
  JisonError
}
export default Filter

