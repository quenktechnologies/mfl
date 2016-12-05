import must from 'must';
import WhiteList from '../src/WhiteList';

var policy;

describe('WhiteList', function() {


    it('should ignore unknown fields', function() {

        policy = new WhiteList({ name: true, age: true, sex: false });

        must(policy.enforce('name', '?', 'Kaw')).eql({ name: { '$regex': 'Kaw', '$options': 'i' } });
        must(policy.enforce('age', '>', 23)).eql({ age: { $gt: 23 } });
        must(policy.enforce('sex', '=', 'male')).eql(null);
        must(policy.enforce('role', '$in', [])).eql(null);

    });

});
