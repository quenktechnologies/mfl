import must from 'must';
import BlackList from '../src/BlackList';

var policy;

describe('BlackList', function() {

    it('should include unknown fields', function() {

        policy = new BlackList({ name: false, age: false, sex: true });

        must(policy.enforce('name', '?', 'Kaw')).eql({ name: { '$regex': 'Kaw', '$options': 'i' } });
        must(policy.enforce('age', '>', 23)).eql({ age: { $gt: 23 } });
        must(policy.enforce('sex', '=', 'male')).eql(null);
        must(policy.enforce('role', '$in', [])).eql({ role: { $in: [] } });

    });

});
