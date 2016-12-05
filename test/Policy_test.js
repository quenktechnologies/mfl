import must from 'must';
import Policy from '../src/Policy';

const fields = {

    name: true,
    age: false,
    role: v => v.map(m => m.toUpperCase()),
    location: '='

};

class Child extends Policy {

    willEliminate(field, op, value) {

        return !this.__fields[field];

    }

}

var policy;

describe('Policy', function() {

    beforeEach(function() {

        policy = new Child(fields);

    });

    it('should return the filter for allowed fields', function() {

        must(policy.enforce('name', '=', 'reed')).eql({ name: 'reed' });

    });

    it('should return null for eliminated fields', function() {

        must(policy.enforce('age', '>=', 22)).eql(null);

    });

    it('should apply a transform function', function() {

        must(policy.enforce('role', '$in', ['admin'])).eql({ role: { $in: ['ADMIN'] } });

    });

    it('should force the operator if specified', function() {

        must(policy.enforce('location', '?', 'trin')).eql({ location: 'trin'});

    });

});
