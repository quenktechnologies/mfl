import must from 'must';
import * as mqfl from '../src/lib';
import asts from './asts';

describe('convert', function() {

    it('should not convert if not in map', function() {

        must(mqfl.convert(asts['should parse multiple filters'], {
            type: 1,
            name: 1,
            active: 1
        })).eql({
            '$and': [{
                type: 'c'
            }, {
                name: 'johan'
            }, {
                active: false
            }]
        });


    });



});
