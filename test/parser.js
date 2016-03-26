import must from 'must';
import expects from './expectations';
import {
    parser
}
from '../src/parser';

var input = null;
var result = null;

function parse(text) {
    result = parser.parse(text || input);
}

describe('Parser', function() {

    beforeEach(function() {

        input = null;
        result = null;

    });

    describe('parse()', function() {

        it('should parse a single filter', function() {

            input = 'type:c';
            parse();
            must(result).eql(expects[this.test.title]);

        });

        it('should parse multiple filters', function() {

            input = 'type:c name:johan active:false stars:>22';
            parse();
            must(result).eql(expects[this.test.title]);

        });

        it('should parse with all basic operators', function() {

            input = 'age:>14 rank:<23 price:>=22.40 discount:<=5.40 name:?"Product name"';
            parse();
            must(result).eql(expects[this.test.title]);

        });

        it('should parse with the OR operator', function() {

            input = 'tag:old OR tag:new OR user:?grandma OR filetype:jpeg';
            parse();
            must(result).eql(expects[this.test.title]);

        });

    });

});
