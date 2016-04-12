import must from 'must';
import expects from './asts';
import * as mqfl from '../src/lib';

var input = null;
var result = null;

function parse(text) {
    result = mqfl.parse(text || input);
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

        it('should parse the $in function', function() {

            input = 'tag:[24, 88.9,"mumch", 23.5, "Cake mix"]';
            parse();
            must(result).eql(expects[this.test.title]);

        });

    });

});
