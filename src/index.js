import * as lib from './lib';

export default function parse(src, map) {

    return lib.convert(lib.parse(src), map);

}
