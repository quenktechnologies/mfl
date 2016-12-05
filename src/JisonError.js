export default function JisonError(e) {

    Object.keys(e).forEach(k => this[k] = e[k]);

    this.stack = (new Error(e.message)).stack;

    if (Error.hasOwnProperty('captureStackTrace'))
        Error.captureStackTrace(this, this.constructor);

    Object.defineProperties(this, {

        message: {
            configurable: true,
            enumerable: true,
            writable: true,
            value: e.message
        }
    });

}

JisonError.prototype = Object.create(Error.prototype);
JisonError.prototype.message = '';
JisonError.prototype.constructor = JisonError;


