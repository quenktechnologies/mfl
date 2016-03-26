export default {
    'should parse a single filter': {
        "type": "query",
        "filters": {
            "OR": [],
            "AND": [{
                "type": "filter",
                "field": "type",
                "operator": "=",
                "value": "c"
            }]
        }
    },
    'should parse multiple filters': {
        "type": "query",
        "filters": {
            "OR": [],
            "AND": [{
                "type": "filter",
                "field": "stars",
                "operator": ">",
                "value": 22
            }, {
                "type": "filter",
                "field": "active",
                "operator": "=",
                "value": false
            }, {
                "type": "filter",
                "field": "name",
                "operator": "=",
                "value": "johan"
            }, {
                "type": "filter",
                "field": "type",
                "operator": "=",
                "value": "c"
            }]
        }
    },
    'should parse with all basic operators': {
        "type": "query",
        "filters": {
            "OR": [],
            "AND": [{
                "type": "filter",
                "field": "name",
                "operator": "?",
                "value": "Product name"
            }, {
                "type": "filter",
                "field": "discount",
                "operator": "<=",
                "value": 5.4
            }, {
                "type": "filter",
                "field": "price",
                "operator": ">=",
                "value": 22.4
            }, {
                "type": "filter",
                "field": "rank",
                "operator": "<",
                "value": 23
            }, {
                "type": "filter",
                "field": "age",
                "operator": ">",
                "value": 14
            }]
        }
    },
    'should parse with the OR operator': {
        "type": "query",
        "filters": {
            "OR": [{
                "type": "filter",
                "field": "tag",
                "operator": "=",
                "value": "new"
            }, {
                "type": "filter",
                "field": "tag",
                "operator": "=",
                "value": "old"
            }],
            "AND": [{
                "type": "filter",
                "field": "filetype",
                "operator": "=",
                "value": "jpeg"
            }, {
                "type": "filter",
                "field": "user",
                "operator": "=",
                "value": "grandma"
            }]
        }
    }
};
