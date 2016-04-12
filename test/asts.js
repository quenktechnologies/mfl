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
        'type': 'query',
        'filters': {
            'AND': [{
                'type': 'filter',
                'field': 'type',
                'operator': '=',
                'value': 'c'
            }, {
                'type': 'filter',
                'field': 'name',
                'operator': '=',
                'value': 'johan'
            }, {
                'type': 'filter',
                'field': 'active',
                'operator': '=',
                'value': false
            }, {
                'type': 'filter',
                'field': 'stars',
                'operator': '>',
                'value': 22
            }],
            'OR': []
        }
    },
    'should parse with all basic operators': {
        'type': 'query',
        'filters': {
            'AND': [{
                'type': 'filter',
                'field': 'age',
                'operator': '>',
                'value': 14
            }, {
                'type': 'filter',
                'field': 'rank',
                'operator': '<',
                'value': 23
            }, {
                'type': 'filter',
                'field': 'price',
                'operator': '>=',
                'value': 22.4
            }, {
                'type': 'filter',
                'field': 'discount',
                'operator': '<=',
                'value': 5.4
            }, {
                'type': 'filter',
                'field': 'name',
                'operator': '?',
                'value': 'Product name'
            }],
            'OR': []
        }
    },
    'should parse with the OR operator': {
        'type': 'query',
        'filters': {
            'AND': [],
            'OR': [{
                'type': 'filter',
                'field': 'tag',
                'operator': '=',
                'value': 'old'
            }, {
                'type': 'filter',
                'field': 'tag',
                'operator': '=',
                'value': 'new'
            }, {
                'type': 'filter',
                'field': 'user',
                'operator': '?',
                'value': 'grandma'
            }, {
                'type': 'filter',
                'field': 'filetype',
                'operator': '=',
                'value': 'jpeg'
            }]
        }
    },
    'should parse the $in function':
{"type":"query","filters":{"AND":[{"type":"filter","field":"tag","operator":"$in","value":[24,88.9,"mumch",23.5,"Cake mix"]}],"OR":[]}}

};
