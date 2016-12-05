# MFL

MongoDB Filter Language

This is a simple string parser than generates MongoDB compatiable filter
objects from a string.

Use it for search forms or api endpoint queries.


Turns:
`type:c created_by:ffan client.name:?hospital`

into:

```javascript

{
  type: 'c',
  created_by: 'ffan',
  'client.name': {$regex:new RegExp('hospital'), options:'i'}
}

```

#License

Apache-2.0

