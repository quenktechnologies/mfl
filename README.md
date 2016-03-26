# MQFL

Mongo Query Filter Language

This is an attempt to provide a super simple syntax for generating the
filtering parts of a MongoDB query.

Ideally it would allow you to turn:
`type:c created_by:ffan client.name:?hospital`

into:

```javascript

{
  type: 'c',
  created_by: 'ffan',
  'client.name': {$regex:new RegExp('hospital')}
}

```
after some processing of course.

#License

Apache-2.0

