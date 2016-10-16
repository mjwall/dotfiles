##
# file: less-than-slash-spec.coffee
# author: @mrhanlon
#

{reduceTags, traverse, getParser} = require '../lib/less-than-slash.coffee'
{xmlparser} = require '../lib/parsers.coffee'

describe "LessThanSlash", ->
  parsers = [xmlparser]
  xmlparser.emptyTags = [
    "!doctype",
    "br",
    "hr",
    "img",
    "input",
    "link",
    "meta",
    "area",
    "base",
    "col",
    "command",
    "embed",
    "keygen",
    "param",
    "source",
    "track",
    "wbr"
  ]

  describe "getParser", ->
    it 'returns the parser based on the type', ->
      parser = {name: 'a'}
      expect(getParser('a', [parser])).toEqual(parser)

  describe "traverse", ->
    it "converts a string into a list of tag descriptors", ->
      expect(traverse('<a></a>', parsers)).toEqual([
        getParser('xml', parsers).parse('<a>'),
        getParser('xml', parsers).parse('</a>')
      ])
      expect(traverse('<a></a>', parsers)).toEqual([
        getParser('xml', parsers).parse('<a>'),
        getParser('xml', parsers).parse('</a>')
      ])

  describe "reduceTags", ->
    it "matches tags, leaving only those that are unclosed", ->
      expect(reduceTags(traverse('<a>', parsers))).toEqual([
        getParser('xml', parsers).parse('<a>')
      ])
      expect(reduceTags(traverse('<a></a>', parsers))).toEqual([])
      expect(reduceTags(traverse('<a><div></a>', parsers))).toEqual([])
      expect(reduceTags(traverse('<a><div></div></a>', parsers))).toEqual([])
      expect(reduceTags(traverse('<a><div><div></div></div></a>', parsers))).toEqual([])
      expect(reduceTags(traverse('<a><div><div>', parsers))).toEqual([
        getParser('xml', parsers).parse('<a>')
        getParser('xml', parsers).parse('<div>')
        getParser('xml', parsers).parse('<div>')
      ])
      expect(reduceTags(traverse('<a><div><i></div>', parsers))).toEqual([
        getParser('xml', parsers).parse('<a>')
      ])
      expect(reduceTags(traverse('<a></a><div>', parsers))).toEqual([
        getParser('xml', parsers).parse('<div>')
      ])
