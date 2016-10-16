##
# file: parsers-spec.coffee
# author: @MarcoThePoro
#

{xmlparser, xmlcdataparser, xmlcommentparser} = require "../lib/parsers"

describe "xmlparser", ->
  describe "trigger", ->
    it "triggers a close on </", ->
      expect('</'.match(xmlparser.trigger)?[0]).toEqual '</'

    it "doesn't trigger for anything else", ->
      expect('<//'.match(xmlparser.trigger)?[0]).toBe undefined

  describe "parse", ->
    it "parses an opening tag", ->
      expect(xmlparser.parse('<div>')).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'div',
        type: 'xml'
        length: 5
      }

    it "parses a closing tag", ->
      expect(xmlparser.parse('</div>')).toEqual {
        opening: false
        closing: true
        selfClosing: false
        element: 'div'
        type: 'xml'
        length: 6
      }

    it "parses self closing tags", ->
      expect(xmlparser.parse('<br/>')).toEqual {
        opening: false
        closing: false
        selfClosing: true
        element: 'br'
        type: 'xml'
        length: 5
      }

    it "returns null when there is no tag", ->
      expect(xmlparser.parse('No tag here!')).toBe null

    it "works around element properties", ->
      expect(xmlparser.parse('<div class="container">')).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'div'
        type: 'xml'
        length: 23
      }

    it "doesn't care which quotes you use", ->
      expect(xmlparser.parse("<div class='container'>")).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'div'
        type: 'xml'
        length: 23
      }
      expect(xmlparser.parse("<div class=`container`>")).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'div'
        type: 'xml'
        length: 23
      }

    it "plays nicely with multiline namespaced attributes", ->
      text = "<elem\n ns1:attr1=\"text\"\n  ns2:attr2=\"text\"\n>"
      expect(xmlparser.parse(text)).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'elem'
        type: 'xml'
        length: 44
      }

    it "works around weird spacing", ->
      text = "<div  class=\"container\" \n  foo=\"bar\">"
      expect(xmlparser.parse(text)).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'div'
        type: 'xml'
        length: 37
      }

    it "works around lone properties", ->
      text = "<input type=\"text\" required/>"
      expect(xmlparser.parse(text)).toEqual {
        opening: false
        closing: false
        selfClosing: true
        element: 'input'
        type: 'xml'
        length: 29
      }

    it "doesn't have a cow when properties contain a '>'", ->
      text = "<p ng-show=\"3 > 5\">Uh oh!"
      expect(xmlparser.parse(text)).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'p'
        type: 'xml'
        length: 19
      }

    it "finds the expected tag when tags are nested", ->
      text = "<a><i>"
      expect(xmlparser.parse(text)).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'a'
        type: 'xml'
        length: 3
      }

    it "finds the expected tag when tags with attributes are nested", ->
      text = "<a href=\"#\"><i class=\"fa fa-home\">"
      expect(xmlparser.parse(text)).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'a'
        type: 'xml'
        length: 12
      }

    it "does not capture leading `<`s", ->
      text = "<<<<p>I love less than!"
      expect(xmlparser.parse(text)).toEqual null

    it "does not capture opening comments in front of tags", ->
      text = "<!--<div>"
      expect(xmlparser.parse(text)).toEqual null

    it "does not capture comments", ->
      text = "<!-- COMMENT -->>"
      expect(xmlparser.parse(text)).toEqual null

  describe "getPair", ->
    it "returns the appropriate closing tag", ->
      expect(xmlparser.getPair(xmlparser.parse('<div>'))).toBe('</div>')

describe "xmlcdataparser", ->
  describe "trigger", ->
    it "triggers a close on ]]", ->
      expect(']]'.match(xmlcdataparser.trigger)?[0]).toBe ']]'

    it "doesn't trigger on anything else", ->
      expect(']['.match(xmlcdataparser.trigger)?[0]).toBe undefined

  describe "parse", ->
    it "parses a cdata opening", ->
      expect(xmlcdataparser.parse('<![CDATA[')).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'xml-cdata'
        type: 'xml-cdata'
        length: 9
      }

    it "parses a cdata closing", ->
      expect(xmlcdataparser.parse(']]>')).toEqual {
        opening: false
        closing: true
        selfClosing: false
        element: 'xml-cdata'
        type: 'xml-cdata'
        length: 3
      }

    it "returns null when there is no cdata", ->
      expect(xmlcdataparser.parse('no cdata here ┐(ﾟ～ﾟ)┌')).toBe null

  describe "getPair", ->
    it "returns ]]>", ->
      expect(xmlcdataparser.getPair(xmlcdataparser.parse('<![CDATA['))).toBe ']]>'

describe "xmlcommentparser", ->
  describe "trigger", ->
    it "triggers a close on --", ->
      expect(xmlcommentparser.trigger('--')).toBe '--'

    it "doesn't trigger on anything else", ->
      expect(xmlcommentparser.trigger('┐(ﾟ～ﾟ)┌')).toBe null

    it "doesn't trigger on <!--", ->
      expect(xmlcommentparser.trigger('<!--')).toBe null
      expect(xmlcommentparser.trigger('<!---')).toBe null
      expect(xmlcommentparser.trigger('<!----')).toBe '--'

  describe "parse", ->
    it "parses a comment opening", ->
      expect(xmlcommentparser.parse('<!--')).toEqual {
        opening: true
        closing: false
        selfClosing: false
        element: 'xml-comment'
        type: 'xml-comment'
        length: 4
      }

    it "parses a comment closing", ->
      expect(xmlcommentparser.parse('-->')).toEqual {
        opening: false
        closing: true
        selfClosing: false
        element: 'xml-comment'
        type: 'xml-comment'
        length: 3
      }

    it "returns null when there is no comment", ->
      expect(xmlcommentparser.parse('no comment here ┐(ﾟ～ﾟ)┌')).toBe null
      expect(xmlcommentparser.parse("<comment>Hah! You thought this was a comment!")).toBe null
