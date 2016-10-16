##
# file: parsers.coffee
# author: @marcotheporo
#

# Parser schema
# name: <string> The name of the parser
# trigger: <RegExp|func (string) -> <string>> A RegExp that captures the trigger in
# group 0 or a function that takes the text and returns the trigger or null
# test: <RegExp> determines whether the given string is a candidate
#   for a full parse, (opening or closing)
# parse: <func (string) -> <TagDescriptor|null>> A function that parses a tag from
#   the front of the given text or rejects by returning null
# getPair: <func (TagDescriptor) -> <string>> renders a closing tag to match
#   that given by the TagDescriptor
#
# TagDescriptor Schema
# opening: <bool>
# closing: <bool>
# selfClosing: <bool>
# element: <string> Whilst required, this property doesn't apply to all types
#   of tags, for example there is only one variety of html comment. If you don't
#   have any unique data to put in here, use the tag type
# type: <string> it's best to just put the parser name in here
# length: <number>
#

module.exports =
  xmlparser:
    name: "xml"
    trigger: /<\/$/
    test: /^</
    parse: (text) ->
      result = {
        opening: false
        closing: false
        selfClosing: false
        element: ''
        type: @name
        length: 0
      }
      match = text.match(/^<(\/)?([^\s\/<>!][^\s\/<>]*)(\s+([\w-:]+)(=["'`{](.*?)["'`}])?)*\s*(\/)?>/i)
      if match
        result.element     = match[2]
        result.length      = match[0].length
        if @emptyTags.indexOf(result.element.toLowerCase()) > -1
          result.selfClosing = true
        else
          result.opening     = if match[1] or match[7] then false else true
          result.closing     = if match[1] then true else false
          result.selfClosing = if match[7] then true else false
        result
      else
        null
    getPair: (tagDescriptor) ->
      "</#{tagDescriptor.element}>"
    emptyTags: []
  xmlcdataparser:
    name: 'xml-cdata'
    trigger: /\]\]$/
    test: /^(<!\[|]]>)/
    parse: (text) ->
      result = {
        opening: false
        closing: false
        selfClosing: false
        element: 'xml-cdata'
        type: @name
        length: 0
      }
      match = text.match(/(<!\[CDATA\[)|(\]\]>)/i)
      if match
        result.length  = match[0].length
        result.opening = if match[1] then true else false
        result.closing = if match[2] then true else false
        result
      else
        null
    getPair: ->
      return "]]>"
  xmlcommentparser:
    name: 'xml-comment'
    # FIXME tries to close the comment immediately after you open it
    # eg. Input: `<!--` Result: `<!-->`
    # DISABLED FOR NOW
    trigger: (text) ->
      # unless text then return false
      match = text.match /(<!-{1,3})$|(--)$/
      if (match and match[2]) then return match[2] else return null
    test: /^(<!--|-->)/
    parse: (text) ->
      result = {
        opening: false
        closing: false
        selfClosing: false
        element: 'xml-comment'
        type: @name
        length: 0
      }
      match = text.match(/(<!--|-->)/)
      if match
        result.length  = match[0].length
        result.opening = if match[1] is '<!--' then true else false
        result.closing = if match[1] is '-->' then true else false
        result
      else
        null
    getPair: ->
      return "-->"
  underscoretemplateparser:
    name: 'underscore-template',
    trigger: null
    test: /<%=.+?%>/
    parse: (text) ->
      {
        type: @name,
        selfClosing: true,
        length: text.match(@test)[0].length
      }
    getPair: null
  # DISABLED
  mustacheparser:
    name: 'mustache',
    trigger: /\{\{\/$/
    test: /^{{[\^\/#]/
    parse: (text) ->
      result = {
        opening: false
        closing: false
        selfClosing: false
        element: ''
        type: @name
        length: 0
      }
      match = text.match(/\{\{([#\/])([^\s]+?)(\s+?([^\s]+?))?(\s)*?\}\}/i)
      if match
        result.opening = if match[1] is '#' then true else false
        result.closing = not result.opening
        result.element = match[2]
        result.length = match[0].length
        return result
      else
        return null
    getPair: (tagDescriptor) ->
      "{{/#{tagDescriptor.element}#{unless @omitClosingBraces then "}}" else ""}"
    omitClosingBraces: false
