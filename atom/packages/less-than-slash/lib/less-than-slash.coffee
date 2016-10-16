##
# file: less-than-slash.coffee
# author: @mrhanlon
#

{
  xmlparser,
  xmlcdataparser,
  xmlcommentparser,
  underscoretemplateparser,
  mustacheparser
} = require './parsers'

module.exports =

  parsers: [
    xmlparser,
    xmlcdataparser,
    xmlcommentparser,
    underscoretemplateparser,
    mustacheparser
  ]

  disposable: {}

  config:
    completionMode:
      title: "Completion Mode"
      description: "Choose immediate to have your tags completed immediately after you type '</' (the traditional way). Choose suggest to have them appear in an autocomplete suggestion box."
      type: "string",
      default: "Immediate"
      enum: ["Immediate", "Suggest"]
      order: 1
    emptyTags:
      title: "Empty tags"
      description: "A space separated list of elements to be ignored from auto-closing."
      type: "string"
      default: [
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
      ].join(" ")
      order: 2
    returnCursor:
      title: "Return cursor"
      description: "Returns the cursor to the beginning of the closing tag after it's been inserted (does not work in suggest mode)"
      type: "boolean"
      default: false
      order: 3

  deactivate: (state) ->
    for key in Object.keys @disposable
      @disposable[key].dispose()
      delete @disposable[key]

  activate: (state) ->
    # Register config change handler to update the empty tags list
    atom.config.observe "less-than-slash.emptyTags", (value) ->
      xmlparser.emptyTags = (tag.toLowerCase() for tag in value.split(/\s*[\s,|]+\s*/))
    atom.config.observe "less-than-slash.completionMode", (value) =>
      mustacheparser.omitClosingBraces = value.toLowerCase() is "immediate"
      @forceComplete = value.toLowerCase() is "immediate"
    atom.config.observe "less-than-slash.returnCursor", (value) =>
      @returnCursor = value

    @disposable._root = atom.workspace.observeTextEditors (editor) =>
      buffer = editor.getBuffer()
      if not @disposable[buffer.id]
        @disposable[buffer.id] = buffer.onDidChange (event) =>
          if event.newText is '' then return
          if not @forceComplete then return
          if prefix = @getPrefix(editor, event.newRange.end, @parsers)
            if completion = @getCompletion(editor, event.newRange.end, prefix)
              buffer.delete [
                [event.newRange.end.row, event.newRange.end.column - prefix.length]
                event.newRange.end
              ]
              buffer.insert [event.newRange.end.row, event.newRange.end.column - prefix.length], completion
              # If we inserted a mustache closing tag, we need to advance the
              # cursor past the automatically inserted `}}`
              if (prefix is "{{/" and @forceComplete and not @returnCursor)
                editor.moveRight(2)
              if @returnCursor
                editor.moveLeft(completion.length)

        buffer.onDidDestroy (event) =>
          if @disposable[buffer.id]
            @disposable[buffer.id].dispose()
            delete @disposable[buffer.id]

    @provider =
      selector: ".text, .source"
      inclusionPriority: 1
      excludeLowerPriority: false
      getSuggestions: ({editor, bufferPosition, scopeDescriptor, activatedManually}) =>
        if @forceComplete then return []
        if prefix = @getPrefix(editor, bufferPosition, @parsers)
          if completion = @getCompletion editor, bufferPosition, prefix
            return [{
              text: completion
              prefix: unless activatedManually then prefix else undefined
              type: 'tag'
            }]

  getCompletion: (editor, bufferPosition, prefix) ->
    text = editor.getTextInRange [[0, 0], bufferPosition]
    unclosedTags = @reduceTags(@traverse(text, @parsers))
    if tagDescriptor = unclosedTags.pop()
    # Check that this completion corresponds to the trigger
      if @matchPrefix(prefix, @getParser(tagDescriptor.type, @parsers))
        return @getParser(tagDescriptor.type, @parsers).getPair(tagDescriptor)
    return null

  provide: ->
    @provider

  # Pure logic

  traverse: (text, parsers) ->
    tags = []
    loop
      if text is ''
        break
      newIndex = 1
      for index, parser of parsers
        if text.match(parser.test)
          if tagDescriptor = parser.parse(text)
            tags.push tagDescriptor
            newIndex = tagDescriptor.length
            break
      text = text.substr newIndex
    tags

  reduceTags: (tags) ->
    result = []
    loop
      tag = tags.shift()
      if not tag then break
      switch
        when tag.opening
          result.push tag
        when tag.closing
          _result = result.slice()
          foundMatchingTag = false
          while result.length
            previous = result.pop()
            if previous.element is tag.element and previous.type is tag.type
              foundMatchingTag = true
              break
          unless foundMatchingTag
            result = _result
        when tag.selfClosing
        else
          throw new Error("Invalid parse")
    result

  # Utils
  getPrefix: (editor, bufferPosition, parsers) ->
    line = editor.getTextInRange([[bufferPosition.row, 0], bufferPosition])
    for index, parser of parsers
      if match = @matchPrefix line, parser
        return match
    return false

  matchPrefix: (text, parser) ->
    if typeof parser.trigger is 'function' then parser.trigger(text) else text.match(parser.trigger)?[0]

  getParser: (name, parsers) ->
    for index, parser of parsers
      if parser.name is name
        return parser
    null
