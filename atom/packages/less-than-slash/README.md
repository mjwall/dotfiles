# </ Less Than-Slash

[![Build Status](https://travis-ci.org/mrhanlon/less-than-slash.png)](https://travis-ci.org/mrhanlon/less-than-slash)

Atom.io package for closing open tags when less-than, slash (`</`) is typed, like in Sublime Text 3.


![Less Than Slash](https://mrhanlon.github.io/images/less-than-slash.gif)

## Installation

`apm install less-than-slash`

## Settings

### Completion Mode

You can choose between _Immediate Mode_ (default) to have tags closed seamlessly after you type `</`, or _Suggest Mode_, where less-than-slash acts as a smarter [autocomplete-plus](https://github.com/atom/autocomplete-plus) provider, suggesting the appropriate closing tag when you type `</`.

### Empty Tags

You can specify a list of "Empty Tags" to be ignored from auto-closing. The default value for "Empty Tags" is:

`!doctype`, `br`, `hr`, `img`, `input`, `link`, `meta`, `area`, `base`, `col`, `command`, `embed`, `keygen`, `param`, `source`, `track`, `wbr`

The plugin will automatically ignore any of these self-closing tags. This is useful for frameworks like Angular.js, which allows the definition of custom elements.

### Return cursor

Returns the cursor to the beginning of the closing tag so you can write both the opening and closing tags before writing code in between them. (disabled by default)

## Contributing

Please follow the guidelines for [Contributing to Atom](https://atom.io/docs/latest/contributing).
