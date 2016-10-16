### V0.15.0

Enhancements:

- Added 'return cursor' setting. When enabled, the cursor will be placed 
  just closed tag. This allows to you write both the opening and closing
  tag first, then return the cursor to insert between the just-typed 
  tags; #37.

Bug fixes: 

- XML Parser closes comment tags; #39.

### V0.14.0

Enhancements:

- Autocomplete plus integration #24

Bug fixes: 

- Editor crashes when <br> closed #36

### v0.13.0

Bug fixes:

- Fix disposal #29
- Fix issue with "double less-than's" #32

### v0.12.3

- Fix multiple event subscription when splitting editor panes. #27

### v0.12.2

- Fix bug when closing a split pane. #26

### v0.12.1

- Add `deactivate` handler and properly handle disposables to prevent multiple event
  subscription. #22 #23 #25

### v0.12.0

- Add support for underscore templates. #20

### v0.11.0

- Updated `findUnclosedTags` function to use `while` loop instead of recursion. On very
  long files the recursion caused `RangeError: Maximum call stack size exceeded`. #18

### v0.10.0

- `findUnclosedTags` is now aware of other types of tags. #16

### v0.9.0

- Refactor to easily support multiple/future tag parsers
  - Thanks [MarcoThePoro](https://github.com/MarcoThePoro)!

### v0.8.0

- Support for attributes spaced across multiple lines
- Proper tag handling inside of comments
- Support for CDATA

### v0.7.0

- Fixed deprecation issue in specs
- Add support for curly braces in property values; #12

### v0.6.0

Fixed deprecation warnings:

- Support new configuration schema

### v0.5.0

Fixed deprecation cop warnings:

- `TextBuffer.on` is deprecated; use `TextBuffer.onDidChange` instead
- Package styles should be in `/styles` not `/stylesheets`. However, less-than-slash doesn't have any styles so just nuked the practically empty stylesheet instead. :fire: :fire: :fire:

### v0.4.0

Now automatically ignores self-closed tags, e.g. `<my-element />`, without needing to specify in `@emptyTags`.

### v0.3.4

Now with more CoffeeScript!

### v0.3.3

Documentation update

### v0.3.2

Documentation update

### v0.3.1

Fix multi-line editing not always performing as expected

### v0.3.0

Remove deprecated API calls

### v0.2.0

Bug fixes

### v0.1.1

Bug fixes

### v0.1.0

Initial release
