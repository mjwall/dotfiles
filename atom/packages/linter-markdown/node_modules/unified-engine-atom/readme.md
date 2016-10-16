# unified-engine-atom [![Build Status][travis-badge]][travis]

<!--lint disable heading-increment no-duplicate-headings list-item-spacing-->

Interface for creating [Atom Linter][linter]s around
[**unified**][unified] processors.  Wrapper around the
[**engine**][engine] to run it from Atom.

## Installation

[npm][npm-install]:

```bash
npm install unified-engine-atom
```

## Usage

```js
/* Dependencies. */
var engine = require('unified-engine-atom');

/* Expose. */
module.exports.provideLinter = function () {
  return {
    grammarScopes: ['source.gfm', 'source.pfm', 'text.md'],
    name: 'remark',
    scope: 'file',
    lintOnFly: true,
    lint: engine({
      processor: require('remark'),
      rcName: '.remarkrc',
      packageField: 'remarkConfig',
      ignoreName: '.remarkignore',
      pluginPrefix: 'remark'
    })
  };
}
```

## API

### `engine(options)`

Create a `lint` function for use in an AtomLinter package.  Read more
about linters in the [Linter-API Wiki][wiki].

###### `options`

*   [`processor`][processor] ([`Processor`][unified-processor], required)
    — Unified processor to transform files.
*   [`rcName`][rc-name] (`string`, optional)
    — Name of configuration files to load.
*   [`packageField`][package-field] (`string`, optional)
    — Property at which configuration can be found in `package.json`
    files.
*   [`detectConfig`][detect-config] (`boolean`, default: whether
    `rcName` or `packageField` is given)
    — Whether to search for configuration files.
*   [`rcPath`][rc-path] (`string`, optional)
    — File-path to a configuration file to load.
*   [`settings`][settings] (`Object`, optional)
    — Configuration for the parser and compiler of the processor.
*   [`ignoreName`][ignore-name] (`string`, optional)
    — Name of ignore files to load.
*   [`detectIgnore`][detect-ignore] (`boolean`, default: whether
    `ignoreName` is given)
    — Whether to search for ignore files.
*   [`ignorePath`][ignore-path] (`string`, optional)
    — File-path to an ignore file to load.
*   [`silentlyIgnore`][silently-ignore] (`boolean`, default: `false`)
    — Skip given files if they are ignored.
*   [`plugins`][plugins] (`Object`, optional)
    — Map of plug-in names or paths and options to use.
*   [`pluginPrefix`][plugin-prefix] (`string`, optional)
    — When given, optional prefix to use when searching for plug-ins.
*   [`injectedPlugins`][injected-plugins] (`Array`, optional)
    — List of loaded plug-ins to use.

###### Returns

`Function` — Can be used as `provider.lint`, where `provider` is the
returned value of `provideLinter`.  This function takes an Atom `Editor`
instance, resolves an array of [`LinterMessages`][messages], or rejects
a fatal `Error`.

## Todo

*   [ ] If anyone knows how to add coverage to Atom tests, I’d love to
    hear about it.

## License

[MIT][license] © [Titus Wormer][author]

<!-- Definitions -->

[travis-badge]: https://img.shields.io/travis/wooorm/unified-engine-atom.svg

[travis]: https://travis-ci.org/wooorm/unified-engine-atom

[npm-install]: https://docs.npmjs.com/cli/install

[license]: LICENSE

[author]: http://wooorm.com

[unified]: https://github.com/wooorm/unified

[engine]: https://github.com/wooorm/unified-engine

[linter]: https://github.com/steelbrain/linter

[wiki]: https://github.com/steelbrain/linter/wiki/Linter-API

[messages]: https://github.com/steelbrain/linter/wiki/Linter-API#messages

[unified-processor]: https://github.com/wooorm/unified#processor

[processor]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsprocessor

[detect-config]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsdetectconfig

[rc-name]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsrcname

[package-field]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionspackagefield

[rc-path]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsrcpath

[settings]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionssettings

[detect-ignore]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsdetectignore

[ignore-name]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsignorename

[ignore-path]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsignorepath

[silently-ignore]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionssilentlyignore

[plugin-prefix]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionspluginprefix

[plugins]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsplugins

[injected-plugins]: https://github.com/wooorm/unified-engine/blob/master/doc/options.md#optionsinjectedplugins
