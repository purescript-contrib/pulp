# <img align="right" src="https://travis-ci.org/bodil/pulp.svg?branch=master"> Pulp

A build tool for PureScript.

![Jarvis Cocker dancing](http://24.media.tumblr.com/77b76c557515a801a7e99ca5507b6548/tumblr_n5cx52oT831r4ba6to1_400.gif)

## Installation

```sh
$ npm install -g purescript pulp
```

## Project Structure

The structure of a `pulp` project should look like this:

```
  root
  - bower.json
  - src/
  - test/
```

Put your `.purs` source files in the `src` directory, and any tests
you might have in the `test` directory.

Also, create a `bower.json` file (see
<http://bower.io/#defining-a-package>). `pulp` expects to find this
file in the root of your project. You can invoke `pulp` from a
subdirectory; if there is no `bower.json` to be found in the current
directory, `pulp` will look through parent directories until it finds
it.

## Commands

`pulp` supports the following commands:

* `pulp init` generates a project skeleton with a `bower.json` file
  and a simple example program.
* `pulp dep` does dependency management through Bower; essentially, it
  just passes you on to a locally installed version of `bower`. Thus,
  `pulp dep install foo --save` is the equivalent of `bower install
  foo --save` except you don't need to install Bower globally.
* `pulp build` invokes the PureScript compiler. Currently, it just
  compiles all `.purs` files in your `src` and dependencies into the
  target directory, which defaults to `output`.
* `pulp test` runs your test suite: it expects a `Test.Main` package
  in the `test` directory, containing a `main` function, which is
  required and run using Node. It's expected that failing tests will
  cause the program to terminate with an error. `Test.QuickCheck`
  works well for this purpose.
* `pulp run` will first run `pulp build`, then launch the compiled
  project code in a Node process. The entry point will be the `main`
  function in the module specified with the `--main` option, or, by
  default, the module `Main`.
* `pulp browserify` also runs `pulp build`, then runs the project code
  through [Browserify](http://browserify.org/). The entry point is
  decided in the same way as with `pulp run`. You can specify an
  output file using `--to`; the default is to output the bundle to
  stdout, which is convenient for doing things like `pulp browserify |
  uglifyjs -c`.

  If you want to browserify your test suite, e.g. if you want to run
  tests in the browser, you can run `pulp browserify -I test --main
  Test.Main`.
* `pulp docs` generates a project documentation file using
  PureScript's `psc-docs` command.
* `pulp psci` launches a PureScript REPL using `psci` with the
  project's modules and dependencies installed.

## Watch and restart

You can launch any of the above commands with the `--watch` or `-w`
option, which will cause `pulp` to run indefinitely, watching your
`src` and `test` folders for changes, and re-running the command
whenever something changes.

## License

Copyright 2014 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

See the [LICENSE](LICENSE.md) file for further details.
