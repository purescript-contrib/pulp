# Pulp

A very rudimentary build tool for PureScript.

![Jarvis Cocker dancing](http://24.media.tumblr.com/77b76c557515a801a7e99ca5507b6548/tumblr_n5cx52oT831r4ba6to1_400.gif)

## Installation

```sh
$ cabal install purescript
$ npm install -g pulp
```

## Usage

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
<http://bower.io/#defining-a-package>). `pulp` will need to be run
from the root directory of your project, and it will need to find the
`bower.json` file there.

`pulp` currently does three things:

* `pulp install` will install dependencies as given in the
  `bower.json` file. It essentially just invokes `bower install`.
* `pulp build` invokes the PureScript compiler. Currently, it just
  compiles all `.purs` files in your `src` and dependencies, and
  outputs it as `out.js`.
* `pulp test` runs your test suite, which right now means it compiles
  your project, including files in the `test` directory, into a file
  called `test.js`, then invokes that file using Node.

You were warned that it was rudimentary.

## License

Copyright 2014 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

See the [LICENSE](LICENSE.md) file for further details.
