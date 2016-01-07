# Pulp Release History

## 7.0.0

* Remove the `--engine` option, since the `--runtime` option fulfils the same
  need.
* Fix the `--runtime` option, which was previously broken. (#143)
* Fix a bug where Pulp was sometimes using terminal ANSI codes for colours
  when it shouldn't have been, and not using them when it should. (#147)
* Relay interrupts (Ctrl+C) to `psci` when running `pulp psci`, so that
  long-running commands can be interrupted, and to stop the "hGetChar:
  hardware fault (Input/output error)" message from being shown. (#88)

## 6.2.1

* Fix the `--watch` option, which was broken in 6.2.0.
* Remove the `--optimise` option for `pulp test` and `pulp server`, since it
  doesn't really make sense with these commands.

## 6.2.0

* `pulp dep` is now deprecated. It continues to work as before, but
  you will have to install Bower yourself in order to use it. It’s
  recommended that you use Bower directly instead.
* New global options `--before` and `--else` to complement `--then`.
* `--skip-entry-point` now works when using `pulp build --to`.

## 6.1.0

* You can now use `pulp browserify --standalone <module-name>` to
  produce a browserified bundle wrapped in a UMD header, which can be
  `require()`d, and which re-exports the main module of your
  PureScript project. It works by invoking `browserify --standalone`;
  see
  [the Browserify documentation](https://github.com/substack/node-browserify#usage).

## 6.0.1

* Remove unnecessary `postinstall` script.

## 6.0.0

* Pulp has been ported to PureScript :)

* The `--with-deps` flag for `pulp docs` has been renamed to
  `--with-dependencies`.

Bugs fixed: #123, #122, #121, #111, #108, #92.

## 5.0.2

Bugs fixed: #109.

## 5.0.1

Bugs fixed: #105, #106, #107, #113.

## 5.0.0

* Pulp will now skip the build step if your project hasn't changed
  since your last rebuild.

* The `--force` flag is now avaliable on all commands that trigger a
  build, and will force a rebuild regardless of whether your project
  has changed. It still also forces a non-incremental build when
  used with `pulp browserify`.

* You can now use the flags `--src-path`, `--test-path` and
  `--dependency-path` to override the normal locations for these
  directories.

* The format for passing multiple directories to `--include` has
  changed: you now separate directories using the standard system path
  delimiter, as opposed to spaces.
