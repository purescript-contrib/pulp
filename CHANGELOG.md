# Pulp Release History

## 12.0.1

* Fix a bug where running commands with `--watch` would sometimes produce
  an internal error (@thoradam, #300).

## 12.0.0

* Add support for psc-package (@thoradam, #243). See the README for details.
* Check that a program's entry point is of a suitable type when bundling (see https://github.com/purescript/purescript/issues/2086). By default `main` is required to be of type `Eff`, but this can be controlled using the `--check-main-type` flag. Alternatively this check can be turned off entirely using the `--no-check-main` flag.
* Fix a bug where pulp would crash on uncommon operating systems (#299)
* Fix an error in the help message for the `--else` option (@tkawachi, #294)

## 11.0.2

* Fix a bug where running `pulp version` in a repo which didn't yet have any git tags would cause pulp to crash

## 11.0.1

* Allow empty paths for the `--include` option (@anilanar, #263)
* Various fixes to pulp's docs and `--help` output (@anttih)
* If psa is being used, check that it is not too old (#272)
* Use an exitcode of 1 on invalid options/commands (#285)
* Set Cache-Control: no-cache in `pulp server` (@geigerzaehler, #288)

## 11.0.0

* Compatibility with PureScript 0.11.x. Compatibility with 0.10.x and previous versions of the PureScript compiler has been dropped (@natefaubion).
* Create a .purs-repl file during pulp init, to automatically import Prelude in new projects (@chexxor).

## 10.0.4

* Fix an issue causing "EXDEV: cross-device link not permitted" errors in some configurations (#252).

## 10.0.3

Nothing changed this release, I just messed up the 10.0.2 release so published another patch-level update.

## 10.0.2

* Allow pulp to work with recent development builds of the PureScript compiler (#255, @sloosch).
* Fix a missing space character in a message during 'pulp run' (#256, @bionicbrian).

## 10.0.1

* Fix an issue where extra command line arguments were not being passed to test programs properly (#239, @mcoffin).

## 10.0.0

### Breaking changes

* Explicit separation of passthrough arguments (#220). The original behaviour
  that unrecognised arguments were passed through to `psc` (or whichever
  underlying program `pulp` calls) has turned out to be confusing. Instead,
  passthrough arguments are now separated from pulp's arguments with a `--`.
  Any unrecognised arguments before the `--` will now cause pulp to complain.
* `pulp server` has been broken since 9.0.0 and is now fixed! It also no longer
  uses webpack and purs-loader, instead it builds with the same mechanisms that
  `pulp build` uses, which should make things more reliable and easier (#151).
  This is a breaking change because some command line options for `pulp server`
  were removed as they were webpack-specific and therefore no longer
  applicable.
* Remove options from `pulp run` which were not applicable and should never
  have been there: `--skip-entry-point`, `--to`, and `--optimise`.
* Remove `pulp dep` (#234). `pulp dep` has been deprecated for quite a long
  time now.

### Other changes

* Add `--jobs` for specifying parallelism in `psc` (#93).
* Fix swallowing of "Compiling \<module\>" messages from `psc`.
* Stop hardcoding version ranges in the initial bower.json created by `pulp
  init` (#231). Now, pulp delegates to `bower install --save` to select an
  appropriate version range.
* Fix a bug where some arguments were mistakenly dropped instead of being
  passed to `psc-bundle` (#188).

## 9.1.0

* Ignore .psc-ide-port, .psa-stash, and other dotfiles beginning with .psc or .psa in the default .gitignore file created by `pulp init` (@texastoland, #223, 225).
* Bump version ranges in the default bower.json file created by `pulp init` to pick up the newest versions of core libraries (@Risto-Stevcev, #230).
* Updated some npm dependencies to reduce the number of warnings you get when you run `npm install pulp`.

## 9.0.1

* Improved error messages in the case where submitting a package to Pursuit as
  part of `pulp publish` fails.
* Update README to use `bower uninstall` instead of the undocumented and
  presumably deprecated `bower rm` (@menelaos, #215).

## 9.0.0

* Compatibility with version 0.9 of the PureScript compiler. Pulp no longer
  works with earlier versions of the PureScript compiler; to use earlier
  versions, you will need to downgrade to a previous version of Pulp.
* Fix a bug where the version of psc being used was not being printed properly
  (#210).

## 8.2.1

* Remove unused npm dependencies (`xhr2`, `ansi`, and `supports-color`).
* Fix Pulp's description in package.json; Pulp is not a package manager.

## 8.2.0

* Update the dependency on `watchpack` to fix deprecation warnings (#196).
* Add a `--no-push` flag to `pulp publish`, allowing you to skip pushing
  tags to a Git remote as part of publishing (#201).
* Add a `--push-to` option to `pulp publish`, allowing you to push to a
  specific Git remote (#201).

## 8.2.0-rc.3

* Actually fix `pulp login` (which was *still* broken).
* Include the response body for any errors from the GitHub API in `pulp login`.

## 8.2.0-rc.2

* Remove the `moduleType` field from the bower.json file generated by `pulp
  init`.
* Fix `pulp login` using the wrong environment variable for the home directory
  on Windows (#197).
* Fix `pulp login` failing to check the auth token with GitHub (#199).
* Don't require being inside a project to run `pulp init` (#200).

## 8.2.0-rc.1

* Added `pulp version` for bumping versions and creating git tags for releases
  of PureScript packages, as well as `pulp publish` for sending those releases
  out into the world. See `pulp version --help` and `pulp publish --help` for
  more info.

## 8.1.1

* Revert to an open Prelude import in the default code generated by `pulp
  init`.

## 8.1.0

* Fix `pulp browserify` hanging on Windows.
* Fix `--dependency-path` and `--monochrome` options not being honoured
  when using psa.
* Add `pulp login`, which will be useful later along with the upcoming
  `pulp release`.
* Fix compiler warnings in PureScript source files generated by `pulp init`.

## 8.0.0

* Pulp's rebuild logic has been removed, as it was causing more
  trouble than it was worth. This means the `--force` flag is now once
  again only available on `pulp browserify`, to force a
  non-incremental build.
* Pulp will now use the
  [`psa`](https://github.com/natefaubion/purescript-psa) tool instead
  of `psc` if available on your path. You can disable this behaviour
  by passing the `--no-psa` flag.

Bugs fixed: #140.

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
