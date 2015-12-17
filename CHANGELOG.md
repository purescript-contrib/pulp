# Pulp Release History

## 6.0.1

* Remove unnecessary `postinstall` script.

## 6.0.0

* Pulp has been ported to PureScript :)

* The `--with-deps` flag for `pulp docs` has been renamed to
  `--with-dependencies`.

* Bugs fixed: #123, #122, #121, #111, #108, #92.

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
