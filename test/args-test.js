import path from "path";
import assert from "assert";
import args from "../args";

describe('Args', () => {
  describe('#argErr', () => {
    var arg = 'argument';
    var message = 'is invalid';

    context('given arg and message', () => {
      it('returns an argErr', () => {
        assert.deepEqual(args.argErr(arg, message), {
          error: true,
          message: 'Argument argument: is invalid'
        });
      });
    });
  });

  describe('#isError', () => {
    context('given an error', () => {
      it('returns true', () => {
        assert.equal(args.isError({error: true}), true);
      });
    });

    context('not given an error', () => {
      it('returns false', () => {
        assert.equal(args.isError({}), false);
      });
    });
  });

  describe('#flag', () => {
    var arg = 'argument';
    var stream = [];

    context('given arg and stream', () => {
      it('returns [bool, stream]', () => {
        assert.deepEqual(args.flag(arg, stream), [true, stream]);
      });
    });
  });

  describe('#int', () => {
    var arg = 'argument';

    context('given arg and stream', () => {
      context('and stream has valid number string', () => {
        var stream = ['1', {}];

        it('returns [number, stream]', () => {
          assert.deepEqual(args.int(arg, stream), [1, [{}]]);
        });
      });

      context('and stream has invalid number', () => {
        var stream = ['foo', {}];

        it('returns argErr: Need Integer', () => {
          assert.deepEqual(args.int(arg, stream), {
            error: true,
            message: 'Argument argument: Needs an integer argument.'
          });
        });
      });
    });
  });

  describe('string', () => {
    context('given arg and stream', () => {
      var arg = 'argument';

      context('and stream has a string', () => {
        var stream = ['string', {}];

        it('returns [string, stream]', () => {
          assert.deepEqual(args.string(arg, stream), ['string', [{}]]);
        });
      });

      context('and stream does not have a string', () => {
        var stream = [];

        it('returns argErr: Need String', () => {
          assert.deepEqual(args.string(arg, stream), {
            error: true,
            message: 'Argument argument: Needs a string argument.'
          });
        });
      });
    });
  });

  describe('#file', () => {
    context('given arg and stream', () => {
      var arg = 'argument';

      context('and stream has file path', () => {
        context('and the file exists', () => {
          var filename = path.join(__dirname, 'args-test.js');
          var stream = [filename, {}];

          it('returns [filename, stream]', () => {
            assert.deepEqual(args.file(arg, stream), [filename, [{}]]);
          });
        });

        context('and the file does not exist', () => {
          var filename = path.join(__dirname, 'not-here.js');
          var stream = [filename, {}];

          it('return argErr: File Not Found', () => {
            assert.deepEqual(args.file(arg, stream), {
              error: true,
              message: "Argument argument: File '" + filename + "' not found."
            });
          });
        });
      });

      context('and stream does not have file path', () => {
        var stream = [];

        it('returns argErr: Need File', () => {
          assert.deepEqual(args.file(arg, stream), {
            error: true,
            message: 'Argument argument: Needs a file argument.'
          });
        });
      });
    });
  });

  describe('#directories', () => {
    context('given arg and stream', () => {
      var arg = 'argument';

      context('and stream has directories', () => {
        context('and all directories exist', () => {
          var directories = path.join(__dirname);
          var stream = [directories, {}];

          it('returns [[directories], stream]', () => {
            assert.deepEqual(args.directories(arg, stream), [[directories], [{}]]);
          });
        });

        context('and some directories do not exist', () => {
          context('and given one directory', () => {
            var directories = path.join(__dirname, 'nothere/');
            var stream = [directories, {}];

            it('returns argErr: Directory Not Found', () => {
              assert.deepEqual(args.directories(arg, stream), {
                error: true,
                message: (
                  'Argument argument: Directory: ' +
                  "'" + directories + "'" + " not found.")
              });
            });
          });

          context('and given many directories', () => {
            var directories = [
              path.join(__dirname, 'nothere/'),
              path.join(__dirname, 'nothereagain/')
            ];
            var quoted = directories.map(dir => "'" + dir + "'");
            var stream = [directories.join(path.delimiter), {}];

            it('returns argErr: Directories Not Found', () => {
              assert.deepEqual(args.directories(arg, stream), {
                error: true,
                message: (
                  'Argument argument: Directories: ' +
                  quoted.join(' ') + " not found.")
              });
            });
          });
        });
      });

      context('and stream does not have directories', () => {
        var stream = [];

        it('returns argErr: Need Directory', () => {
          assert.deepEqual(args.directories(arg, stream), {
            error: true,
            message: 'Argument argument: Needs a directory argument.'
          });
        });
      });
    });
  });

  describe.skip('#parse', () => {});

  describe('#option', () => {
    context('given name, match, type, desc', () => {
      var name = 'name';
      var match = 'match';
      var type = 'type';
      var desc = 'desc';

      context('and given defaultValue', () => {
        var defaultValue = 'value';

        it('returns Option obj', () => {
          var option = args.option(name, match, type, desc, defaultValue);
          assert.deepEqual(option, {
            name: name,
            match: match,
            type: type,
            desc: desc,
            defaultValue: defaultValue
          });
        });
      });

      context('and given not defaultValue', () => {
        it('returns Option obj', () => {
          var option = args.option(name, match, type, desc);
          assert.deepEqual(option, {
            name: name,
            match: match,
            type: type,
            desc: desc,
            defaultValue: undefined
          });
        });
      });
    });
  });

  describe('#command', () => {
    context('given name desc, action', () => {
      var name = 'name';
      var desc = 'desc';
      var action = 'action';

      context('and given options', () => {
        var options = [{}];

        it('returns Command obj', () => {
          assert.deepEqual(args.command(name, desc, action, options), {
            name: name,
            desc: desc,
            action: action,
            options: [{}]
          });
        })
      });

      context('and not given options', () => {
        it('returns Command obj', () => {
          assert.deepEqual(args.command(name, desc, action), {
            name: name,
            desc: desc,
            action: action,
            options: []
          });
        });
      })
    });
  });

  describe.skip('#help', () => {});
});
