var merge = require("merge");
var fs = require("fs");

function wrap(s, indent) {
  var w = require("wordwrap").hard;
  if (process.stdout.columns) {
    return w(indent, process.stdout.columns)(s);
  } else {
    return indent ? (new Array(indent + 1).join(" ")) + s : s;
  }
}

function defaults(options) {
  return options.reduce(function(acc, next) {
    if (next.defaultValue) {
      acc[next.name] = next.defaultValue;
    }
    return acc;
  }, {});
}

function argErr(arg, message) {
  return {
    error: true,
    message: "Argument " + arg + ": " + message
  };
}
module.exports.argErr = argErr;

function isError(v) {
  return v && v.error;
}
module.exports.isError = isError;

function isParsed(v) {
  return v && v instanceof Array && v.length === 2;
}

function isFail(v) {
  return v === null;
}

function flag(arg, stream) {
  return [true, stream];
}
module.exports.flag = flag;

function int(arg, stream) {
  var num = parseInt(stream[0], 10);
  if (isNaN(num)) return argErr(arg, "Needs an integer argument.");
  return [num, stream.slice(1)];
};
module.exports.int = int;

function string(arg, stream) {
  if (!stream[0]) return argErr(arg, "Needs a string argument.");
  return [stream[0], stream.slice(1)];
};
module.exports.string = string;

function file(arg, stream) {
  var filename = stream[0];
  if (!filename) return argErr(arg, "Needs a file argument.");
  if (require("fs").existsSync(filename)) {
    return [filename, stream.slice(1)];
  } else {
    return argErr(arg, "File '" + filename + "' not found.");
  }
};
module.exports.file = file;

function directories(arg, stream) {
  if (!stream[0]) return argErr(arg, "Needs a directory argument.");
  var dirnames = stream[0].split(/\s+/).filter(function(s) { return s !== '' });
  var notExists = dirnames.filter(function(d) { return !fs.existsSync(d) });
  if (notExists.length === 0) {
    return [dirnames, stream.slice(1)];
  } else {
    var quoted = notExists.map(function(f) { return "'" + f + "'" })
    var directories = notExists.length === 1 ? "Directory" : "Directories"
    return argErr(arg, directories + ": " + quoted.join(' ') + " not found.");
  }
}
module.exports.directories = directories;

function helpOpt(context, stream) {
  return ["--help", "-h"].indexOf(stream[0]) >= 0 ?
    {error: true, help: true, context: context} : null;
}

function versionOpt(context, stream) {
  return ["--version", "-v"].indexOf(stream[0]) >= 0 ?
    {error: true, version: true, context: context} : null;
}

function arg(args, stream) {
  var token = stream[0], next = stream.slice(1),
      i = 0, l = args.length, result, o = {};
  for (; i < l; i++) {
    if (args[i].match.indexOf(token) >= 0) {
      result = args[i].type(token, next);
      if (isParsed(result)) {
        o[args[i].name] = result[0];
        return [o, result[1]];
      } else {
        return result;
      }
    }
  }
  return null;
}

function cmd(cmds, stream) {
  var token = stream[0], next = stream.slice(1),
      i = 0, l = cmds.length;
  for (; i < l; i++) {
    if (cmds[i].name === token) {
      return [cmds[i], next];
    }
  }
  return {
    error: true,
    message: token ? "Unknown command '" + token + "'." : "No command found."
  };
}

function seq(parser, fn) {
  return function(stream) {
    var result = parser(stream);
    if (isParsed(result)) {
      return fn(result[0])(result[1]);
    } else {
      return result;
    }
  };
}

function many(parser, fn) {
  return function(stream) {
    var result, acc = [];
    while (isParsed(result = parser(stream))) {
      acc.push(result[0]);
      stream = result[1];
    }
    if (isError(result)) {
      return result;
    } else {
      return [acc, stream];
    }
  };
}

function either(p1, p2) {
  return function(stream) {
    var result = p1(stream);
    if (isFail(result)) {
      return p2(stream);
    } else {
      return result;
    }
  };
}

function parse(globals, commands, stream) {
  return seq(
    many(either(either(helpOpt.bind(this, null),
                       versionOpt.bind(this, null)),
                arg.bind(this, globals))),
    function(globalOpts) {
      return seq(
        cmd.bind(this, commands),
        function(command) {
          return seq(
            many(either(helpOpt.bind(this, command.name),
                        arg.bind(this, command.options))),
            function(commandOpts) {
              return function(remainder) {
                return {
                  opts: merge(defaults(globals),
                              merge.apply(this, globalOpts)),
                  command: command,
                  commandOpts: merge(defaults(command.options),
                                     merge.apply(this, commandOpts)),
                  remainder: remainder
                };
              };
            }
          );
        }
      );
    }
  )(stream);
};
module.exports.parse = parse;

function option(name, match, type, desc, defaultValue) {
  return {
    name: name,
    match: match,
    type: type,
    desc: desc,
    defaultValue: defaultValue || undefined
  };
};
module.exports.option = option;

function command(name, desc, action, options) {
  return {
    name: name,
    desc: desc,
    action: action,
    options: options || []
  };
}
module.exports.command = command;

function printTable(table, stream) {
  var keys = Object.keys(table).sort();
  var longest = Math.max.apply(null, keys.map(function(key) {
    return key.length;
  }));
  keys.forEach(function(key) {
    var pad = longest - key.length, indent = longest + 3;
    stream.write("  " + key + (new Array(pad + 2).join(" ")));
    stream.write(wrap(table[key], indent).slice(indent) + "\n");
  });
}

function printOpts(options, stream) {
  printTable(options.reduce(function(acc, next) {
    var key = next.match.join(" ");
    if (next.type === file) key += " <file>";
    else if (next.type === string) key += " <string>";
    else if (next.type === directories) key += " <dirs>";
    var desc = next.desc;
    if (next.defaultValue) desc += " [Default: " + next.defaultValue + "]";
    acc[key] = desc;
    return acc;
  }, {}), stream);
}

function printCmds(commands, stream) {
  printTable(commands.reduce(function(acc, next) {
    acc[next.name] = next.desc;
    return acc;
  }, {}), stream);
}

function help(globals, commands, context, stream) {
  var out = require("ansi")(stream), cmd;
  globals = globals.concat([
    option(
      "help", ["--help", "-h"], flag, "Show this help message."
    ),
    option(
      "version", ["--version", "-v"], flag, "Show current pulp version"
    )
  ]);
  out.write("Usage: " + process.argv[1] + " [options] <command> [command-options]\n");
  if (context) {
    cmd = commands.filter(function(c) { return c.name === context; })[0];
    out.write("\n").bold().write("Command: " + cmd.name).reset().write("\n");
    out.write(wrap(cmd.desc, 2) + "\n");
    out.write("\n").bold().write("Command options:").reset().write("\n");
    printOpts(cmd.options, out);
  }
  out.write("\n").bold().write("Global options:").reset().write("\n");
  printOpts(globals, out);
  out.write("\n");
  if (!context) {
    out.bold().write("Commands:").reset().write("\n");
    printCmds(commands, out);
    out.write("\n").write(wrap(
      "Use `" + process.argv[1] + " <command> --help` to learn about " +
        "command specific options.", 2
    )).write("\n\n");
  }
};
module.exports.help = help;
