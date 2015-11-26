var fs = require("fs");
var path = require("path");
var touch = require("touch");
var webpack = require("webpack");
var Server = require("webpack-dev-server");
var files = require("./files");
var watch = require("./watch").watch;
var minimatch = require("minimatch");

module.exports = function(pro, args, callback) {
  var buildPath = path.resolve(args.buildPath);
  var globSet = files.defaultGlobs(args).union(files.SourceFileGlobSet(args.includePaths));
  var sources = globSet.sources().map(function(i) {
    return "src[]=" + i;
  });
  var ffis = globSet.ffis().map(function(i) {
    return "ffi[]=" + i;
  });

  files.resolveGlobs(globSet.sources(), function(err, sourceFiles) {
    if (err) return callback(err);
    var main = args.main.replace(".", path.sep) + ".purs";
    var entryPoint = main.replace("\\", "\\\\").replace("'", "\\'");
    var src = "require('." + path.sep.replace("\\", "\\\\") + entryPoint + "').main();\n";
    var entryPath = path.join("src", ".webpack.js");
    fs.writeFileSync(entryPath, src, "utf-8");

    var config = {
      cache: true,
      context: path.resolve(process.cwd(), "src"),
      entry: "./.webpack.js",
      debug: true,
      devtool: "source-map",
      output: {
        path: process.cwd(),
        pathinfo: true,
        filename: "app.js"
      },
      module: {
        loaders: [
          {
            test: /\.purs$/,
            loader: "purs-loader?output=" + buildPath +
                    "&" + sources.concat(ffis).join("&")
          }
        ]
      },
      resolve: {
        modulesDirectories: [
          "node_modules",
          "bower_components/purescript-prelude/src",
          buildPath
        ],
        extensions: [ "", ".js", ".purs" ]
      },
      resolveLoader: {
        root: path.resolve(__dirname, "node_modules")
      }
    };

    if (args.config) {
      config = require(path.resolve(args.config));
    }

    var options = {
      noInfo: args.noInfo,
      quiet: args.quiet,
      stats: {
        cached: false,
        cachedAssets: false,
        colors: require("supports-color") && !args.monochrome
      }
    };

    var server = new Server(webpack(config), options);
    server.listen(args.port, args.host, function(err) {
      if (err) throw err;
      console.log("Server running on http://" + args.host + ":" + args.port + "/");
    });

    watch(["src"], function(change) {
      var matched = minimatch(change, "src/**/*.js");
      if (matched) {
        touch(path.join("src", main), function(err) {
          if (err) return callback(err);
        });
      }
    });
  });
};
