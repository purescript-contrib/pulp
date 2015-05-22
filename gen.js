var log = require("./log");
var path = require("path");
var fs = require("fs");

function src(moduleName){
  return "module " + moduleName + " where";
}

function test(moduleName){
  return ["module Test." + moduleName + " where",
  "",
  "import " + moduleName,
  "import Debug.Trace",
  "",
  "main = do",
  "  trace \"Write tests for " + moduleName + "\""
  ].join("\n") + "\n";
}

function capitalise(string) {
    if(string){
      return string.charAt(0).toUpperCase() + string.slice(1).toLowerCase();
    }else{ return ""; }
}

function prependBase(trail, x){
  return path.join.apply(this, [process.cwd()].concat(trail).concat([x]));
}

function write(file, trail, moduleName){
  var modulePath = moduleName.split(".").map(capitalise);

  function writeFile(fileName){
    var f = prependBase(trail, fileName + ".purs");
    if(fs.existsSync(f)){
      console.log("file already exists : " + f + "\nleaving it be." );
    }else{
      console.log("writing file : " + f );
      fs.writeFileSync(f, file(modulePath.join(".")), "utf-8");
    }
  }

  function writeDir(dirName){
    var p = prependBase(trail, dirName);
    if(!fs.existsSync(p)){
      fs.mkdirSync(p);
    }
    trail.push(dirName);
  }

  writeDir("");

  for(var i = 0; i < modulePath.length; i++){
    var current = modulePath[i];
    if(i === modulePath.length - 1){
      writeFile(current);
    }else{
      writeDir(current);
    }
  }

}

module.exports = function(pro, args, callback) {
  write(src,  ["src"],  args.remainder[0]);
  write(test, ["test"], args.remainder[0]);
  callback();
}
