// Example of running the ZOWIE reference interpreter, zowie.py,
// under Skulpt, in a web browser.  As of this writing, this
// can be seen in action here: http://catseye.tc/installation/ZOWIE

function load_file(url, callback)
{
  var xhr = new XMLHttpRequest();
  xhr.onload = function(e) {
    if (xhr.readyState === 4 && xhr.responseText) {
      if (xhr.status === 200) {
        callback(xhr.responseText);
      } else {
        alert("Error: could not load " + url + ": " + xhr.statusText);
      }
    }
  };
  xhr.open("get", url, true);
  xhr.send();
}

PresetManager = function() {
  this.init = function(cfg) {
    this.selectElem = cfg.selectElem;
    this.clear();
    var $this = this;
    this.selectElem.onchange = function() {
      $this._select(this.options[this.selectedIndex].value);
    };
    return this;
  };

  this.clear = function() {
    this.reactTo = {};
    while (this.selectElem.firstChild) {
      this.selectElem.removeChild(this.selectElem.firstChild);
    }
    this.add('(select one...)', function() {});
    return this;
  };

  this.add = function(id, callback) {
    var opt = document.createElement("option");
    opt.text = id;
    opt.value = id;
    this.selectElem.options.add(opt);
    var $this = this;
    this.reactTo[id] = callback;
    return this;
  };

  this._select = function(id) {
    this.reactTo[id](id);
    if (this.onselect) {
      this.onselect(id);
    }
  };

  this.select = function(id) {
    var i = 0;
    var opt = this.selectElem.options[i];
    while (opt) {
      if (opt.value === id) {
        this.selectElem.selectedIndex = i;
        this._select(id);
        return this;
      }
      i++;
      opt = this.selectElem.options[i];
    }
    // if not found, select the "(select one...)" option
    this.selectElem.selectedIndex = 0;
    return this;
  };
};

function launch(prefix, container, config) {
  if (typeof container === 'string') {
    container = document.getElementById(container);
  }
  config = config || {};

  document.getElementById('installation').innerHTML =
    '<div id="control_panel"></div>' +
    '<button type="button" id="run">Run</button>' +
    '<span class="control-container">example program: <select id="select_source"></select></span>' +
    '<pre id="output"></pre>' +
    '<textarea id="editor" rows="14" cols="60" wrap="off"></textarea>';

  var presetManager = (new PresetManager()).init({
      selectElem: document.getElementById('select_source')
  });
  function makeCallback(sourceText) {
    return function(id) {
      document.getElementById('editor').value = sourceText;
    }
  }
  for (var i = 0; i < examplePrograms.length; i++) {
    presetManager.add(examplePrograms[i].filename, makeCallback(examplePrograms[i].contents));
  }
  presetManager.select("chars.zow");

  document.getElementById("run").style.enabled = false;
  var zowie_interpreter;

  load_file("../modules/zowie/src/zowie.py", function(text) {
      zowie_interpreter = text;
      document.getElementById("run").style.enabled = true;
  });

  document.getElementById("run").onclick = function() {
    var esoprog = document.getElementById("editor").value;
    var mypre = document.getElementById("output");
    mypre.innerHTML = '';
    // Sk is the Skulpt object, which we assume has been loaded first
    Sk.canvas = "mycanvas";
    Sk.pre = "output";
    Sk.configure({
      output: function(text) {
        var mypre = document.getElementById("output");
        mypre.innerHTML = mypre.innerHTML + text.replace(/\n$/, "");
      },
      read: function(x) {
        if (Sk.builtinFiles === undefined || Sk.builtinFiles["files"][x] === undefined) {
          throw "File not found: '" + x + "'";
        }
        return Sk.builtinFiles["files"][x];
      }
    });
    var prog = zowie_interpreter + "\n";
    prog += 'p = Processor()\n';
    prog += 'p.load_string("""\n';
    prog += esoprog;
    prog += '""")\n';
    prog += 'p.run()\n';
    eval(Sk.importMainWithBody("<stdin>", false, prog));
  };
}
