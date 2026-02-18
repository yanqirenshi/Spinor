/**
 * Spinor Browser REPL
 *
 * xterm.js Terminal と Emscripten WASM モジュールを接続するブリッジ。
 * Module.onRuntimeInitialized から initREPL() が呼ばれる。
 */

var PROMPT = 'spinor> ';
var lineBuffer = '';
var term = null;
var historyList = [];
var historyIndex = -1;

function initREPL() {
  term = new Terminal({
    theme: {
      background: '#1e1e2e',
      foreground: '#cdd6f4',
      cursor: '#f5e0dc',
      cursorAccent: '#1e1e2e',
      selectionBackground: '#45475a',
      black: '#45475a',
      red: '#f38ba8',
      green: '#a6e3a1',
      yellow: '#f9e2af',
      blue: '#89b4fa',
      magenta: '#f5c2e7',
      cyan: '#94e2d5',
      white: '#bac2de',
      brightBlack: '#585b70',
      brightRed: '#f38ba8',
      brightGreen: '#a6e3a1',
      brightYellow: '#f9e2af',
      brightBlue: '#89b4fa',
      brightMagenta: '#f5c2e7',
      brightCyan: '#94e2d5',
      brightWhite: '#a6adc8'
    },
    fontFamily: "'JetBrains Mono', 'Fira Code', 'Consolas', monospace",
    fontSize: 14,
    cursorBlink: true,
    cols: 80,
    rows: 24
  });

  var fitAddon = new FitAddon.FitAddon();
  term.loadAddon(fitAddon);

  var container = document.getElementById('terminal-container');
  term.open(container);
  fitAddon.fit();

  window.addEventListener('resize', function() {
    fitAddon.fit();
  });

  /* welcome message */
  term.writeln('\x1b[34mSpinor REPL\x1b[0m  (Browser/WASM)');
  term.writeln('Type S-expressions to evaluate. Examples:');
  term.writeln('  (+ 1 2)          => 3');
  term.writeln('  (* 4 5)          => 20');
  term.writeln('  (if (> 10 5) 42 0) => 42');
  term.writeln('');
  term.write(PROMPT);

  term.onData(function(data) {
    handleInput(data);
  });
}

function handleInput(data) {
  for (var i = 0; i < data.length; i++) {
    var ch = data[i];
    var code = data.charCodeAt(i);

    if (ch === '\r' || ch === '\n') {
      /* Enter */
      term.writeln('');
      if (lineBuffer.trim().length > 0) {
        evalAndPrint(lineBuffer.trim());
        historyList.push(lineBuffer.trim());
        historyIndex = historyList.length;
      }
      lineBuffer = '';
      term.write(PROMPT);
    } else if (code === 127 || ch === '\b') {
      /* Backspace */
      if (lineBuffer.length > 0) {
        lineBuffer = lineBuffer.slice(0, -1);
        term.write('\b \b');
      }
    } else if (ch === '\x03') {
      /* Ctrl+C */
      lineBuffer = '';
      term.writeln('^C');
      term.write(PROMPT);
    } else if (ch === '\x1b') {
      /* Escape sequences (arrow keys) */
      if (i + 2 < data.length && data[i + 1] === '[') {
        var arrow = data[i + 2];
        if (arrow === 'A') {
          /* Up arrow - history back */
          if (historyIndex > 0) {
            clearLine();
            historyIndex--;
            lineBuffer = historyList[historyIndex];
            term.write(lineBuffer);
          }
        } else if (arrow === 'B') {
          /* Down arrow - history forward */
          if (historyIndex < historyList.length - 1) {
            clearLine();
            historyIndex++;
            lineBuffer = historyList[historyIndex];
            term.write(lineBuffer);
          } else if (historyIndex === historyList.length - 1) {
            clearLine();
            historyIndex = historyList.length;
            lineBuffer = '';
          }
        }
        i += 2; /* skip the escape sequence chars */
      }
    } else if (code >= 32) {
      /* printable character */
      lineBuffer += ch;
      term.write(ch);
    }
  }
}

function clearLine() {
  for (var j = 0; j < lineBuffer.length; j++) {
    term.write('\b \b');
  }
}

function evalAndPrint(input) {
  try {
    var result = Module.ccall('sp_eval_string', 'string', ['string'], [input]);
    if (result && result.length > 0) {
      term.writeln('\x1b[32m' + result + '\x1b[0m');
    }
  } catch (e) {
    term.writeln('\x1b[31merror: ' + e.message + '\x1b[0m');
  }
}
