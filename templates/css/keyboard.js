/* ********************************************************************
 **********************************************************************
 * HTML Virtual Keyboard Interface script - v1.2
 *   Copyright (c) 2007 - GreyWyvern
 *
 *  - Licenced for free distribution under the BSDL
 *          http://www.opensource.org/licenses/bsd-license.php
 *
 * Add a script-driven keyboard interface to your text fields, password
 * fields and textareas.
 *
 * See http://www.greywyvern.com/code/js/keyboard.html for examples and
 * usage instructions.
 *
 * Version 1.2 - June 24, 2007
 *   - Added support for dead keys
 *
 * Keyboard Credits
 *   - Lithuanian and Russian keyboard layouts added by Ramunas
 *   - German keyboard layout added by QuHno
 *   - French keyboard layout added by Hidden Evil
 *   - Polish Programmers layout assisted by moose
 *
 */

function buildKeyboardInputs() {
  var self = this;

  this.VKI_target = "";
  this.VKI_visible = -1;
  this.VKI_shift = this.VKI_capslock = this.VKI_alternate = this.VKI_dead = false;
  this.VKI_deadkeysOn = false;
  this.VKI_kt = "US";  // Default keyboard layout
  this.VKI_range = false;


  /* ***** Create keyboards **************************************** */
  this.VKI_layout = new Object();

  // - Lay out each keyboard in rows of sub-arrays
  // 
  // - If a sub-array has only two items (eg. ["t", "T"]) then this key
  //   is a normal letter, where the first element is unshifted and the
  //   second is shifted.
  //
  // - If a sub-array has three items and the third item matches one of
  //   the following strings:
  //       "Tab", "Caps", "Shift", "Enter", "Bksp", "Alt" OR "AltGr"
  //     then the function of the key will be the following,
  //     respectively:
  //     - Insert a tab
  //     - Toggle Caps Lock (technically a Shift Lock)
  //     - Next entered character will be the shifted character
  //     - Insert a newline (textarea), or close the keyboard
  //     - Delete the previous character
  //     - Next entered character will be the alternate character
  //
  // - If a sub-array has three items and the third item does not match
  //   any of the strings above, or it has four items, then the third
  //   and fourth items are the unshifted and shifted versions of the
  //   alternate character respectively.
  //
  // - Layout dead keys (diacritic + letter) should be added as arrays
  //   of two item arrays with hash keys equal to the diacritic.  See
  //   the "this.VKI_deadkey" object below the layout definitions. In
  //   each two item child array, the second item is what the diacritic
  //   would change the first item to.
  //
  // - Note that any characters beyond the normal ASCII set should be
  //   entered in escaped Unicode format.  (eg \u00a3 = Pound symbol)
  //   You can find Unicode values for characters here:
  //     http://unicode.org/charts/
  //
  // - To remove a keyboard, just delete it, or comment it out of the
  //   source code

  this.VKI_layout.Dvorak = [ // Dvorak Keyboard
    [["`", "~"], ["1", "!"], ["2", "@"], ["3", "#"], ["4", "$"], ["5", "%"], ["6", "^"], ["7", "&"], ["8", "*"], ["9", "("], ["0", ")"], ["[", "{"], ["]", "}"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["'", '"'], [",", "<"], [".", ">"], ["p", "P"], ["y", "Y"], ["f", "F"], ["g", "G"], ["c", "C"], ["r", "R"], ["l", "L"], ["/", "?"], ["=", "+"], ["\\", "|"]],
    [["Caps", "CAPS", "Caps"], ["a", "A"], ["o", "O"], ["e", "E"], ["u", "U"], ["i", "I"], ["d", "D"], ["h", "H"], ["t", "T"], ["n", "N"], ["s", "S"], ["-", "_"], ["Enter", "Enter", "Enter"]],
    [["Shift", "SHIFT", "Shift"], [";", ":"], ["q", "Q"], ["j", "J"], ["k", "K"], ["x", "X"], ["b", "B"], ["m", "M"], ["w", "W"], ["v", "V"], ["z", "Z"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "]]
  ];

  this.VKI_layout.French = [ // French Standard Keyboard
    [["\u00b2", "\u00b3"], ["&", "1"], ["\u00e9", "2", "~"], ['"', "3", "#"], ["'", "4", "{"], ["(", "5", "["], ["-", "6", "|"], ["\u00e8", "7", "\u0060"], ["_", "8", "\\"], ["\u00e7", "9", "\u005e"], ["\u00e0", "0", "\u0040"], [")", "\u00b0", "]"], ["=", "+", "}"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["a", "A"], ["z", "Z"], ["e", "E", "\u20ac"], ["r", "R"], ["t", "T"], ["y", "Y"], ["u", "U"], ["i", "I"], ["o", "O"], ["p", "P"], ["\u005e", "\u00a8"], ["$", "\u00a3", "\u00a4"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["q", "Q"], ["s", "S"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L"], ["m", "M"], ["\u00f9", "%"], ["*", "\u03bc"]],
    [["Shift", "SHIFT", "Shift"], ["<", ">"], ["w", "W"], ["x", "X"], ["c", "C"], ["v", "V"], ["b", "B"], ["n", "N"], [",", "?"], [";", "."], [":", "/"], ["!", "\u00a7"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "], ["AltGr", "AltGr", "AltGr"]]
  ];

  this.VKI_layout.German = [ // German Standard Keyboard
    [["\u005e", "\u00b0"], ["1", "!"], ["2", '"', "\u00b2"], ["3", "\u00a7", "\u00b3"], ["4", "$"], ["5", "%"], ["6", "&"], ["7", "/", "{"], ["8", "(", "["], ["9", ")", "]"], ["0", "=", "}"], ["\u00df", "?", "\\"], ["\u00b4", "\u0060"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["q", "Q", "\u0040"], ["w", "W"], ["e", "E", "\u20ac"], ["r", "R"], ["t", "T"], ["z", "Z"], ["u", "U"], ["i", "I"], ["o", "O"], ["p", "P"], ["\u00fc", "\u00dc"], ["+", "*", "~"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["a", "A"], ["s", "S"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L"], ["\u00f6", "\u00d6"], ["\u00e4", "\u00c4"], ["#", "'"]],
    [["Shift", "SHIFT", "Shift"], ["<", ">", "\u00a6"], ["y", "Y"], ["x", "X"], ["c", "C"], ["v", "V"], ["b", "B"], ["n", "N"], ["m", "M", "\u00b5"], [",", ";"], [".", ":"], ["-", "_"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "], ["AltGr", "AltGr", "AltGr"]]
  ];

  this.VKI_layout.Greek = [ // Greek Standard Keyboard
    [["`", "~"], ["1", "!"], ["2", "@", "\u00b2"], ["3", "#", "\u00b3"], ["4", "$", "\u00a3"], ["5", "%", "\u00a7"], ["6", "^", "\u00b6"], ["7", "&"], ["8", "*", "\u00a4"], ["9", "(", "\u00a6"], ["0", ")", "\u00ba"], ["-", "_", "\u00b1"], ["=", "+", "\u00bd"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],[";", ":"], ["\u03c2", "^"], ["\u03b5", "\u0395"], ["\u03c1", "\u03a1"], ["\u03c4", "\u03a4"], ["\u03c5", "\u03a5"], ["\u03b8", "\u0398"], ["\u03b9", "\u0399"], ["\u03bf", "\u039f"], ["\u03c0", "\u03a0"], ["[", "{", "\u201c"], ["]", "}", "\u201d"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["\u03b1", "\u0391"], ["\u03c3", "\u03a3"], ["\u03b4", "\u0394"], ["\u03c6", "\u03a6"], ["\u03b3", "\u0393"], ["\u03b7", "\u0397"], ["\u03be", "\u039e"], ["\u03ba", "\u039a"], ["\u03bb", "\u039b"], ["\u0384", "\u00a8", "\u0385"], ["'", '"'], ["\\", "|", "\u00ac"]],
    [["Shift", "SHIFT", "Shift"], ["<", ">"], ["\u03b6", "\u0396"], ["\u03c7", "\u03a7"], ["\u03c8", "\u03a8"], ["\u03c9", "\u03a9"], ["\u03b2", "\u0392"], ["\u03bd", "\u039d"], ["\u03bc", "\u039c"], [",", "<"], [".", ">"], ["/", "?"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "], ["AltGr", "AltGr", "AltGr"]]
  ];

  this.VKI_layout.Lithuanian = [ // Lithuanian Standard Keyboard
    [["`", "~"], ["\u0105", "\u0104"], ["\u010D", "\u010C"], ["\u0119", "\u0118"], ["\u0117", "\u0116"], ["\u012F", "\u012E"], ["\u0161", "\u0160"], ["\u0173", "\u0172"], ["\u016B", "\u016A"], ["\u201E", "("], ["\u201C", ")"], ["-", "_"], ["\u017E", "\u017D"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["q", "Q"], ["w", "W"], ["e", "E"], ["r", "R"], ["t", "T"], ["y", "Y"], ["u", "U"], ["i", "I"], ["o", "O"], ["p", "P"], ["[", "{"], ["]", "}"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["a", "A"], ["s", "S"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L"], [";", ":"], ["'", '"'], ["\\", "|"]],
    [["Shift", "SHIFT", "Shift"], ["\u2013", "\u20AC"], ["z", "Z"], ["x", "X"], ["c", "C"], ["v", "V"], ["b", "B"], ["n", "N"], ["m", "M"], [",", "<"], [".", ">"], ["/", "?"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "]]
  ];

  this.VKI_layout.Norwegian = [ // Norwegian Standard Keyboard
    [["|", "\u00a7"], ["1", "!"], ["2", '"', "@"], ["3", "#", "\u00a3"], ["4", "\u00a4", "$"], ["5", "%"], ["6", "&"], ["7", "/", "{"], ["8", "(", "["], ["9", ")", "]"], ["0", "=", "}"], ["+", "?"], ["\\", "`", "\u00b4"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["q", "Q"], ["w", "W"], ["e", "E", "\u20ac"], ["r", "R"], ["t", "T"], ["y", "Y"], ["u", "U"], ["i", "I"], ["o", "O"], ["p", "P"], ["\u00e5", "\u00c5"], ["\u00a8", "^", "~"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["a", "A"], ["s", "S"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L"], ["\u00f8", "\u00d8"], ["\u00e6", "\u00c6"], ["'", "*"]],
    [["Shift", "SHIFT", "Shift"], ["<", ">"], ["z", "Z"], ["x", "X"], ["c", "C"], ["v", "V"], ["b", "B"], ["n", "N"], ["m", "M", "\u03bc", "\u039c"], [",", ";"], [".", ":"], ["-", "_"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "], ["AltGr", "AltGr", "AltGr"]]
  ];

  this.VKI_layout.PolishPrg = [ // Polish Programmers Keyboard
    [["`", "~"], ["1", "!"], ["2", "@"], ["3", "#"], ["4", "$"], ["5", "%"], ["6", "^"], ["7", "&"], ["8", "*"], ["9", "("], ["0", ")"], ["-", "_"], ["=", "+"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["q", "Q"], ["w", "W"], ["e", "E", "\u0119", "\u0118"], ["r", "R"], ["t", "T"], ["y", "Y"], ["u", "U"], ["i", "I"], ["o", "O", "\u00f3", "\u00d3"], ["p", "P"], ["[", "{"], ["]", "}"], ["\\", "|"]],
    [["Caps", "CAPS", "Caps"], ["a", "A", "\u0105", "\u0104"], ["s", "S", "\u015b", "\u015a"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L", "\u0142", "\u0141"], [";", ":"], ["'", '"'], ["Enter", "Enter", "Enter"]],
    [["Shift", "SHIFT", "Shift"], ["z", "Z", "\u017c", "\u017b"], ["x", "X", "\u017a", "\u0179"], ["c", "C", "\u0107", "\u0106"], ["v", "V"], ["b", "B"], ["n", "N", "\u0144", "\u0143"], ["m", "M"], [",", "<"], [".", ">"], ["/", "?"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "], ["Alt", "Alt", "Alt"]]
  ];

  this.VKI_layout.Russian = [ // Russian Standard Keyboard
    [["\u0451", "\u0401"], ["1", "!"], ["2", '"'], ["3", "\u2116"], ["4", ";"], ["5", "%"], ["6", ":"], ["7", "?"], ["8", "*"], ["9", "("], ["0", ")"], ["-", "_"], ["=", "+"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["\u0439", "\u0419"], ["\u0446", "\u0426"], ["\u0443", "\u0423"], ["\u043A", "\u041A"], ["\u0435", "\u0415"], ["\u043D", "\u041D"], ["\u0433", "\u0413"], ["\u0448", "\u0428"], ["\u0449", "\u0429"], ["\u0437", "\u0417"], ["\u0445", "\u0425"], ["\u044A", "\u042A"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["\u0444", "\u0424"], ["\u044B", "\u042B"], ["\u0432", "\u0412"], ["\u0430", "\u0410"], ["\u043F", "\u041F"], ["\u0440", "\u0420"], ["\u043E", "\u041E"], ["\u043B", "\u041B"], ["\u0434", "\u0414"], ["\u0436", "\u0416"], ["\u044D", "\u042D"], ["\\", "/"]],
    [["Shift", "SHIFT", "Shift"], ["/", "|"], ["\u044F", "\u042F"], ["\u0447", "\u0427"], ["\u0441", "\u0421"], ["\u043C", "\u041C"], ["\u0438", "\u0418"], ["\u0442", "\u0422"], ["\u044C", "\u042C"], ["\u0431", "\u0411"], ["\u044E", "\u042E"], [".", ","], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "]]
  ];

  this.VKI_layout.UK = [ // UK Standard Keyboard
    [["`", "\u00ac", "\u00a6"], ["1", "!"], ["2", '"'], ["3", "\u00a3"], ["4", "$", "\u20ac"], ["5", "%"], ["6", "^"], ["7", "&"], ["8", "*"], ["9", "("], ["0", ")"], ["-", "_"], ["=", "+"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["q", "Q"], ["w", "W"], ["e", "E", "\u00e9", "\u00c9"], ["r", "R"], ["t", "T"], ["y", "Y"], ["u", "U", "\u00fa", "\u00da"], ["i", "I", "\u00ed", "\u00cd"], ["o", "O", "\u00f3", "\u00d3"], ["p", "P"], ["[", "{"], ["]", "}"], ["Enter", "Enter", "Enter"]],
    [["Caps", "CAPS", "Caps"], ["a", "A", "\u00e1", "\u00c1"], ["s", "S"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L"], [";", ":"], ["'", "@"], ["#", "~"]],
    [["Shift", "SHIFT", "Shift"], ["\\", "|"], ["z", "Z"], ["x", "X"], ["c", "C"], ["v", "V"], ["b", "B"], ["n", "N"], ["m", "M"], [",", "<"], [".", ">"], ["/", "?"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "], ["AltGr", "AltGr", "AltGr"]]
  ];

  this.VKI_layout.US = [ // US Standard Keyboard
    [["`", "~"], ["1", "!"], ["2", "@"], ["3", "#"], ["4", "$"], ["5", "%"], ["6", "^"], ["7", "&"], ["8", "*"], ["9", "("], ["0", ")"], ["-", "_"], ["=", "+"], ["Bksp", "Bksp", "Bksp"]],
    [["Tab", "Tab", "Tab"],["q", "Q"], ["w", "W"], ["e", "E"], ["r", "R"], ["t", "T"], ["y", "Y"], ["u", "U"], ["i", "I"], ["o", "O"], ["p", "P"], ["[", "{"], ["]", "}"], ["\\", "|"]],
    [["Caps", "CAPS", "Caps"], ["a", "A"], ["s", "S"], ["d", "D"], ["f", "F"], ["g", "G"], ["h", "H"], ["j", "J"], ["k", "K"], ["l", "L"], [";", ":"], ["'", '"'], ["Enter", "Enter", "Enter"]],
    [["Shift", "SHIFT", "Shift"], ["z", "Z"], ["x", "X"], ["c", "C"], ["v", "V"], ["b", "B"], ["n", "N"], ["m", "M"], [",", "<"], [".", ">"], ["/", "?"], ["Shift", "SHIFT", "Shift"]],
    [[" ", " "]]
  ];


  /* ***** Define Dead Keys **************************************** */
  this.VKI_deadkey = new Object();

  // - Lay out each dead key set in one row of sub-arrays.  The rows
  //   below are wrapped so uppercase letters are below their lowercase
  //   equivalents.
  //
  // - The first letter in each sub-array is the letter pressed after
  //   the diacritic.  The second letter is the letter this key-combo
  //   will generate.
  //
  // - Note that if you have created a new keyboard layout and want it
  //   included in the distributed script, PLEASE TELL ME if you have
  //   added additional dead keys to the ones below.

  this.VKI_deadkey['"'] = this.VKI_deadkey['\u00a8'] = [ // Umlaut / Diaeresis / Greek Dialytika
    ["a", "\u00e4"], ["e", "\u00eb"], ["i", "\u00ef"], ["o", "\u00f6"], ["u", "\u00fc"], ["y", "\u00ff"], ["\u03b9", "\u03ca"], ["\u03c5", "\u03cb"],
    ["A", "\u00c4"], ["E", "\u00cb"], ["I", "\u00cf"], ["O", "\u00d6"], ["U", "\u00dc"], ["Y", "\u0178"], ["\u0399", "\u03aa"], ["\u03a5", "\u03ab"]
  ];
  this.VKI_deadkey['~'] = [ // Tilde
    ["a", "\u00e3"], ["o", "\u00f5"], ["n", "\u00f1"],
    ["A", "\u00c3"], ["O", "\u00d5"], ["N", "\u00d1"]
  ];
  this.VKI_deadkey['^'] = [ // Circumflex
    ["a", "\u00e2"], ["e", "\u00ea"], ["i", "\u00ee"], ["o", "\u00f4"], ["u", "\u00fb"], ["w", "\u0175"], ["y", "\u0177"],
    ["A", "\u00c2"], ["E", "\u00ca"], ["I", "\u00ce"], ["O", "\u00d4"], ["U", "\u00db"], ["W", "\u0174"], ["Y", "\u0176"]
  ];
  this.VKI_deadkey['`'] = [ // Grave
    ["a", "\u00e0"], ["e", "\u00e8"], ["i", "\u00ec"], ["o", "\u00f2"], ["u", "\u00f9"],
    ["A", "\u00c0"], ["E", "\u00c8"], ["I", "\u00cc"], ["O", "\u00d2"], ["U", "\u00d9"]
  ];
  this.VKI_deadkey["'"] = this.VKI_deadkey['\u00b4'] = this.VKI_deadkey['\u0384'] = [ // Acute / Greek Tonos
    ["a", "\u00e1"], ["e", "\u00e9"], ["i", "\u00ed"], ["o", "\u00f3"], ["u", "\u00fa"], ["\u03b1", "\u03ac"], ["\u03b5", "\u03ad"], ["\u03b7", "\u03ae"], ["\u03b9", "\u03af"], ["\u03bf", "\u03cc"], ["\u03c5", "\u03cd"], ["\u03c9", "\u03ce"],
    ["A", "\u00c1"], ["E", "\u00c9"], ["I", "\u00cd"], ["O", "\u00d3"], ["U", "\u00da"], ["\u0391", "\u0386"], ["\u0395", "\u0388"], ["\u0397", "\u0389"], ["\u0399", "\u038a"], ["\u039f", "\u038c"], ["\u03a5", "\u038e"], ["\u03a9", "\u038f"]
  ];
  this.VKI_deadkey["\u0385"] = [ // Greek Dialytika + Tonos
    ["\u03b9", "\u0390"], ["\u03c5", "\u03b0"]
  ];
  this.VKI_deadkey['\u00b0'] = [ // Ring
    ["a", "\u00e5"],
    ["A", "\u00c5"]
  ];



  /* ***** Find tagged input & textarea elements ******************* */
  var inputElems = [
    document.getElementsByTagName('input'),
    document.getElementsByTagName('textarea'),
  ]
  for (var y = 0, inputCount = 0; y < inputElems.length; y++) {
    if (inputElems[y]) {
      for (var x = 0; x < inputElems[y].length; x++) {
          if ((inputElems[y][x].type == "password") || ((y || inputElems[y][x].type == "text") && inputElems[y][x].className.indexOf("keyboardInput") > -1)) {
          var keyid = (inputElems[y][x].id) ? inputElems[y][x].id : 'keyboardInputInitiator' + inputCount++;

          var keybut = document.createElement('img');
              keybut.src = "../css/keyboard.png";
              keybut.alt = "Keyboard interface";
              keybut.className = "keyboardInputInitiator";
              keybut.title = "Display graphical keyboard interface";
              keybut.onclick = (function(a) { return function() { self.VKI_show(a); }; })(keyid);

          inputElems[y][x].id = keyid;
          inputElems[y][x].keyboardId = x;
          inputElems[y][x].parentNode.insertBefore(keybut, inputElems[y][x].nextSibling);
          inputElems[y][x].onclick = inputElems[y][x].onkeypress = inputElems[y][x].onselect = function() {
            if (self.VKI_target.createTextRange) self.VKI_range = document.selection.createRange();
          }
        }
      }
    }
  }


  /* ***** Build the keyboard interface **************************** */
  this.VKI_keyboard = document.createElement('div');
  this.VKI_keyboard.id = "keyboardInputMaster";

  var layouts = 0;
  for (ktype in this.VKI_layout) if (typeof this.VKI_layout[ktype] == "object") layouts++;

  if (layouts > 1) {
    var kblist = document.createElement('select');
      for (ktype in this.VKI_layout) {
        if (typeof this.VKI_layout[ktype] == "object") {
          var opt = document.createElement('option');
              opt.value = ktype;
              opt.appendChild(document.createTextNode(ktype));
            kblist.appendChild(opt);
        }
      }
        kblist.value = this.VKI_kt;
        kblist.onchange = function() {
          self.VKI_kt = this.value;
          self.VKI_buildKeys();
          self.VKI_position();
        }
    this.VKI_keyboard.appendChild(kblist);
  }

  var label = document.createElement('label');
    var checkbox = document.createElement('input');
        checkbox.type = "checkbox";
        checkbox.checked = this.VKI_deadkeysOn;
        checkbox.title = "Toggle dead key input";
        checkbox.onclick = function() {
          self.VKI_deadkeysOn = this.checked;
          this.nextSibling.nodeValue = " Dead keys: " + ((this.checked) ? "On" : "Off");
          self.VKI_modify("");
          return true;
        }
      label.appendChild(checkbox);
      label.appendChild(document.createTextNode(" Dead keys: " + ((checkbox.checked) ? "On" : "Off")))
  this.VKI_keyboard.appendChild(label);

  var clearer = document.createElement('span');
      clearer.id = "keyboardInputClear";
      clearer.appendChild(document.createTextNode("Clear"));
      clearer.title = "Clear this input";
      clearer.onmousedown = function() { this.className = "pressed"; }
      clearer.onmouseup = function() { this.className = ""; }
      clearer.onclick = function() {
        self.VKI_target.value = "";
        self.VKI_target.focus();
        return false;
      }
  this.VKI_keyboard.appendChild(clearer);

  var closer = document.createElement('span');
      closer.id = "keyboardInputClose";
      closer.appendChild(document.createTextNode('X'));
      closer.title = "Close this window";
      closer.onmousedown = function() { this.className = "pressed"; }
      closer.onmouseup = function() { this.className = ""; }
      closer.onclick = function(e) { self.VKI_close(); }
  this.VKI_keyboard.appendChild(closer);

  this.VKI_keyboard.appendChild(document.createElement('div'));      



  /* ***** Functions ************************************************ */
  /* ******************************************************************
   * Build or rebuild the keyboard keys
   *
   */
  this.VKI_buildKeys = function() {
    this.VKI_shift = this.VKI_capslock = this.VKI_alternate = this.VKI_dead = false;

    var container = this.VKI_keyboard.getElementsByTagName('div')[0];
    while (container.firstChild) container.removeChild(container.firstChild);

    for (var x = 0; x < this.VKI_layout[this.VKI_kt].length; x++) {
      var div = document.createElement('div');
      if (this.VKI_layout[this.VKI_kt][x].length <= 3) div.className = "keyboardInputCenter";
        var ul = document.createElement('ul');
        for (var y = 0; y < this.VKI_layout[this.VKI_kt][x].length; y++) {
          var li = document.createElement('li');
              li.appendChild(document.createTextNode(this.VKI_layout[this.VKI_kt][x][y][0]));

            var alive = false;
            if (this.VKI_deadkeysOn) for (key in this.VKI_deadkey) if (key === this.VKI_layout[this.VKI_kt][x][y][0]) alive = true;
              li.className = (alive) ? "alive" : "";

            if (this.VKI_layout[this.VKI_kt][x][y][0] == " ")
              li.style.paddingLeft = li.style.paddingRight = "50px";
              li.onmouseover = function(e) { if (this.className != "dead") this.className += " hover"; }
              li.onmouseout = function(e) { if (this.className != "dead") this.className = this.className.replace(/ ?hover/g, ""); }
              li.onmousedown = function(e) { if (this.className != "dead") this.className += " pressed"; }
              li.onmouseup = function(e) { if (this.className != "dead") this.className = this.className.replace(/ ?pressed/g, ""); }
              li.ondblclick = function() { return false; }
            if (this.VKI_layout[this.VKI_kt][x][y].length == 3) {
              switch (this.VKI_layout[this.VKI_kt][x][y][2]) {
                case "Caps":
                case "Shift":
                case "Alt":
                case "AltGr":
                  li.onclick = (function(type) { return function() { self.VKI_modify(type); return false; }})(this.VKI_layout[this.VKI_kt][x][y][2]);
                  break;
                case "Tab":
                  li.onclick = function() { self.VKI_insert("\t"); return false; }
                  break;
                case "Bksp":
                  li.onclick = function() {
                    self.VKI_target.focus();
                    if (self.VKI_target.setSelectionRange) {
                      var srt = self.VKI_target.selectionStart;
                      var len = self.VKI_target.selectionEnd;
                      if (srt < len) srt++;
                      self.VKI_target.value = self.VKI_target.value.substr(0, srt - 1) + self.VKI_target.value.substr(len);
                      self.VKI_target.setSelectionRange(srt - 1, srt - 1);
                    } else if (self.VKI_target.createTextRange) {
                      try { self.VKI_range.select(); } catch(e) {}
                      self.VKI_range = document.selection.createRange();
                      if (!self.VKI_range.text.length) self.VKI_range.moveStart('character', -1);
                      self.VKI_range.text = "";
                    } else self.VKI_target.value = self.VKI_target.value.substr(0, self.VKI_target.value.length - 1);
                    if (self.VKI_shift) self.VKI_modify("Shift");
                    if (self.VKI_alternate) self.VKI_modify("AltGr");
                    return true;
                  }
                  break;
                case "Enter":
                  li.onclick = function() {
                    if (self.VKI_target.nodeName == "TEXTAREA") { self.VKI_insert("\n"); } else self.VKI_close();
                    return true;
                  }
                  break;
                default:
                 this.VKI_layout[this.VKI_kt][x][y][3] = "\xa0";
              }
            } else if (this.VKI_layout[this.VKI_kt][x][y].length == 2)
              this.VKI_layout[this.VKI_kt][x][y][2] = this.VKI_layout[this.VKI_kt][x][y][3] = "\xa0";
  
            if (!li.onclick) {
              li.onclick = function() {
                if (self.VKI_deadkeysOn && self.VKI_dead) {
                  if (self.VKI_dead != this.firstChild.nodeValue) {
                    for (key in self.VKI_deadkey) {
                      if (key == self.VKI_dead) {
                        if (this.firstChild.nodeValue != " ") {
                          for (var z = 0, rezzed = false; z < self.VKI_deadkey[key].length; z++) {
                            if (self.VKI_deadkey[key][z][0] == this.firstChild.nodeValue) {
                              self.VKI_insert(self.VKI_deadkey[key][z][1]);
                              rezzed = true;
                              break;
                            }
                          }
                        } else {
                          self.VKI_insert(self.VKI_dead);
                          rezzed = true;
                        }
                        break;
                      }
                    }
                  } else rezzed = true;
                }
                self.VKI_dead = false;

                if (!rezzed && this.firstChild.nodeValue != "\xa0") {
                  if (self.VKI_deadkeysOn) {
                    for (key in self.VKI_deadkey) {
                      if (key == this.firstChild.nodeValue) {
                        self.VKI_dead = key;
                        this.className = "dead";
                        if (self.VKI_shift) self.VKI_modify("Shift");
                        if (self.VKI_alternate) self.VKI_modify("AltGr");
                        break;
                      }
                    }
                    if (!self.VKI_dead) self.VKI_insert(this.firstChild.nodeValue);
                  } else self.VKI_insert(this.firstChild.nodeValue);
                }

                self.VKI_modify("");
                return false;
              }
            }
            ul.appendChild(li);
          div.appendChild(ul);
      }
      container.appendChild(div);
    }
  }

  this.VKI_buildKeys();
  this.VKI_keyboard.style.display = "none";
  document.body.appendChild(this.VKI_keyboard);


  /* ******************************************************************
   * Controls modifier keys
   *
   */
  this.VKI_modify = function(type) {
    switch (type) {
      case "Caps": this.VKI_capslock = !this.VKI_capslock; break;
      case "Shift": this.VKI_shift = !this.VKI_shift; break;
      case "Alt":
      case "AltGr": this.VKI_alternate = !this.VKI_alternate; break;
    }
    var vchar = 0;
    if (!this.VKI_shift != !this.VKI_capslock) vchar += 1;

    var uls = this.VKI_keyboard.getElementsByTagName('ul');
    for (var x = 0; x < uls.length; x++) {
      var lis = uls[x].getElementsByTagName('li');
      for (var y = 0; y < lis.length; y++) {
        if (type) lis[y].firstChild.nodeValue = this.VKI_layout[this.VKI_kt][x][y][vchar + ((this.VKI_alternate && this.VKI_layout[this.VKI_kt][x][y].length == 4) ? 2 : 0)];
        var char = lis[y].firstChild.nodeValue;

        var dead = alive = target = false;
        if (this.VKI_deadkeysOn) {
          if (this.VKI_dead) {
            if (char == this.VKI_dead) dead = true;
            for (var z = 0; z < this.VKI_deadkey[this.VKI_dead].length; z++)
              if (char == this.VKI_deadkey[this.VKI_dead][z][0]) { target = true; break; }
          }
          for (key in this.VKI_deadkey) if (key === char) { alive = true; break; }
        }

        lis[y].className = (dead) ? "dead" : ((target) ? "target" : ((alive) ? "alive" : ""));
      }
    }
    this.VKI_target.focus();
  }


  /* ******************************************************************
   * Insert text at the cursor
   *
   */
  this.VKI_insert = function(text) {
    this.VKI_target.focus();
    if (this.VKI_target.setSelectionRange) {
      var srt = this.VKI_target.selectionStart;
      var len = this.VKI_target.selectionEnd;
      this.VKI_target.value = this.VKI_target.value.substr(0, srt) + text + this.VKI_target.value.substr(len);
      if (text == "\n" && window.opera) srt++;
      this.VKI_target.setSelectionRange(srt + text.length, srt + text.length);
    } else if (this.VKI_target.createTextRange) {
      try { this.VKI_range.select(); } catch(e) {}
      this.VKI_range = document.selection.createRange();
      this.VKI_range.text = text;
      this.VKI_range.collapse(true);
      this.VKI_range.select();
    } else this.VKI_target.value += text;
    if (this.VKI_shift) this.VKI_modify("Shift");
    if (this.VKI_alternate) this.VKI_modify("AltGr");
    this.VKI_target.focus();
  }


  /* ******************************************************************
   * Show the keyboard interface
   *
   */
  this.VKI_show = function(id) {
    this.VKI_target = document.getElementById(id);

    if (this.VKI_visible != this.VKI_target.keyboardId) {
      this.VKI_range = "";
      this.VKI_keyboard.style.display = "none";

      var elem = this.VKI_target;
      this.VKI_target.keyboardPosition = "absolute";
      do {
        if (getStyle(elem, "position") == "fixed") {
          this.VKI_target.keyboardPosition = "fixed";
          break;
        }
      } while (elem = elem.offsetParent);

      this.VKI_keyboard.style.top = this.VKI_keyboard.style.right = this.VKI_keyboard.style.bottom = this.VKI_keyboard.style.left = "auto";
      this.VKI_keyboard.style.position = this.VKI_target.keyboardPosition;
      this.VKI_keyboard.style.display = "block";

      this.VKI_visible = this.VKI_target.keyboardId;
      this.VKI_position();

      this.VKI_target.focus();
    } else this.VKI_close();
  }


  /* ******************************************************************
   * Position the keyboard
   *
   */
  this.VKI_position = function() {
    if (this.VKI_visible > -1) {
      var inputElemPos = findPos(this.VKI_target);
      this.VKI_keyboard.style.top = inputElemPos[1] - ((this.VKI_target.keyboardPosition == "fixed") ? document.body.scrollTop : 0) + this.VKI_target.offsetHeight + 3 + "px";
      this.VKI_keyboard.style.left = Math.min(innerDimensions()[0] - this.VKI_keyboard.offsetWidth - 15, inputElemPos[0]) + "px";
    }
  }


  if (window.addEventListener) {
    window.addEventListener('resize', this.VKI_position, false); 
  } else if (window.attachEvent)
    window.attachEvent('onresize', this.VKI_position);


  /* ******************************************************************
   * Close the keyboard interface
   *
   */
  this.VKI_close = function() {
    this.VKI_keyboard.style.display = "none";
    this.VKI_visible = -1;
    this.VKI_target.focus();
    this.VKI_target = "";
  }
}


/* ***** Attach this script to the onload event ******************** */
if (window.addEventListener) {
  window.addEventListener('load', buildKeyboardInputs, false); 
} else if (window.attachEvent)
  window.attachEvent('onload', buildKeyboardInputs);


/* ********************************************************************
 * Handy element positioning function
 *
 */
function findPos(obj) {
  var curleft = curtop = 0;
  do {
    curleft += obj.offsetLeft;
    curtop += obj.offsetTop;
  } while (obj = obj.offsetParent);    
  return [curleft, curtop];
}


/* ********************************************************************
 * Return the dimensions of the viewport, also from Quirksmode.org
 *
 */
function innerDimensions() {
  if (self.innerHeight) {
    return [self.innerWidth, self.innerHeight];
  } else if (document.documentElement && document.documentElement.clientHeight) {
    return [document.documentElement.clientWidth, document.documentElement.clientHeight];
  } else if (document.body)
    return [document.body.clientWidth, document.body.clientHeight];
  return [0, 0];
}


/* ********************************************************************
 * Return an element's set style
 *
 */
function getStyle(obj, styleProp) {
  if (obj.currentStyle) {
    var y = obj.currentStyle[styleProp];
  } else if (window.getComputedStyle)
    var y = window.getComputedStyle(obj, null)[styleProp];
  return y;
}
