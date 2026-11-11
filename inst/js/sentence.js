/* The words of a script block's sentence, as its controls.
 *
 * R paints the band (sb_sentence_ui): a `.blockr-slot` span per live word and
 * a `.blockr-slot-offer` chip per control whose clause dropped, inside
 * `.blockr-script-sentence`, which carries `data-sb-slots` (what each word opens, as
 * JSON) and `data-sb-input` (the Shiny input a pick goes to). This file only
 * listens: one delegated handler for every script block on the page, so the
 * band can be redrawn on every pick without re-wiring.
 *
 * R decides which words are live (sb_slot_arg_names) and paints only those
 * with the slot mark; a word that cannot open anything is plain text there
 * and its control stays in the grid. The one thing R cannot see is the menu
 * script in the browser. If Blockr.Select.menu() is missing or has no multi
 * mode, the page says so in every band that has a list word (menuCheck), and
 * a click on such a word throws.
 *
 * A select or multi-select opens Blockr.Select.menu() (blockr.ui) under the
 * word. A checkbox has no list; the word is the switch. A number or a text
 * turns into a field in place, drawn as a field (border, focus ring, range),
 * after the chart block's (blockr.viz SentenceSlots._edit).
 */
(function () {
  if (window.__blockrScriptSentence) return;
  window.__blockrScriptSentence = true;

  var openMenu = null;
  var openAnchor = null;
  var openKey = null;
  // What is on the page while a word is open: the field of a number or a
  // text, the word itself for a list.
  var openEl = null;

  function mark(anchor, on) {
    anchor.classList.toggle('blockr-slot--open', on);
    anchor.setAttribute('aria-expanded', on ? 'true' : 'false');
  }

  function closeMenu() {
    if (openMenu) { openMenu.close(); openMenu = null; }
    if (openAnchor) { mark(openAnchor, false); openAnchor = null; }
    openKey = null;
    openEl = null;
  }

  function send(inputId, arg, value) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(inputId, { arg: arg, value: value, nonce: Date.now() },
                        { priority: 'event' });
  }

  function open(anchor) {
    var arg = anchor.getAttribute('data-slot-arg');
    var by = anchor.getAttribute('data-slot-by') || 'name';
    var host = anchor.closest('[data-sb-slots]');
    if (!host || !arg) return;
    var inputId = host.getAttribute('data-sb-input');
    var specs;
    try { specs = JSON.parse(host.getAttribute('data-sb-slots') || '{}'); }
    catch (e) { specs = {}; }
    var spec = specs[arg];
    if (!spec || !inputId) return;

    var key = inputId + '@' + arg;
    var wasKey = openKey;
    // R may have redrawn the band under an open field without a blur. What
    // was open is then off the page, and this click opens the word; it does
    // not close something that is no longer there.
    var wasOnPage = !!openEl && document.contains(openEl);
    closeMenu();
    if (wasKey === key && wasOnPage) return;  // the word toggles its own menu

    if (spec.kind === 'flag') {
      send(inputId, arg, !spec.value);
      return;
    }
    if (spec.kind === 'number' || spec.kind === 'text') {
      openInline(anchor, key, spec, inputId, arg);
      return;
    }
    var problem = menuCheck();
    if (problem) throw new Error('blockr.script: ' + problem);
    var multi = spec.kind === 'multi';
    openKey = key;
    openAnchor = anchor;
    openEl = anchor;
    mark(anchor, true);
    openMenu = Blockr.Select.menu(anchor, {
      mode: multi ? 'multi' : 'single',
      options: spec.options || [],
      selected: multi ? (spec.selected || []) : spec.selected,
      title: spec.label || arg,
      labelFirst: multi || by === 'label',
      maxTagChars: multi ? 48 : 0,
      searchPlaceholder: 'Filter',
      // Every pick redraws the band, so the word a multi menu hangs off is
      // replaced while it stays open. Find it again by input and argument.
      reanchor: function () {
        var hosts = document.querySelectorAll('[data-sb-slots]');
        for (var i = 0; i < hosts.length; i++) {
          if (hosts[i].getAttribute('data-sb-input') !== inputId) continue;
          var word = hosts[i].querySelector('.blockr-slot[data-slot-arg="' + arg + '"]') ||
            hosts[i].querySelector('.blockr-slot-offer[data-slot-arg="' + arg + '"]');
          if (word) {
            mark(word, true);
            openAnchor = word;
            openEl = word;
            return word;
          }
        }
        return null;
      },
      onChange: function (val) {
        send(inputId, arg, multi ? (Array.isArray(val) ? val : [val]) : val);
      },
      onClose: function () {
        if (openAnchor) mark(openAnchor, false);
        openAnchor = null;
        openMenu = null;
        openKey = null;
        openEl = null;
      }
    });
  }

  /* A number or a text word turns into a field in place: the word is
   * replaced by an input that looks like one (border, focus ring), holding
   * the current value, selected. Enter or leaving the field commits, Escape
   * puts the word back. A number steps on the arrow keys (by ten with Shift),
   * is held inside its range, and on its step when the control has one (a
   * control without a step takes any number, such as 0.25), and shows its range beside the
   * field ("1 to 5"), which says which end it will be held at while the typed
   * value is outside. An unchanged value sends nothing. */
  function openInline(anchor, key, spec, inputId, arg) {
    var isNum = spec.kind === 'number';
    var before = (spec.value === null || spec.value === undefined) ? '' : String(spec.value);
    var inp = document.createElement('input');
    inp.className = 'blockr-script-slot-input';
    inp.type = isNum ? 'number' : 'text';
    inp.value = before;
    inp.setAttribute('aria-label', spec.label || arg);
    if (!isNum && spec.placeholder) inp.placeholder = spec.placeholder;

    var lo = isNum && isFinite(parseFloat(spec.min)) ? Number(spec.min) : -Infinity;
    var hi = isNum && isFinite(parseFloat(spec.max)) ? Number(spec.max) : Infinity;
    // The step the control declares. Without one, a typed value is kept as
    // typed, and the arrow keys move by 1.
    var hasStep = isNum && Number(spec.step) > 0;
    var step = hasStep ? Number(spec.step) : 1;
    var rangeText = isFinite(lo) && isFinite(hi) ? lo + ' to ' + hi :
      isFinite(lo) ? 'at least ' + lo : isFinite(hi) ? 'at most ' + hi : '';
    function fix(v) {
      var on = v;
      if (hasStep) {
        var base = isFinite(lo) ? lo : 0;
        on = base + Math.round((v - base) / step) * step;
      }
      return Math.min(hi, Math.max(lo, Number(on.toFixed(10))));
    }
    function fit() {
      var n = Math.max(3, inp.value.length + 1);
      inp.style.width = 'calc(' + n + 'ch + 12px)';
    }

    var note = null;
    if (isNum && rangeText) {
      note = document.createElement('span');
      note.className = 'blockr-script-slot-range';
      note.textContent = rangeText;
    }
    function check() {
      if (!note) return;
      var v = parseFloat(inp.value);
      var out = isFinite(v) && (v < lo || v > hi);
      note.classList.toggle('blockr-script-slot-range--out', out);
      note.textContent = !out ? rangeText : (v > hi ? 'at most ' + hi : 'at least ' + lo);
    }

    var done = false;
    function close(commit) {
      if (done) return;
      done = true;
      var val = inp.value.trim();
      if (note && note.parentNode) note.parentNode.removeChild(note);
      // The band may have been redrawn under the field; then there is no word
      // to put back, and the new band already shows the current value.
      if (inp.parentNode) inp.replaceWith(anchor);
      mark(anchor, false);
      if (openKey === key) { openAnchor = null; openMenu = null; openKey = null; openEl = null; }
      if (isNum) {
        var v = parseFloat(val);
        val = isFinite(v) ? String(fix(v)) : before;
      }
      if (commit && val !== before) send(inputId, arg, val);
      else if (document.contains(anchor)) anchor.focus();
    }

    inp.addEventListener('input', function () { fit(); check(); });
    inp.addEventListener('click', function (e) { e.stopPropagation(); });
    inp.addEventListener('keydown', function (e) {
      e.stopPropagation();
      if (isNum && (e.key === 'ArrowUp' || e.key === 'ArrowDown')) {
        e.preventDefault();
        var v = parseFloat(inp.value);
        var by = step * (e.shiftKey ? 10 : 1) * (e.key === 'ArrowUp' ? 1 : -1);
        inp.value = String(fix((isFinite(v) ? v : (isFinite(lo) ? lo : 0)) + by));
        fit(); check();
        inp.select();
      }
      if (e.key === 'Enter') { e.preventDefault(); close(true); }
      if (e.key === 'Escape') { e.preventDefault(); close(false); }
    });
    inp.addEventListener('blur', function () { close(true); });

    openKey = key;
    openAnchor = anchor;
    openEl = inp;
    openMenu = { close: function () { close(false); } };
    anchor.replaceWith(inp);
    if (note) inp.after(note);
    fit();
    inp.focus();
    inp.select();
  }

  /* The menu comes from blockr.ui. A missing or older one is a deployment
   * mistake: say so on the page, not only on click. */
  function menuProblem() {
    if (!window.Blockr || !Blockr.Select || !Blockr.Select.menu) {
      return 'Blockr.Select.menu() is missing: blockr.ui is not loaded or too old';
    }
    if (!Blockr.Select.menuMulti) {
      return 'Blockr.Select.menu() has no multi mode: blockr.ui is too old';
    }
    return null;
  }

  function menuCheck() {
    var problem = menuProblem();
    document.documentElement.classList.toggle('blockr-script-no-menu', !!problem);
    if (problem && window.console) {
      console.error('blockr.script: the sentence\'s list words cannot open. ' + problem + '.');
    }
    return problem;
  }

  if (document.readyState === 'complete') menuCheck();
  else window.addEventListener('load', menuCheck);

  var SEL = '.blockr-script-sentence .blockr-slot[data-slot-arg], ' +
            '.blockr-script-sentence .blockr-slot-offer[data-slot-arg]';

  document.addEventListener('click', function (e) {
    var t = e.target && e.target.closest ? e.target.closest(SEL) : null;
    if (!t) return;
    e.stopPropagation();
    open(t);
  });

  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Enter' && e.key !== ' ') return;
    var t = e.target && e.target.closest ? e.target.closest(SEL) : null;
    if (!t) return;
    e.preventDefault();
    open(t);
  });
})();
