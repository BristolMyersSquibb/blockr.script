/**
 * sb-select — a generic Shiny input binding that mounts a `Blockr.Select`
 * (single or multi) over an empty container, so function-block parameter
 * fields use the same select component as blockr.dplyr.
 *
 * Unlike blockr.dm's dm-table-picker (custom-message driven), this is a
 * self-initializing input binding: the function block renders its fields via
 * renderUI, so we let Shiny bind/unbind the control as the field set changes.
 * All config travels in `data-*` attributes on the container:
 *
 *   <div class="blockr-select-input"
 *        data-multiple="true|false"
 *        data-options='[{"value":"a","label":"A"}, ...]'
 *        data-selected='"a"'  (single) | '["a","b"]' (multi)
 *        data-placeholder="Select…"
 *        data-single-line="true|false"
 *        data-tag-chars="16"></div>
 *
 * Depends on: blockr-core.js, blockr-select.js (via blockr_select_dep()).
 */
(() => {
  'use strict';

  const parseJSON = (raw, fallback) => {
    if (raw == null || raw === '') return fallback;
    try {
      return JSON.parse(raw);
    } catch (e) {
      return fallback;
    }
  };

  const mount = (el) => {
    if (el._fbSelect) return el._fbSelect;
    const multiple = el.getAttribute('data-multiple') === 'true';
    const options = parseJSON(el.getAttribute('data-options'), []);
    const selected = parseJSON(el.getAttribute('data-selected'), multiple ? [] : null);
    const placeholder = el.getAttribute('data-placeholder') || '';
    // The params grid sizes a row to its tallest field, so a multi-select that
    // wraps its tags is what makes the generated band tall. One row plus a
    // "+N" chip instead.
    const singleLine = el.getAttribute('data-single-line') !== 'false';
    // Long values are the norm here (arm labels, derived variable names), and
    // one of them can fill the row on its own.
    const tagChars = parseInt(el.getAttribute('data-tag-chars'), 10) || 0;

    const slot = document.createElement('div');
    el.appendChild(slot);

    const factory = multiple ? Blockr.Select.multi : Blockr.Select.single;
    el._fbSelect = factory(slot, {
      options: options,
      selected: selected,
      placeholder: placeholder,
      reorderable: true,
      singleLine: multiple && singleLine,
      maxTagChars: tagChars,
      onChange: () => {
        if (typeof el._fbOnChange === 'function') el._fbOnChange();
      }
    });
    el._fbSelect.el.classList.add('blockr-select--bordered');
    return el._fbSelect;
  };

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: function (scope) {
      // Shiny hands `find` a jQuery-wrapped scope; `$(scope)` also tolerates a
      // raw DOM node, so this is the portable form.
      return $(scope).find('.blockr-select-input');
    },
    initialize: function (el) {
      mount(el);
    },
    getValue: function (el) {
      const sel = el._fbSelect || mount(el);
      return sel.getValue();
    },
    subscribe: function (el, callback) {
      el._fbOnChange = () => callback(true);
    },
    unsubscribe: function (el) {
      el._fbOnChange = null;
      if (el._fbSelect) {
        el._fbSelect.destroy();
        el._fbSelect = null;
      }
    },
    // Allow the server to swap options/selection without a full re-render.
    receiveMessage: function (el, data) {
      const sel = el._fbSelect || mount(el);
      if (data && data.options) {
        sel.setOptions(data.options, data.selected);
        if (typeof el._fbOnChange === 'function') el._fbOnChange();
      }
    },
    getType: function () {
      return false;
    }
  });

  // Shiny (and Blockr.Select) may not be ready when this IIFE runs. The wait
  // ends after ten seconds: a page that never loads them has a deployment
  // problem, and polling forever would hide it.
  const started = Date.now();
  const register = () => {
    if (typeof Shiny === 'undefined' || !Shiny.inputBindings ||
        typeof Blockr === 'undefined' || !Blockr.Select) {
      if (Date.now() - started > 10000) {
        console.error('blockr.script: the dropdown controls cannot start. ' +
          'Shiny or Blockr.Select (blockr.ui) did not load.');
        return;
      }
      setTimeout(register, 50);
      return;
    }
    Shiny.inputBindings.register(binding, 'blockr.script.select');
  };
  register();
})();
