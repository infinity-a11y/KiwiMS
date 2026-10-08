// Deconvolution spinner guard
// Keeps the deconv-pre-init class (which hides the shinycssloaders spinner)
// until the user first interacts with the deconvolution main panel or sidebar.
// This prevents a spinner flash on initial page load.
document.addEventListener('DOMContentLoaded', function () {
  var cid = 'app-deconvolution_main-deconvolution_ui_container';

  function activate() {
    var el = document.getElementById(cid);
    if (el) el.classList.remove('deconv-pre-init');
    document.body.removeEventListener('change', h, true);
    document.body.removeEventListener('click', h, true);
  }

  function h(e) {
    var c = document.getElementById(cid);
    var s = document.querySelector('.deconvolution-sidebar');
    if ((c && c.contains(e.target)) || (s && s.contains(e.target))) activate();
  }

  document.body.addEventListener('change', h, true);
  document.body.addEventListener('click', h, true);
});

// Target file list (Start Deconvolution dialog)
// Spreadsheet-style selection on the .kiwi-file-list checkbox groups:
//   click               toggle one file (it becomes the anchor)
//   Shift+click         give the range anchor..file the anchor's state
//   drag                paint the first file's new state over the rows passed,
//                       scrolling the list when held near its edges
//   Up/Down, Home/End   move focus; with Shift the rows passed take the
//                       anchor's state
//   Enter (or Space)    toggle (with Shift: range, as Shift+click)
//   Ctrl/Cmd+A          select all, or deselect all when all are selected
// Shiny hears one change event per gesture, not one per file.
(function () {
  var BOX = 'input[type="checkbox"]';
  var anchor = null;
  var drag = null;

  function boxes(list) {
    return Array.prototype.slice.call(list.querySelectorAll(BOX));
  }

  function setRange(all, from, to, state) {
    var lo = Math.min(from, to), hi = Math.max(from, to);
    for (var k = lo; k <= hi; k++) all[k].checked = state;
  }

  function notify(list) {
    var first = list.querySelector(BOX);
    if (first) first.dispatchEvent(new Event('change', { bubbles: true }));
  }

  function anchorIn(all) {
    return anchor ? all.indexOf(anchor) : -1;
  }

  // Paint up to the row under (x, y), clamped into the list so a pointer
  // above/below or beside it still reaches the edge rows
  function paintAt(x, y) {
    var r = drag.list.getBoundingClientRect();
    var cx = Math.min(Math.max(x, r.left + 4), r.right - 20);
    var cy = Math.min(Math.max(y, r.top + 2), r.bottom - 2);
    var el = document.elementFromPoint(cx, cy);
    var row = el && el.closest('.checkbox');
    if (!row || !drag.list.contains(row)) return;
    var idx = drag.all.indexOf(row.querySelector(BOX));
    if (idx < 0) return;
    setRange(drag.all, drag.last, idx, drag.state);
    drag.last = idx;
  }

  function autoscroll() {
    if (!drag) return;
    var r = drag.list.getBoundingClientRect(), edge = 24, dy = 0;
    if (drag.y < r.top + edge) dy = -Math.ceil((r.top + edge - drag.y) / 3);
    else if (drag.y > r.bottom - edge) dy = Math.ceil((drag.y - r.bottom + edge) / 3);
    if (dy) {
      drag.list.scrollTop += dy;
      paintAt(drag.x, drag.y);
    }
    drag.raf = requestAnimationFrame(autoscroll);
  }

  function endDrag() {
    if (!drag) return;
    cancelAnimationFrame(drag.raf);
    notify(drag.list);
    drag = null;
  }

  document.addEventListener('mousedown', function (e) {
    if (e.button !== 0) return;
    var row = e.target.closest && e.target.closest('.kiwi-file-list .checkbox');
    if (!row) return;
    var box = row.querySelector(BOX);
    if (!box) return;
    // No text selection, and the click that follows is cancelled below:
    // the state is set here so drag and Shift work the same way
    e.preventDefault();

    var list = row.closest('.kiwi-file-list');
    var all = boxes(list);
    var i = all.indexOf(box);
    var a = anchorIn(all);
    var state;

    if (e.shiftKey && a >= 0) {
      state = anchor.checked;
      setRange(all, a, i, state);
    } else {
      state = !box.checked;
      box.checked = state;
      anchor = box;
    }
    box.focus({ preventScroll: true });

    drag = { list: list, all: all, state: state, last: i, x: e.clientX, y: e.clientY };
    drag.raf = requestAnimationFrame(autoscroll);
  });

  document.addEventListener('mousemove', function (e) {
    if (!drag) return;
    if (!(e.buttons & 1)) return endDrag();
    drag.x = e.clientX;
    drag.y = e.clientY;
    paintAt(e.clientX, e.clientY);
  });

  window.addEventListener('mouseup', endDrag);
  window.addEventListener('blur', endDrag);

  // Mouse clicks were handled on mousedown. Keyboard clicks (Space, detail 0)
  // keep the native toggle.
  document.addEventListener('click', function (e) {
    if (e.detail === 0) return;
    if (e.target.closest && e.target.closest('.kiwi-file-list .checkbox')) {
      e.preventDefault();
    }
  }, true);

  // A native toggle (Space) moves the anchor; our own notify() events are
  // untrusted and leave it alone
  document.addEventListener('change', function (e) {
    if (e.isTrusted && e.target.matches && e.target.matches('.kiwi-file-list ' + BOX)) {
      anchor = e.target;
    }
  });

  // One tab stop for the whole list instead of one per file
  document.addEventListener('focusin', function (e) {
    var t = e.target;
    if (!t.matches || !t.matches('.kiwi-file-list ' + BOX)) return;
    boxes(t.closest('.kiwi-file-list')).forEach(function (b) {
      b.tabIndex = b === t ? 0 : -1;
    });
  });

  document.addEventListener('keydown', function (e) {
    var t = e.target;
    if (!t || !t.closest) return;

    if ((e.ctrlKey || e.metaKey) && !e.altKey && (e.key === 'a' || e.key === 'A')) {
      // Inside the picker, or on the freshly opened dialog itself (never in
      // a text field, where Ctrl+A selects text)
      var sel = t.closest('.kiwi-file-selector');
      var host = sel || (t.classList.contains('modal') ? t : null);
      var target = host && host.querySelector('.kiwi-file-list');
      if (!target) return;
      e.preventDefault();
      var every = boxes(target);
      var on = !every.every(function (b) { return b.checked; });
      every.forEach(function (b) { b.checked = on; });
      notify(target);
      return;
    }

    if (!t.matches('.kiwi-file-list ' + BOX)) return;
    var list = t.closest('.kiwi-file-list');
    var all = boxes(list);
    var i = all.indexOf(t);
    var a = anchorIn(all);
    var j;

    switch (e.key) {
      case 'ArrowDown': j = Math.min(i + 1, all.length - 1); break;
      case 'ArrowUp': j = Math.max(i - 1, 0); break;
      case 'Home': j = 0; break;
      case 'End': j = all.length - 1; break;
      case 'Enter':
        e.preventDefault();
        if (e.shiftKey && a >= 0) {
          setRange(all, a, i, anchor.checked);
        } else {
          t.checked = !t.checked;
          anchor = t;
        }
        notify(list);
        return;
      case ' ':
        if (e.shiftKey && a >= 0) {
          e.preventDefault();
          setRange(all, a, i, anchor.checked);
          notify(list);
        }
        return;
      default:
        return;
    }

    e.preventDefault();
    if (e.shiftKey) {
      if (a < 0) {
        anchor = t;
        t.checked = true;
      }
      setRange(all, i, j, anchor.checked);
      notify(list);
    }
    all[j].focus();
    all[j].scrollIntoView({ block: 'nearest' });
  });
})();
