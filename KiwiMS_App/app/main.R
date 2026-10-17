# app/main.R

box::use(
  bslib,
  shiny,
  shinyjs[disable, enable, hide, hidden, show, runjs, useShinyjs],
  shinyWidgets[show_toast],
  waiter[useWaiter, waiter_hide, waiterShowOnLoad],
)

box::use(
  app / logic / dev_utils,
  app /
    logic /
    main_ui[licence_modal_body, unidec_modal_body, update_modal_body],
  app /
    logic /
    main_ui[licence_modal_body, unidec_modal_body, update_modal_body],
  app / view / conversion_main,
  app / view / conversion_sidebar,
  app / view / deconvolution_main,
  app / view / deconvolution_sidebar,
  app / view / log_view,
  app / view / log_sidebar,
  app / logic / logging[start_logging, write_log, close_logging],
  app /
    logic /
    user_settings[
      read_user_settings,
      save_user_settings,
      update_user_setting,
      get_default_user_settings,
    ],
  app /
    logic /
    helper_functions[
      check_github_version,
      config_badge,
      config_icon,
      get_kiwims_version,
      get_latest_release_url,
      normalize_colnames,
      normalize_config_units,
      read_config_file,
      validate_config,
    ],
  app / logic / ms_formats[has_ms_extension],
  app /
    logic /
    conversion_functions[hit_preference_choices, hit_preference_rule],
)

suppressWarnings(library(logr))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    dev_utils$add_dev_headers(),
    shiny$tags$head(
      shiny$tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0(
          "static/css/app.min.css?v=",
          get_kiwims_version()["version"]
        )
      )
    ),
    shiny$div(id = "blocking-overlay"),
    shiny$tags$script(shiny$HTML(
      "
      (function() {
        var map = new Map();
        $(document).on('shiny:recalculating', function(event) {
          var el = event.target;
          if (el.closest && el.closest('.conversion-main-spinner')) {
            el.classList.add('kiwi-rendering');
            map.set(el.id, el);
          }
        });
        $(document).on('shiny:value shiny:error', function(event) {
          var el = map.get(event.name);
          if (!el) return;
          map.delete(event.name);
          requestAnimationFrame(function() {
            requestAnimationFrame(function() {
              el.classList.remove('kiwi-rendering');
            });
          });
        });
      })();
    "
    )),
    shiny$tags$script(shiny$HTML(
      "
      // Legend rows that only explain a symbol or colour (legendgroup prefix
      // 'kiwims-key-', see legend_key_group()) draw no data, so a click would
      // only grey out the row and a double click would hide every trace that
      // does. plotly cancels the click when a jQuery handler on the plot
      // returns false; plots announce themselves through the bubbling
      // plotly_afterplot event, and the handler is bound once per plot.
      (function() {
        function keyRowClick(e, d) {
          var t = d && d.fullData && d.fullData[d.curveNumber];
          if (t && String(t.legendgroup || '').indexOf('kiwims-key-') === 0) {
            return false;
          }
        }
        $(document).on('plotly_afterplot', function(e) {
          var $gd = $(e.target);
          if ($gd.data('kiwiKeyGuard')) return;
          $gd.data('kiwiKeyGuard', true);
          $gd.on('plotly_legendclick.kiwiKey', keyRowClick);
        });
      })();
    "
    )),
    shiny$tags$script(shiny$HTML(
      "
      // Captured from the on-screen plot at click time (see the click handler
      // below) so a zoomed-in export matches what the user is looking at
      // instead of always re-rendering the full, unzoomed figure — the
      // server only ever knows the plot's default view, never the client-side
      // zoom state, so this has to be read out of the live Plotly DOM node.
      var _kiwiPendingZoomRange = null;
      var _kiwiPendingCamera = null;

      // Annotations and axis titles are cut off by the SVG viewport as soon as
      // they're drawn past the edge of the paper - a label anchored above a
      // plot area whose top margin is 0, for one. Estimating how much room
      // those need from font size and text length is guesswork, so instead
      // render once, measure what actually overflowed, and hand back exactly
      // that much. Deliberately left/top/bottom only: a legend at x > 1 is
      // NOT part of this - see kiwiFitLegend for why measuring its right-edge
      // overflow the same way is actively misleading.
      function kiwiFitPaper(div) {
        var paper = div.getBoundingClientRect();
        if (!paper.width || !paper.height) return Promise.resolve();
        var grow = { l: 0, t: 0, b: 0 };
        var anns = (div.layout && div.layout.annotations) || [];
        var nodes = div.querySelectorAll('.infolayer .annotation, .infolayer .g-gtitle');
        Array.prototype.forEach.call(nodes, function(node) {
          // The side tables hang from the top of the plot area, so a bigger
          // bottom margin never brings them back onto the canvas - it only
          // squashed the spectrum. kiwiFitSideTables sizes the canvas for them.
          var ann = anns[+node.getAttribute('data-index')];
          if (node.classList.contains('annotation') && kiwiIsSideTable(ann)) return;
          var b = node.getBoundingClientRect();
          if (!b.width && !b.height) return;
          grow.l = Math.max(grow.l, paper.left - b.left);
          grow.t = Math.max(grow.t, paper.top - b.top);
          grow.b = Math.max(grow.b, b.bottom - paper.bottom);
        });
        var m = (div._fullLayout && div._fullLayout.margin) || {};
        var update = {}, any = false;
        ['l', 't', 'b'].forEach(function(side) {
          if (grow[side] > 1) {
            update['margin.' + side] = Math.ceil((m[side] || 0) + grow[side] + 8);
            any = true;
          }
        });
        return any ? Plotly.relayout(div, update) : Promise.resolve();
      }

      // The tables add_side_tables() (deconvolution_functions.R) draws beside
      // a spectrum: annotations and pixel-sized lines carrying this name.
      function kiwiIsSideTable(item) {
        return !!item && item.name === 'kiwims-side-table';
      }

      function kiwiHasSideTables(layout) {
        return (layout.shapes || []).some(kiwiIsSideTable);
      }

      // The tables are placed in pixels down from the top of the plot area,
      // so once their text is scaled up for export they can run longer than
      // the plot is tall and off the bottom of the canvas. Grow the canvas by
      // exactly the shortfall: the top margin stays put, so the plot area
      // takes all of the extra height and the tables end inside it. Resolves
      // to the canvas height.
      function kiwiFitSideTables(div, height) {
        var extent = 0;
        ((div.layout && div.layout.shapes) || []).forEach(function(s) {
          if (!kiwiIsSideTable(s) || s.ysizemode !== 'pixel') return;
          extent = Math.max(extent, -Math.min(s.y0, s.y1));
        });
        var plotHeight = div._fullLayout && div._fullLayout._size &&
                         div._fullLayout._size.h;
        if (!(extent > 0) || !(plotHeight > 0)) return Promise.resolve(height);
        var shortfall = Math.ceil(extent + 8 - plotHeight);
        if (shortfall <= 0) return Promise.resolve(height);
        var newHeight = height + shortfall;
        div.style.height = newHeight + 'px';
        return Plotly.relayout(div, { height: newHeight })
          .then(function() { return newHeight; });
      }

      // A legend at the default x=1.02/xref='paper' is NOT positioned relative
      // to margin.r at all - it is paper-relative, and Plotly clamps it to
      // paperWidth - legend._width so it never draws past the right edge of the
      // canvas. That clamp is why measuring whether the legend crosses
      // paper.right always reads ~0: it can't not be true, whatever margin.r
      // is set to.
      // Confirmed against the real renderer (not just this comment): growing
      // margin.r from 0 to several thousand px moved the legend's rendered
      // position by exactly zero pixels.
      //
      // What actually happens when the legend is wider than the room left of
      // that clamp point is it lands on top of the plot instead of past its
      // edge - spectrum_plot() sets margin.r = 0 outright, so this is the
      // normal case for any legend entry longer than a couple of characters
      // once the label-size scaling below is applied. The only lever that
      // moves a paper-relative legend is the paper's own width, so this grows
      // it by exactly the legend's measured width and reserves that same
      // amount as margin.r, which keeps the plot itself the same size and
      // simply appends a legend-width gutter to its right.
      function kiwiFitLegend(div, width) {
        if (div._fullLayout.showlegend === false) return Promise.resolve(width);
        var legend = div.querySelector('.infolayer .legend');
        if (!legend) return Promise.resolve(width);
        var legendWidth = kiwiLegendWidth(div);
        if (!(legendWidth > 0)) return Promise.resolve(width);
        var domainRight = div.getBoundingClientRect().right -
          ((div._fullLayout.margin && div._fullLayout.margin.r) || 0);
        if (legend.getBoundingClientRect().left >= domainRight - 1) {
          return Promise.resolve(width);
        }
        // Room for the safety margin kiwiFitLegendClip takes, plus a visible
        // gap: at export sizes a fixed 12px gutter is invisible and the last
        // glyph ends up flush against the edge of the page.
        var neededR = Math.ceil(legendWidth + kiwiLegendSafetyPad(div) + 12);
        var newWidth = width + neededR;
        div.style.width = newWidth + 'px';
        return Plotly.relayout(div, { width: newWidth, 'margin.r': neededR })
          .then(function() { return newWidth; });
      }

      // How wide the legend really is, which is not always what Plotly thinks.
      // legend._width is Plotly's own estimate and it is what the legend's clip
      // rectangle is cut to, so when the estimate falls short of the text that
      // actually got drawn the last characters are shaved off - the compound
      // name ending mid-glyph at the edge of an export. Measuring the rendered
      // contents gives the true number; the bigger of the two is the one to
      // reserve room for.
      function kiwiLegendWidth(div) {
        var declared = (div._fullLayout.legend && div._fullLayout.legend._width) || 0;
        return Math.max(declared, kiwiLegendContentWidth(div));
      }

      function kiwiLegendContentWidth(div) {
        var scrollbox = div.querySelector('.infolayer .legend .scrollbox');
        if (!scrollbox || !scrollbox.getBBox) return 0;
        try {
          var box = scrollbox.getBBox();
          return Math.ceil(box.x + box.width + 4);
        } catch (e) {
          return 0;
        }
      }

      // Roughly one character at the legend's own type size. Both the measured
      // content width and Plotly's own estimate come from text metrics, and a
      // shortfall of a few percent is the difference between a compound name
      // ending in a digit and one ending in half a digit - so the legend gets
      // a character of slack on top of whatever was measured rather than being
      // trusted to the pixel.
      function kiwiLegendSafetyPad(div) {
        var size = (div._fullLayout.legend && div._fullLayout.legend.font &&
                    div._fullLayout.legend.font.size) || 12;
        return Math.max(12, Math.ceil(size * 0.75));
      }

      // Widens the legend's clip rectangle past whatever was actually drawn and
      // slides the legend left by the same amount, so the extra width comes out
      // of the gutter kiwiFitLegend reserved rather than off the edge of the
      // page. Has to run after the final relayout, because a relayout redraws
      // the legend and puts the original clip back.
      function kiwiFitLegendClip(div) {
        var legend = div.querySelector('.infolayer .legend');
        if (!legend) return;
        var scrollbox = legend.querySelector('.scrollbox');
        if (!scrollbox) return;
        var declared = (div._fullLayout.legend && div._fullLayout.legend._width) || 0;
        var extra = Math.max(0, kiwiLegendContentWidth(div) - declared) +
                    kiwiLegendSafetyPad(div);
        var clip = scrollbox.getAttribute('clip-path') || '';
        var id = clip.replace(/^url\\(['\"]?#?/, '').replace(/['\"]?\\)$/, '');
        if (id) {
          var clipRect = div.querySelector('[id=\"' + id + '\"] rect');
          if (clipRect)
            clipRect.setAttribute(
              'width', parseFloat(clipRect.getAttribute('width') || 0) + extra);
        }
        var bg = legend.querySelector('rect.bg');
        if (bg)
          bg.setAttribute('width', parseFloat(bg.getAttribute('width') || 0) + extra);
        var move = /translate\\(\\s*([-0-9.]+)[ ,]\\s*([-0-9.]+)\\s*\\)/.exec(
          legend.getAttribute('transform') || '');
        if (!move) return;
        var legendX = parseFloat(move[1]);
        var paperWidth = div._fullLayout.width || 0;
        // Plotly does not always pull the legend back onto the canvas: left to
        // itself it can sit at x = 1.02 of the plot area and hang off the right
        // edge, which is the last glyph of a compound name going missing. Slide
        // it by whatever that overhang actually measures, never less than the
        // safety margin...
        var rightEdge = legendX + Math.max(declared, kiwiLegendContentWidth(div));
        var pad = kiwiLegendSafetyPad(div);
        var shift = Math.max(pad, rightEdge + pad - paperWidth);
        // ...and never more than the gutter kiwiFitLegend reserved, or the
        // legend backs out of it and onto the plot.
        var domainRight = paperWidth -
                          ((div._fullLayout.margin && div._fullLayout.margin.r) || 0);
        shift = Math.min(shift, Math.max(0, legendX - domainRight));
        legend.setAttribute(
          'transform', 'translate(' + (legendX - shift) + ',' + move[2] + ')');
      }

      // The only way past Plotly's 16px clamp on legend key symbols: resize
      // them in the rendered SVG. The symbol paths are drawn centred on their
      // own translate origin, so appending a scale keeps each one centred where
      // it already sits. Runs after the last relayout, since a relayout redraws
      // the legend.
      function kiwiScaleLegendSymbols(div, scale) {
        if (!(scale > 1)) return;
        var nodes = div.querySelectorAll('.legend .legendpoints path');
        Array.prototype.forEach.call(nodes, function(p) {
          var t = p.getAttribute('transform') || '';
          p.setAttribute('transform', t + ' scale(' + scale + ')');
          // A scale multiplies the outline along with the shape, so the key
          // came out ringed in a border several times heavier than the same
          // marker carries inside the plot. Dividing it back out leaves the
          // stroke at the width-to-size ratio the in-plot markers hold, which
          // is constant across label sizes.
          var w = parseFloat(p.style.strokeWidth || p.getAttribute('stroke-width'));
          if (w > 0) p.style.strokeWidth = (w / scale) + 'px';
        });
      }

      function kiwiWait(ms) {
        return new Promise(function(resolve) { setTimeout(resolve, ms); });
      }

      // A 3D scene is drawn by WebGL onto a transparent canvas laid over its
      // part of the plot. Draws a frame of it and reads it back at layout
      // pixels, whatever ratio the canvas is drawn at: { px (RGBA), w, h,
      // frame }, the frame being the canvas's box in the div's own pixels.
      function kiwiScenePixels(div, key) {
        var scene = div._fullLayout[key] && div._fullLayout[key]._scene;
        var canvas = scene && ((scene.glplot && scene.glplot.canvas) || scene.canvas);
        if (!canvas || !canvas.width) return null;
        var page = div.getBoundingClientRect();
        var box = canvas.getBoundingClientRect();
        var w = Math.max(1, Math.round(box.width));
        var h = Math.max(1, Math.round(box.height));
        var probe = document.createElement('canvas');
        probe.width = w;
        probe.height = h;
        var ctx = probe.getContext('2d', { willReadFrequently: true });
        // The canvas only holds a frame until the browser has shown it, so
        // draw one and read it in the same go, as Plotly's own toImage does.
        if (scene.glplot && scene.glplot.redraw) scene.glplot.redraw();
        ctx.drawImage(canvas, 0, 0, w, h);
        var l = box.left - page.left, t = box.top - page.top;
        return {
          px: ctx.getImageData(0, 0, w, h).data, w: w, h: h,
          frame: { left: l, top: t, right: l + w, bottom: t + h }
        };
      }

      // Whatever is not transparent on the scene's canvas is the figure
      // itself: lines, markers, grid and axis labels. Returns its box in the
      // div's own pixels, along with the box of the canvas (frame), or null.
      function kiwiSceneContentBox(div, key) {
        var shot = kiwiScenePixels(div, key);
        if (!shot) return null;
        var w = shot.w, h = shot.h, px = shot.px;
        var x0 = w, y0 = h, x1 = -1, y1 = -1;
        for (var y = 0; y < h; y++) {
          for (var x = 0; x < w; x++) {
            if (px[(y * w + x) * 4 + 3] <= 8) continue;
            if (x < x0) x0 = x;
            if (x > x1) x1 = x;
            if (y < y0) y0 = y;
            y1 = y;
          }
        }
        if (x1 < 0) return null;
        var f = shot.frame;
        return {
          left: f.left + x0, top: f.top + y0, right: f.left + x1 + 1,
          bottom: f.top + y1 + 1, frame: f
        };
      }

      // The pixels that differ between two reads of the scene, as a 0/1 mask
      function kiwiPixelDiff(a, b) {
        var n = a.w * a.h, mask = new Uint8Array(n);
        for (var i = 0; i < n; i++) {
          var k = i * 4;
          if (Math.abs(a.px[k] - b.px[k]) + Math.abs(a.px[k + 1] - b.px[k + 1]) +
              Math.abs(a.px[k + 2] - b.px[k + 2]) + Math.abs(a.px[k + 3] - b.px[k + 3]) > 40)
            mask[i] = 1;
        }
        return mask;
      }

      // Grows a mask by r pixels in every direction (a square neighbourhood)
      function kiwiDilate(mask, w, h, r) {
        var tmp = new Uint8Array(w * h), out = new Uint8Array(w * h);
        for (var y = 0; y < h; y++) {
          var run = -Infinity;
          for (var x = 0; x < w; x++) {
            if (mask[y * w + x]) run = x;
            if (x - run <= r) tmp[y * w + x] = 1;
          }
          run = Infinity;
          for (x = w - 1; x >= 0; x--) {
            if (mask[y * w + x]) run = x;
            if (run - x <= r) tmp[y * w + x] = 1;
          }
        }
        for (x = 0; x < w; x++) {
          run = -Infinity;
          for (y = 0; y < h; y++) {
            if (tmp[y * w + x]) run = y;
            if (y - run <= r) out[y * w + x] = 1;
          }
          run = Infinity;
          for (y = h - 1; y >= 0; y--) {
            if (tmp[y * w + x]) run = y;
            if (run - y <= r) out[y * w + x] = 1;
          }
        }
        return out;
      }

      // Plotly places 3D tick labels and axis titles fixed distances out from
      // their axes (tickPad, labelPad), whatever size they are drawn at, so
      // at larger label sizes, or with long tick labels, the ticks run into
      // the titles, and the tick labels of two axes into each other where
      // the axes meet. Nothing in the layout reaches those distances, but the
      // scene keeps them on its axes options, where a replot leaves them
      // alone.
      //
      // How the pads work, as plotly's gl3d axes draw them: an axis's labels
      // are pushed out at right angles to it, component j of that push
      // scaled by pad[j] - so pad[j] moves the labels of every axis whose
      // push has a j part, and never those of axis j itself. A title is
      // pushed on from where its tick labels were pushed to, so it has to be
      // measured with them in place: they are made transparent, not hidden.
      //
      // The labels are drawn a group at a time and compared pixel by pixel,
      // a clash being any pixel of one within a gap of another. Each round
      // tries growing each pad component and keeps the one that leaves the
      // least clash - first among the tick labels of different axes, then
      // between the titles and the tick labels - until nothing clashes, or
      // no step helps any more.
      //
      // Where two axes meet, both their end labels are pushed the same way,
      // and no pad parts them. What is left of such a clash is resolved by
      // leaving out the clashing tick label of the axis that has more of
      // them (the intensity axis's 0 rather than the first mass), through
      // the axis's tickvals, and checking again.
      //
      // Resolves to whether anything moved.
      function kiwiSeparateSceneLabels(div, key, depth) {
        depth = depth || 0;
        var scene = div._fullLayout[key] && div._fullLayout[key]._scene;
        var opts = scene && scene.axesOptions;
        if (!opts || !opts.labelPad || !opts.tickPad || !opts.tickColor ||
            !opts.ticks || !scene.glplot || !scene.glplot.axes)
          return Promise.resolve(false);
        var ticksOn = opts.tickEnable.slice(), titlesOn = opts.labelEnable.slice();
        var tickColors = opts.tickColor.map(function(c) { return c.slice(); });
        var ALL = 3, NONE = -1;
        // Draws the tick labels of one axis (or ALL, or NONE) and one title
        function shoot(tickAxis, titleAxis) {
          for (var i = 0; i < 3; i++) {
            var c = tickColors[i];
            var shown = tickAxis === ALL || tickAxis === i;
            opts.tickColor[i] = [c[0], c[1], c[2], shown ? c[3] : 0];
            opts.labelEnable[i] = i === titleAxis && titlesOn[i];
          }
          scene.glplot.axes.update(opts);
          return kiwiScenePixels(div, key);
        }
        function overlap(a, b) {
          var n = 0;
          for (var p = 0; p < a.length; p++) if (a[p] === 1 && b[p] === 1) n++;
          return n;
        }
        var moved = false, drop = null;
        var bare, gap, step;
        function relieve(pads, clash) {
          var now = clash();
          for (var round = 0; round < 16 && now > 0; round++) {
            var best = -1, least = now;
            for (var j = 0; j < 3; j++) {
              pads[j] += step;
              var c = clash();
              pads[j] -= step;
              if (c < least) { best = j; least = c; }
            }
            if (best < 0) break;
            pads[best] += step;
            now = least;
            moved = true;
          }
          return now;
        }
        function ownTicks() {
          return [0, 1, 2].map(function(a) {
            return ticksOn[a] ? kiwiPixelDiff(shoot(a, NONE), bare) : null;
          });
        }
        // The pairs of axes whose tick labels clash, and by how much
        function tickPairs(own) {
          var pairs = [];
          for (var i = 0; i < 3; i++) {
            if (!own[i]) continue;
            var near = null;
            for (var k = i + 1; k < 3; k++) {
              if (!own[k]) continue;
              near = near || kiwiDilate(own[i], bare.w, bare.h, gap);
              var n = overlap(near, own[k]);
              if (n > 0) pairs.push({ a: i, b: k, n: n });
            }
          }
          return pairs;
        }
        // The tickvals that leave out the labels of one axis clashing with
        // another's - of the pair, the axis with more labels
        function dropClashing(own, pair) {
          var d = opts.ticks[pair.a].length > opts.ticks[pair.b].length ? pair.a : pair.b;
          var other = kiwiDilate(own[d === pair.a ? pair.b : pair.a], bare.w, bare.h, gap);
          // The scene works its ticks out from the axis afresh on every
          // redraw, so each label is drawn alone by giving the axis that one
          // tick for the moment
          var axis = div._fullLayout[key][['xaxis', 'yaxis', 'zaxis'][d]];
          var saved = { tickmode: axis.tickmode, tickvals: axis.tickvals, ticktext: axis.ticktext };
          var ticks = opts.ticks[d].slice(), keep = [];
          try {
            ticks.forEach(function(tick) {
              var value = tick.x / scene.dataScale[d];
              axis.tickmode = 'array';
              axis.tickvals = [value];
              axis.ticktext = [tick.text];
              if (overlap(kiwiPixelDiff(shoot(d, NONE), bare), other) === 0)
                keep.push(value);
            });
          } finally {
            axis.tickmode = saved.tickmode;
            axis.tickvals = saved.tickvals;
            axis.ticktext = saved.ticktext;
          }
          if (keep.length === ticks.length || keep.length < 2) return null;
          var update = {};
          update[key + '.' + ['xaxis', 'yaxis', 'zaxis'][d] + '.tickvals'] = keep;
          return update;
        }
        try {
          bare = shoot(NONE, NONE);
          if (!bare) return Promise.resolve(false);
          var size = Math.max.apply(null, opts.tickSize.concat(opts.labelSize));
          gap = Math.max(3, Math.round(0.3 * size));
          step = Math.max(2, Math.round(0.25 * size));
          var left = relieve(opts.tickPad, function() {
            return tickPairs(ownTicks()).reduce(function(n, p) { return n + p.n; }, 0);
          });
          if (left > 0 && depth < 3) {
            var own = ownTicks(), pairs = tickPairs(own);
            if (pairs.length) drop = dropClashing(own, pairs[0]);
          }
          var ticks = kiwiDilate(kiwiPixelDiff(shoot(ALL, NONE), bare), bare.w, bare.h, gap);
          relieve(opts.labelPad, function() {
            var n = 0;
            for (var i = 0; i < 3; i++) {
              if (!titlesOn[i] || !opts.labels[i]) continue;
              n += overlap(kiwiPixelDiff(shoot(NONE, i), bare), ticks);
            }
            return n;
          });
        } finally {
          for (var j = 0; j < 3; j++) {
            opts.tickColor[j] = tickColors[j];
            opts.labelEnable[j] = titlesOn[j];
          }
          scene.glplot.axes.update(opts);
          if (scene.glplot.redraw) scene.glplot.redraw();
        }
        if (!drop) return Promise.resolve(moved);
        return Plotly.relayout(div, drop).then(function() {
          return kiwiWait(300);
        }).then(function() {
          return kiwiSeparateSceneLabels(div, key, depth + 1);
        }).then(function() { return true; });
      }

      // Points the scene's camera from `distance` away along the direction it
      // already looks in, at a target panned by dx/dy pixels on screen.
      // Camera and target move together, so the view direction - and a
      // rotation set on screen - is kept.
      function kiwiMoveCamera(div, key, distance, dx, dy, frameHeight) {
        var cam = div._fullLayout[key]._scene.getCamera();
        var c = cam.center, e = cam.eye, u = cam.up;
        function cross(a, b) {
          return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
                  a[0] * b[1] - a[1] * b[0]];
        }
        function unit(a) {
          var n = Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]) || 1;
          return [a[0] / n, a[1] / n, a[2] / n];
        }
        var toward = [c.x - e.x, c.y - e.y, c.z - e.z];
        var dist = Math.sqrt(toward[0] * toward[0] + toward[1] * toward[1] +
                             toward[2] * toward[2]);
        var look = unit(toward);
        var right = unit(cross(look, [u.x, u.y, u.z]));
        var upScreen = unit(cross(right, look));
        // Scene units per pixel at the target's depth, for the 45 degree
        // vertical field of view plotly's 3D camera uses
        var perPx = 2 * dist * Math.tan(Math.PI / 8) / frameHeight;
        var nc = [0, 1, 2].map(function(i) {
          return [c.x, c.y, c.z][i] + (dx * right[i] - dy * upScreen[i]) * perPx;
        });
        var update = {};
        update[key + '.camera'] = {
          up: u, projection: cam.projection,
          center: { x: nc[0], y: nc[1], z: nc[2] },
          eye: {
            x: nc[0] - look[0] * distance,
            y: nc[1] - look[1] * distance,
            z: nc[2] - look[2] * distance
          }
        };
        return Plotly.relayout(div, update).then(function() { return kiwiWait(300); });
      }

      function kiwiCameraDistance(div, key) {
        var cam = div._fullLayout[key]._scene.getCamera();
        var c = cam.center, e = cam.eye;
        return Math.sqrt(Math.pow(c.x - e.x, 2) + Math.pow(c.y - e.y, 2) +
                         Math.pow(c.z - e.z, 2));
      }

      // Centres the drawn scene in its frame and sets the camera's distance
      // so that it fills 96% of the frame on its tightest side. Under
      // perspective neither the pan nor the zoom is exact, so each step is
      // measured again. Anything reaching the edge of the frame may be cut
      // off there, so the distance is bracketed between the closest one known
      // to fit and the farthest one known to reach the edge: a step never
      // goes back past either, which is what keeps the search from swinging
      // between the two, and it always ends on a distance that fits.
      function kiwiZoomToFit(div, key, round, bracket) {
        bracket = bracket || { fit: Infinity, clip: 0 };
        var box = kiwiSceneContentBox(div, key);
        if (!box) return Promise.resolve(null);
        var f = box.frame;
        var fw = f.right - f.left, fh = f.bottom - f.top;
        var d = kiwiCameraDistance(div, key);
        var touches = box.left <= f.left + 1 || box.top <= f.top + 1 ||
                      box.right >= f.right - 1 || box.bottom >= f.bottom - 1;
        var dx = 0, dy = 0, target;
        if (touches) {
          bracket.clip = Math.max(bracket.clip, d);
          target = isFinite(bracket.fit) ? (d + bracket.fit) / 2 : d / 0.7;
        } else {
          bracket.fit = Math.min(bracket.fit, d);
          // How far the drawing's centre sits from the frame's, in pixels
          dx = (box.left + box.right - f.left - f.right) / 2;
          dy = (box.top + box.bottom - f.top - f.bottom) / 2;
          var zoom = 0.96 * Math.min(fw / (box.right - box.left),
                                     fh / (box.bottom - box.top));
          target = Math.max(d / zoom, (d + bracket.clip) / 2);
          var settled = Math.abs(target / d - 1) < 0.02 &&
                        Math.abs(dx) < 0.01 * fw && Math.abs(dy) < 0.01 * fh;
          if (settled || round >= 10) return Promise.resolve(box);
        }
        if (round >= 10) {
          // Out of steps on a distance that reaches the edge: go back to the
          // closest one that fitted, or well clear of this one
          var safe = isFinite(bracket.fit) ? bracket.fit : d / 0.5;
          return kiwiMoveCamera(div, key, safe, 0, 0, fh).then(function() {
            return kiwiSceneContentBox(div, key);
          });
        }
        return kiwiMoveCamera(div, key, target, dx, dy, fh).then(function() {
          return kiwiZoomToFit(div, key, round + 1, bracket);
        });
      }

      // The cubic spectrum's export. The legend gets a gutter of its own at
      // the right of the canvas, the axis labels are moved clear of each
      // other, the camera is zoomed to fill what is left, and the legend is
      // then pulled in beside the drawn scene, top-aligned with it. Resolves
      // to the scene's content box (kiwiSceneContentBox).
      function kiwiFitCubic(div) {
        var key = (div._fullLayout._subplots.gl3d || [])[0];
        if (!key) return Promise.resolve(null);
        var hasLegend = div._fullLayout.showlegend !== false &&
                        !!div.querySelector('.infolayer .legend');
        var pad = kiwiLegendSafetyPad(div);
        // The legend, the gap before it, and room for kiwiFitLegendClip, on
        // top of any gutter the side tables hold already
        var gutter = hasLegend ? Math.ceil(kiwiLegendWidth(div) + 3 * pad) : 0;
        var start = gutter > 0 ?
          Plotly.relayout(div, {
            'margin.r': ((div._fullLayout.margin && div._fullLayout.margin.r) || 0) + gutter
          }).then(function() { return kiwiWait(300); }) :
          Promise.resolve();
        return start.then(function() {
          return kiwiSeparateSceneLabels(div, key);
        }).then(function() {
          return kiwiZoomToFit(div, key, 0);
        }).then(function(box) {
          // The projection of the axes shifts a little as the camera moves,
          // so the labels are checked again at the final view, and the fit
          // redone if any had to move.
          return kiwiSeparateSceneLabels(div, key).then(function(moved) {
            return moved ? kiwiZoomToFit(div, key, 0) : box;
          });
        }).then(function(box) {
          if (!box || !hasLegend) return box;
          var size = div._fullLayout._size;
          return Plotly.relayout(div, {
            'legend.x': (box.right + pad - size.l) / size.w,
            'legend.xanchor': 'left',
            'legend.y': 1 - (box.top - size.t) / size.h,
            'legend.yanchor': 'top'
          }).then(function() { return box; });
        });
      }

      // The side tables of the cubic spectrum are anchored at the right edge
      // of the plot area, in the gutter the export keeps for them. The fitted
      // cube rarely reaches that edge, so the tables are moved in to start
      // their own gap past the drawn scene, top-aligned with it where they
      // still fit the canvas that way.
      function kiwiPlaceSideTables(div, box) {
        var full = div._fullLayout;
        if (!box || !kiwiHasSideTables(full)) return Promise.resolve();
        var size = full._size;
        var x = Math.min(1, (box.right - size.l) / size.w);
        var extent = 0;
        (full.shapes || []).forEach(function(s) {
          if (kiwiIsSideTable(s) && s.ysizemode === 'pixel')
            extent = Math.max(extent, -Math.min(s.y0, s.y1));
        });
        var y = box.top + extent + 8 <= full.height ?
          Math.min(1, 1 - (box.top - size.t) / size.h) : 1;
        var update = {};
        (full.annotations || []).forEach(function(a, i) {
          if (!kiwiIsSideTable(a)) return;
          update['annotations[' + i + '].x'] = x;
          update['annotations[' + i + '].y'] = y;
        });
        (full.shapes || []).forEach(function(s, i) {
          if (!kiwiIsSideTable(s)) return;
          update['shapes[' + i + '].xanchor'] = x;
          update['shapes[' + i + '].yanchor'] = y;
        });
        return Plotly.relayout(div, update);
      }

      // The part of the canvas to keep: the drawn scene, the legend and the
      // side tables, with `pad` pixels all round, in the div's own pixels.
      function kiwiCubicCrop(div, box, pad) {
        var W = div._fullLayout.width, H = div._fullLayout.height;
        if (!box) return { x: 0, y: 0, w: W, h: H };
        var l = box.left, t = box.top, r = box.right, b = box.bottom;
        var page = div.getBoundingClientRect();
        function include(node) {
          var nb = node.getBoundingClientRect();
          if (!nb.width || !nb.height) return;
          l = Math.min(l, nb.left - page.left);
          t = Math.min(t, nb.top - page.top);
          r = Math.max(r, nb.right - page.left);
          b = Math.max(b, nb.bottom - page.top);
        }
        var legend = div._fullLayout.showlegend !== false &&
                     div.querySelector('.infolayer .legend .scrollbox');
        if (legend) include(legend);
        var annotations = div._fullLayout.annotations || [];
        Array.prototype.forEach.call(
          div.querySelectorAll('.infolayer g.annotation[data-index]'),
          function(node) {
            if (kiwiIsSideTable(annotations[+node.getAttribute('data-index')]))
              include(node);
          });
        var x = Math.max(0, Math.floor(l - pad));
        var y = Math.max(0, Math.floor(t - pad));
        return {
          x: x, y: y,
          w: Math.min(W, Math.ceil(r + pad)) - x,
          h: Math.min(H, Math.ceil(b + pad)) - y
        };
      }

      // Cuts the serialised figure down to `crop` by moving its viewBox. The
      // 3D scene sits in it as an image placed in absolute pixels, so it is
      // cut along with everything else.
      function kiwiCropSVG(svg, crop) {
        var doc = new DOMParser().parseFromString(svg, 'image/svg+xml');
        var root = doc.documentElement;
        if (!root || root.nodeName !== 'svg') return svg;
        root.setAttribute('viewBox', [crop.x, crop.y, crop.w, crop.h].join(' '));
        root.setAttribute('width', crop.w);
        root.setAttribute('height', crop.h);
        return new XMLSerializer().serializeToString(doc);
      }

      // Plotly.downloadImage does NOT serialise the plot it is handed: it
      // clones the figure into a fresh element, re-plots it from layout/data and
      // serialises the clone. Anything written into the rendered SVG - the
      // enlarged legend key symbols, the widened legend clip - is on the
      // original node and is simply thrown away, which is why those fixes kept
      // looking correct on screen and kept coming out wrong in the file.
      // Snapshot.toSVG serialises the node it is given, so this hands it the
      // node that was actually measured and adjusted. It bakes any WebGL canvas
      // into the SVG as a raster image on the way, so 3D scenes survive too.
      function kiwiSaveImage(div, format, width, height, scale, filename, crop) {
        var svg = Plotly.Snapshot.toSVG(div, format, 1);
        if (crop) svg = kiwiCropSVG(svg, crop);
        if (format === 'svg') {
          kiwiSaveBlob(new Blob([svg], { type: 'image/svg+xml' }), filename + '.svg');
          return Promise.resolve();
        }
        return Plotly.Snapshot.svgToImg({
          format: format, width: width, height: height, scale: scale,
          svg: svg, canvas: document.createElement('canvas'), promise: true
        }).then(function(url) {
          // An export at print resolution runs to tens of megabytes, past what
          // a browser will follow as a data: URL, so it goes out as a blob.
          var parts = url.split(',');
          var bytes = atob(parts[1]);
          var buf = new Uint8Array(bytes.length);
          for (var i = 0; i < bytes.length; i++) buf[i] = bytes.charCodeAt(i);
          kiwiSaveBlob(new Blob([buf], { type: 'image/' + format }), filename + '.' + format);
        });
      }

      function kiwiSaveBlob(blob, name) {
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = name;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(function() { URL.revokeObjectURL(url); }, 10000);
      }

      Shiny.addCustomMessageHandler('downloadPlot', function(msg) {
        var qualityMap = {
          low:    { width: 1280, height: 720,  scale: 1 },
          normal: { width: 1920, height: 1080, scale: 2 },
          high:   { width: 2560, height: 1440, scale: 4 }
        };
        var q = qualityMap[msg.quality] || qualityMap.normal;
        // Font sizes are absolute px on a logical canvas whose width is set by
        // the Quality setting, so an unadjusted size covers a bigger share of
        // the small canvas than of the large one - Quality was silently
        // changing how large the labels read. Normalise against the
        // normal-quality canvas so Label Size means the same thing at every
        // quality; normal is the reference, so it renders exactly as before.
        var qualityFontScale = q.width / 1920;
        // Every level doubled: the old ladder read far too small in an
        // exported figure, where the plot is viewed at print size rather than
        // in a card. The old top level (2.0) is now the second step down.
        var contextScale = ({ small: 1.5, normal: 2.0, large: 3.0, xlarge: 4.0 }[msg.context] || 2.0)
          * qualityFontScale;
        // Line widths keep one decimal rather than rounding to whole pixels:
        // the spectrum's dotted connectors (1) and its peak leader lines (1.5)
        // are deliberately different weights, and integer rounding collapses
        // them onto each other at some scales.
        function roundW(v) { return Math.round(v * 10) / 10; }
        var fig = JSON.parse(JSON.stringify(msg.json));
        // The image is drawn from the SVG in isolation, where none of the web
        // fonts the page loads are available - the app loads Open Sans, the
        // first family in plotly's default list - so the browser falls back
        // to the next one, Verdana, which runs some 16% wider. Everything was
        // measured in the page's font and drawn in the wider one: the end of
        // a long legend entry ran off the canvas however much room had been
        // left for it. Dropping the page's web fonts from every family list
        // makes the export measure in the font it will be drawn in.
        var webFonts = {};
        if (document.fonts && document.fonts.forEach)
          document.fonts.forEach(function(face) {
            webFonts[String(face.family).replace(/['\"]/g, '').trim().toLowerCase()] = true;
          });
        function localFamily(family) {
          var kept = String(family).split(',').filter(function(name) {
            return !webFonts[name.replace(/['\"]/g, '').trim().toLowerCase()];
          });
          return kept.length ? kept.join(',').trim() : 'verdana, arial, sans-serif';
        }
        fig.layout.font = fig.layout.font || {};
        fig.layout.font.family = fig.layout.font.family ||
          '\"Open Sans\", verdana, arial, sans-serif';
        (function localFonts(obj) {
          if (!obj || typeof obj !== 'object') return;
          if (typeof obj.family === 'string') obj.family = localFamily(obj.family);
          Object.values(obj).forEach(localFonts);
        })({ layout: fig.layout, data: fig.data });
        if (_kiwiPendingZoomRange) {
          Object.keys(_kiwiPendingZoomRange).forEach(function(axisKey) {
            if (!fig.layout[axisKey]) return;
            fig.layout[axisKey] = Object.assign({}, fig.layout[axisKey], {
              autorange: false,
              range: _kiwiPendingZoomRange[axisKey]
            });
          });
          _kiwiPendingZoomRange = null;
        }
        // On screen the cubic spectrum's legend overlays its scene, and the
        // figure nudges the camera sideways to clear it (camera.center.x,
        // spectrum_legend_camera_x in R). The export gives the legend a
        // gutter of its own, so that nudge only pushes the spectrum off
        // centre. Read it off the figure as built, before the on-screen
        // camera replaces it.
        var legendDodge = {};
        Object.keys(fig.layout).forEach(function(key) {
          if (!/^scene[0-9]*$/.test(key)) return;
          var cam = fig.layout[key] && fig.layout[key].camera;
          legendDodge[key] = (cam && cam.center && cam.center.x) || 0;
        });
        if (_kiwiPendingCamera) {
          Object.keys(_kiwiPendingCamera).forEach(function(sceneKey) {
            if (!fig.layout[sceneKey]) return;
            fig.layout[sceneKey].camera = _kiwiPendingCamera[sceneKey];
          });
          _kiwiPendingCamera = null;
        }
        // Every plot builder draws on a transparent background so the card
        // shows through in the app. An empty string means the export keeps
        // that; otherwise the transparency switch is off and this is the solid
        // colour that goes with the chosen theme. 3D scenes are covered too -
        // their axes set showbackground false, so the paper is what shows.
        if (msg.background) {
          fig.layout.paper_bgcolor = msg.background;
          fig.layout.plot_bgcolor = msg.background;
        }
        // Snapshot the 3D scene axis fonts before the generic walk below
        // scales every size in the layout, so the scene sizes can be set from
        // the figure's own values instead of being unwound back out of an
        // already-scaled number.
        var sceneAxes = [];
        Object.keys(fig.layout).forEach(function(key) {
          if (!/^scene[0-9]*$/.test(key)) return;
          ['xaxis', 'yaxis', 'zaxis'].forEach(function(ax) {
            var axis = fig.layout[key] && fig.layout[key][ax];
            if (!axis) return;
            sceneAxes.push({
              axis: axis,
              tick: axis.tickfont && axis.tickfont.size,
              title: (axis.titlefont && axis.titlefont.size) ||
                     (axis.title && axis.title.font && axis.title.font.size)
            });
          });
        });
        if (contextScale !== 1.0) {
          (function scaleFonts(obj) {
            if (!obj || typeof obj !== 'object') return;
            if (typeof obj.size === 'number') obj.size = Math.round(obj.size * contextScale);
            Object.values(obj).forEach(scaleFonts);
          })(fig.layout);
          fig.data.forEach(function(trace) {
            if (trace.marker && typeof trace.marker.size === 'number')
              trace.marker.size = Math.round(trace.marker.size * contextScale);
            // The single-sample spectrum never sets a line width, so it draws
            // at Plotly's scatter default of 2 and there was no number here to
            // scale - the trace stayed hairline-thin while every label around
            // it grew. Fill the default in so the scaling below applies to it.
            if (trace.line && typeof trace.line.width !== 'number' &&
                typeof trace.mode === 'string' && trace.mode.indexOf('lines') !== -1)
              trace.line.width = 2;
            if (trace.line && typeof trace.line.width === 'number')
              trace.line.width = roundW(trace.line.width * contextScale);
            // Marker outlines are nested one level deeper than marker.size, so
            // they were missed as well.
            if (trace.marker && trace.marker.line &&
                typeof trace.marker.line.width === 'number')
              trace.marker.line.width = roundW(trace.marker.line.width * contextScale);
            ['error_x', 'error_y'].forEach(function(key) {
              var err = trace[key];
              if (!err) return;
              if (typeof err.thickness === 'number')
                err.thickness = err.thickness * contextScale;
              if (typeof err.width === 'number')
                err.width = err.width * contextScale;
            });
            ['textfont', 'outsidetextfont', 'insidetextfont'].forEach(function(key) {
              if (trace[key] && typeof trace[key].size === 'number')
                trace[key].size = Math.round(trace[key].size * contextScale);
            });
            if (trace.legendgrouptitle && trace.legendgrouptitle.font &&
                typeof trace.legendgrouptitle.font.size === 'number')
              trace.legendgrouptitle.font.size = Math.round(trace.legendgrouptitle.font.size * contextScale);
            if (trace.colorbar) {
              if (trace.colorbar.title && trace.colorbar.title.font &&
                  typeof trace.colorbar.title.font.size === 'number')
                trace.colorbar.title.font.size = Math.round(trace.colorbar.title.font.size * contextScale);
              if (trace.colorbar.tickfont && typeof trace.colorbar.tickfont.size === 'number')
                trace.colorbar.tickfont.size = Math.round(trace.colorbar.tickfont.size * contextScale);
            }
          });
          if (fig.layout.margin && typeof fig.layout.margin === 'object') {
            ['l', 'r', 't', 'b', 'pad'].forEach(function(side) {
              if (typeof fig.layout.margin[side] === 'number')
                fig.layout.margin[side] = Math.round(fig.layout.margin[side] * contextScale);
            });
          }
          // Shapes carry a `width`, not a `size`, so the font walk above never
          // reached them: the single spectrum draws its mass-difference
          // connectors and its peak leader lines as layout shapes, and they
          // stayed hairline while their labels grew. arrowsize is deliberately
          // left alone - it is a multiplier on arrowwidth, so scaling both
          // would grow the arrowhead quadratically.
          (fig.layout.shapes || []).forEach(function(shape) {
            if (shape.line && typeof shape.line.width === 'number')
              shape.line.width = roundW(shape.line.width * contextScale);
            ['arrowwidth', 'standoff'].forEach(function(key) {
              if (typeof shape[key] === 'number')
                shape[key] = roundW(shape[key] * contextScale);
            });
            // A shape sized in pixels (the grid of the spectrum's side tables)
            // is placed like an annotation's xshift/yshift - a pixel offset
            // from its anchor - so it has to grow by the same factor. Left
            // alone, the table lines stayed at their on-screen size in the
            // top corner while the text they divide grew away from them.
            if (shape.xsizemode === 'pixel')
              ['x0', 'x1'].forEach(function(key) {
                if (typeof shape[key] === 'number')
                  shape[key] = Math.round(shape[key] * contextScale);
              });
            if (shape.ysizemode === 'pixel')
              ['y0', 'y1'].forEach(function(key) {
                if (typeof shape[key] === 'number')
                  shape[key] = Math.round(shape[key] * contextScale);
              });
          });
          // xshift/yshift are the pixel gaps that hold a label clear of what it
          // labels - the mass-difference text sitting above its connector, for
          // one - so they have to grow with the text or it lands back on it.
          (fig.layout.annotations || []).forEach(function(anno) {
            ['arrowwidth', 'standoff', 'xshift', 'yshift'].forEach(function(key) {
              if (typeof anno[key] === 'number')
                anno[key] = Math.round(anno[key] * contextScale);
            });
          });
        }
        var isCubicSpectra = fig.data.some(function(t) { return t.type === 'scatter3d'; });
        // 3D scene labels get their own, much shallower ladder rather than the
        // 2D contextScale above. Two things make the same pixel size read far
        // bigger inside a scene: Plotly puts a scene axis title at a fixed
        // offset that does NOT grow with its tick labels, so scaling the ticks
        // up walks them straight over the title; and the scene only gets the
        // slice of the canvas the legend leaves it, so it is drawn much
        // smaller than the figure it sits in. These factors are relative to
        // the figure's own sizes, not multiplied onto contextScale.
        var scene3DScale = ({ small: 1.0, normal: 1.25, large: 1.5, xlarge: 1.75 }[msg.context] || 1.25)
          * qualityFontScale;
        if (isCubicSpectra) {
          sceneAxes.forEach(function(rec) {
            var axis = rec.axis;
            var scale = scene3DScale;
            // The cubic spectrum's sample axis is a category axis carrying one
            // tick per sample, all packed along a single receding edge, so its
            // labels crowd long before the numeric mass and intensity ticks do.
            // multiple_spectra() only turns these labels on by itself up to 8
            // samples — past that they already fill the axis at their
            // on-screen size, so hold them there rather than letting the
            // label-size setting push them into one another.
            var ticks = Array.isArray(axis.tickvals) ? axis.tickvals.length : 0;
            if (axis.type === 'category' && ticks > 0) {
              // Quality-normalised like scene3DScale: the ceiling is a share
              // of the axis, not a pixel count, so it moves with the canvas.
              scale = Math.min(scale, Math.max(1.0, 8 / ticks) * qualityFontScale);
            }
            if (typeof rec.tick === 'number')
              axis.tickfont.size = Math.max(1, Math.round(rec.tick * scale));
            // Keep the title on the same factor as its ticks so the gap the
            // on-screen figure already reads cleanly at is preserved.
            if (typeof rec.title === 'number') {
              var titleSize = Math.max(1, Math.round(rec.title * scale));
              if (axis.titlefont) axis.titlefont.size = titleSize;
              if (axis.title && axis.title.font) axis.title.font.size = titleSize;
            }
            // Thin the numeric axes out as well: the mass axis defaults to
            // enough ticks that 21.2K / 21.4K / 21.6K … already run together
            // along the receding edge before any scaling is applied.
            if (axis.type !== 'category' && typeof axis.nticks !== 'number')
              axis.nticks = 5;
          });
          // How much of the scene's box the projected cube fills depends on
          // the camera, so no fixed margin fits: Plotly's defaults left the
          // spectrum floating in empty space, or its edge labels clipped
          // mid-glyph once they were scaled up. kiwiFitCubic zooms the
          // camera to the scene's box and the image is cropped to what was
          // drawn, so the scene gets the whole canvas here - including the
          // margins plotly's R package fills in by default. Nor may plotly
          // grow them itself: moving a legend taller than the canvas made it
          // push the bottom margin out, which shrank the scene after it had
          // been fitted.
          // The exception is the side tables' gutter (add_side_tables() in
          // R, 'Show All' with metrics or parameters on): they live in
          // margin.r, already scaled with their text above, and the canvas
          // grows by it below. The scene then has the plot area to itself,
          // without the on-screen domain that kept it clear of the tables.
          // The tables were scaled with the 2D labels above, which drew them
          // large beside the scene's much smaller axis labels. Bring them,
          // their grid and their gutter (which holds nothing else here) to
          // the scene's factor instead.
          if (kiwiHasSideTables(fig.layout) && contextScale > 0) {
            var tableScale = scene3DScale / contextScale;
            (fig.layout.annotations || []).forEach(function(anno) {
              if (!kiwiIsSideTable(anno)) return;
              if (anno.font && typeof anno.font.size === 'number')
                anno.font.size = Math.max(1, Math.round(anno.font.size * tableScale));
              ['xshift', 'yshift'].forEach(function(key) {
                if (typeof anno[key] === 'number')
                  anno[key] = Math.round(anno[key] * tableScale);
              });
            });
            (fig.layout.shapes || []).forEach(function(shape) {
              if (!kiwiIsSideTable(shape)) return;
              if (shape.line && typeof shape.line.width === 'number')
                shape.line.width = roundW(shape.line.width * tableScale);
              ['x0', 'x1', 'y0', 'y1'].forEach(function(key) {
                if (typeof shape[key] === 'number')
                  shape[key] = Math.round(shape[key] * tableScale);
              });
            });
            if (fig.layout.margin && typeof fig.layout.margin.r === 'number')
              fig.layout.margin.r = Math.round(fig.layout.margin.r * tableScale);
          }
          var tableGutter = kiwiHasSideTables(fig.layout) ?
            ((fig.layout.margin && fig.layout.margin.r) || 0) : 0;
          fig.layout.margin = { l: 0, r: tableGutter, t: 0, b: 0, pad: 0, autoexpand: false };
          if (tableGutter > 0) {
            Object.keys(fig.layout).forEach(function(key) {
              if (/^scene[0-9]*$/.test(key) && fig.layout[key])
                fig.layout[key].domain = { x: [0, 1], y: [0, 1] };
            });
          }
          if (fig.layout.showlegend !== false) {
            Object.keys(legendDodge).forEach(function(key) {
              var cam = fig.layout[key].camera = fig.layout[key].camera || {};
              cam.center = Object.assign({ x: 0, y: 0, z: 0 }, cam.center);
              cam.center.x -= legendDodge[key];
            });
          }
          // The scene reaches the file as a raster of its WebGL canvas, which
          // is drawn at the screen's pixel ratio - upscaled, and soft, in an
          // export at twice that. Draw it at the export's own scale instead,
          // capped where the canvas grows past what WebGL reliably allocates.
          fig.config = Object.assign({}, fig.config, {
            plotGlPixelRatio: Math.max(window.devicePixelRatio || 1, Math.min(q.scale, 2))
          });
        }

        // Plotly hard-clamps a legend key symbol to 16px whatever the trace's
        // marker size is - legend/style.js does
        //   y('marker.size', mean, [2, 16], 12)
        // and that helper clamps to the range on both branches of itemsizing,
        // so there is no figure-level setting that makes the symbols follow
        // the label size. They get rescaled in the rendered SVG instead (see
        // kiwiScaleLegendSymbols, and kiwiSaveImage for why that rescaling now
        // survives into the file). What has to happen here, before the legend
        // is laid out, is reserving the wider horizontal slot they will need.
        var legendSymbolScale = 1;
        if (fig.layout.showlegend !== false) {
          var legend = fig.layout.legend = fig.layout.legend || {};
          var legendFont = (legend.font && legend.font.size) ||
                           (fig.layout.font && fig.layout.font.size) || 12;
          var LEGEND_MARKER_MAX = 16;
          // On screen a key symbol and its label are the same size; hold that
          // relationship at every label size.
          legendSymbolScale = Math.max(1, legendFont / LEGEND_MARKER_MAX);
          // 2.2x rather than a snug fit: the symbol is drawn centred in this
          // slot, so the slack is what keeps an enlarged key clear of the text
          // beside it.
          if (typeof legend.itemwidth !== 'number')
            legend.itemwidth = Math.max(
              30, Math.round(LEGEND_MARKER_MAX * legendSymbolScale * 2.2));
        }
        // The spectrum's side tables live in margin.r, which grew with the
        // labels above - at the larger label sizes to well over half the
        // canvas, leaving the spectrum a sliver. Append that gutter to the
        // canvas instead, the way kiwiFitLegend does for a legend, so the plot
        // keeps the size it has without tables.
        var exportWidth = q.width, exportHeight = q.height;
        if (kiwiHasSideTables(fig.layout))
          exportWidth += (fig.layout.margin && fig.layout.margin.r) || 0;
        document.body.style.cursor = 'progress';
        var div = document.createElement('div');
        div.style.width  = exportWidth + 'px';
        div.style.height = q.height + 'px';
        if (isCubicSpectra) {
          // WebGL (scatter3d) requires the element to be in the visible viewport —
          // browsers skip WebGL rendering for off-screen elements, producing a blank canvas.
          // Position in-viewport but invisible to the user.
          div.style.position = 'fixed';
          div.style.top = '0';
          div.style.left = '0';
          div.style.zIndex = '99999';
          div.style.opacity = '0.001';
          div.style.pointerEvents = 'none';
        } else {
          div.className = 'plotly-dl-offscreen';
        }
        document.body.appendChild(div);
        var outScale = msg.format === 'svg' ? 1 : q.scale;
        Plotly.newPlot(div, fig.data, fig.layout, fig.config || {}).then(function() {
          // For WebGL/3D, wait for the GPU to finish rendering before screenshotting.
          return new Promise(function(resolve) { setTimeout(resolve, isCubicSpectra ? 800 : 0); });
        }).then(function() {
          if (isCubicSpectra) {
            var sceneBox;
            // Tables taller than the canvas grow it first, so the scene is
            // fitted to the final height
            return kiwiFitSideTables(div, q.height).then(function() {
              return kiwiFitCubic(div);
            }).then(function(box) {
              sceneBox = box;
              return kiwiPlaceSideTables(div, box);
            }).then(function() {
              kiwiScaleLegendSymbols(div, legendSymbolScale);
              kiwiFitLegendClip(div);
              // A margin of 3% of the canvas height all round what was drawn
              var crop = kiwiCubicCrop(div, sceneBox, Math.round(0.03 * q.height));
              return kiwiSaveImage(
                div, msg.format, crop.w, crop.h, outScale, msg.filename, crop);
            });
          }
          return kiwiFitPaper(div).then(function() {
            return kiwiFitSideTables(div, q.height);
          }).then(function(finalHeight) {
            exportHeight = finalHeight;
            return kiwiFitLegend(div, exportWidth);
          }).then(function(finalWidth) {
            kiwiScaleLegendSymbols(div, legendSymbolScale);
            // After the symbols, so the clip is fitted to the final contents.
            kiwiFitLegendClip(div);
            return kiwiSaveImage(
              div, msg.format, finalWidth, exportHeight, outScale, msg.filename);
          });
        }).catch(function(err) {
          // Without this a failed export leaves the busy cursor up for good and
          // the offscreen figure attached to the page, with nothing said.
          console.error('KiwiMS plot export failed:', err);
        }).then(function() {
          if (div.parentNode) document.body.removeChild(div);
          document.body.style.cursor = '';
        });
      });

      // Reads the plot's current on-screen view out of the live Plotly node so
      // the export can reproduce it.
      //
      // Timing matters here. bslib::popover() keeps its content in a
      // <template> and Bootstrap instantiates it into a popover attached to
      // document.body, so by the time an export button is clicked that button
      // is no longer inside the card and closest('.card') finds nothing. (Same
      // reason applyExportState() has to re-run on shown.bs.popover and search
      // for its buttons by id across the whole document.) The trigger IS still
      // in the card while the popover is opening, so that is when the view
      // gets captured; the click is only a fallback for a button reached some
      // other way.
      function kiwiCaptureView(fromEl) {
        var card = fromEl && fromEl.closest && fromEl.closest('.card');
        var gd = card && card.querySelector('.js-plotly-plot');
        if (!gd || !gd._fullLayout) return false;

        // 2D: a box zoom or pan leaves the axis on a fixed range.
        var ranges = {};
        Object.keys(gd._fullLayout).forEach(function(key) {
          if (!/^[xy]axis[0-9]*$/.test(key)) return;
          var axis = gd._fullLayout[key];
          if (axis && axis.autorange === false && Array.isArray(axis.range)) {
            ranges[key] = axis.range.slice();
          }
        });

        // 3D (the cubic spectrum): dragging rotates and scroll-zooms the scene
        // camera - the axis ranges never move, so the 2D branch above sees
        // nothing to carry over. getCamera() is the live value; _fullLayout
        // .camera only holds what the plot was last laid out with.
        var cameras = {};
        Object.keys(gd._fullLayout).forEach(function(key) {
          if (!/^scene[0-9]*$/.test(key)) return;
          var sc = gd._fullLayout[key];
          if (!sc) return;
          var cam = (sc._scene && sc._scene.getCamera && sc._scene.getCamera()) ||
                    (gd.layout && gd.layout[key] && gd.layout[key].camera) ||
                    sc.camera;
          if (cam) cameras[key] = JSON.parse(JSON.stringify(cam));
        });

        _kiwiPendingZoomRange = Object.keys(ranges).length ? ranges : null;
        _kiwiPendingCamera = Object.keys(cameras).length ? cameras : null;
        return true;
      }

      // Opening any popover on a plot card re-reads that card's view, which
      // also drops whatever a previous card left behind.
      $(document).on('shown.bs.popover', function(e) {
        if (e && e.target) kiwiCaptureView(e.target);
      });

      // Set loading cursor immediately on plot export button click.
      $(document).on('click', '.plot-dl-buttons button, .plot-dl-buttons a', function() {
        document.body.style.cursor = 'progress';
        // Only overwrites when the button can still see its card; otherwise the
        // capture taken as the popover opened is the one that stands.
        kiwiCaptureView(this);
      });
      $(window).on('focus', function() {
        if (document.body.style.cursor === 'progress')
          setTimeout(function() { document.body.style.cursor = ''; }, 300);
      });
    "
    )),
    shiny$tags$script(shiny$HTML(
      "
      var _exportStates = {};
      Shiny.addCustomMessageHandler('setExportState', function(msg) {
        _exportStates[msg.prefix] = msg.enabled;
        applyExportState(msg.prefix);
      });
      function applyExportState(prefix) {
        var btns = $('[id$=\"_' + prefix + '_html\"],[id$=\"_' + prefix + '_png\"],[id$=\"_' + prefix + '_svg\"]');
        if (_exportStates[prefix] === false) {
          btns.prop('disabled', true).addClass('disabled').css('pointer-events', 'none').css('opacity', '0.5');
        } else {
          btns.prop('disabled', false).removeClass('disabled').css('pointer-events', '').css('opacity', '');
        }
      }
      $(document).on('shown.bs.popover', function() {
        Object.keys(_exportStates).forEach(applyExportState);
      });
    "
    )),
    # Hover hints written into HTML strings (table cells, value cards) carry
    # a title attribute. Marked with data-bs-toggle="tooltip" they get the same
    # Bootstrap tooltip as every bslib::tooltip() instead of the browser's own.
    # Delegated, so content rendered later is covered too.
    shiny$tags$script(shiny$HTML(
      "
      document.addEventListener('DOMContentLoaded', function() {
        new bootstrap.Tooltip(document.body, {
          selector: '[data-bs-toggle=\"tooltip\"]',
          container: 'body'
        });
      });
    "
    )),
    useWaiter(),
    waiterShowOnLoad(
      html = shiny$tags$div(
        style = "text-align: center;",
        shiny$tags$img(
          src = "static/logo_animated.svg",
          width = "400px",
          height = "400px"
        ),
        shiny$tags$div(
          style = paste0(
            "font-family: monospace; font-size: 50px; color: blac",
            "k; opacity: 0; animation: fadeIn 1s ease-in forwards",
            "; animation-delay: 1s;"
          ),
          "KiwiMS"
        )
      )
    ),
    useShinyjs(),
    bslib$page_navbar(
      id = ns("tabs"),
      title = shiny$tags$div(
        shiny$tags$img(
          src = "static/logo.svg",
          height = "42rem",
          style = "margin-right: 5px; margin-top: -2px"
        ),
        shiny$tags$span(
          "KiwiMS",
          style = "font-size: 21px; font-family: monospace;"
        )
      ),
      window_title = paste("KiwiMS", get_kiwims_version()["version"]),
      navbar_options = bslib$navbar_options(underline = TRUE),
      bslib$nav_panel(
        title = "Deconvolution",
        bslib$page_sidebar(
          sidebar = deconvolution_sidebar$ui(
            ns("deconvolution_pars")
          ),
          deconvolution_main$ui(
            ns("deconvolution_main")
          )
        )
      ),
      bslib$nav_panel(
        title = "Protein Conversion",
        bslib$page_sidebar(
          sidebar = conversion_sidebar$ui(ns("conversion_sidebar")),
          conversion_main$ui(ns("conversion_main"))
        )
      ),
      bslib$nav_panel(
        title = "Logs",
        icon = shiny::icon("list-check"),
        bslib$page_sidebar(
          sidebar = log_sidebar$ui(ns("log_sidebar")),
          bslib$card(
            class = "logs-card",
            log_view$ui(ns("logs"))
          )
        )
      ),
      bslib$nav_spacer(),
      bslib$nav_item(
        shiny::actionButton(
          ns("settings"),
          "Settings",
          icon = shiny::icon("gear"),
          class = "nav-link"
        )
      ),
      bslib$nav_item(
        shiny::actionButton(
          ns("licence"),
          "License",
          icon = shiny::icon("info"),
          class = "nav-link"
        )
      ),
      bslib$nav_item(shiny::uiOutput(
        ns("update_button"),
        class = "nav-link",
        style = "cursor: pointer;",
        onclick = "Shiny.setInputValue('app-open_update_modal', Math.random());"
      )),
      bslib$nav_item(
        shiny::tags$a(
          id = "unidec-tag",
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('app-unidec_click', Math.random());",
          shiny::tags$img(
            src = "static/UniDec.png",
            width = "auto",
            style = "    top: -1px;
    position: relative;",
            height = "18px"
          ),
          "UniDec"
        )
      )
      # bslib$nav_menu(
      #   title = "Links",
      #   align = "right",
      #   icon = shiny$icon("link"),
      #   bslib$nav_item(
      #     shiny$tags$a(
      #       shiny$tags$span(
      #         shiny$tags$i(class = "fa-brands fa-github me-1"),
      #         "KiwiMS GitHub"
      #       ),
      #       href = "https://github.com/infinity-a11y/MSFlow",
      #       target = "_blank",
      #       class = "nav-link"
      #     )
      #   ),
      #   bslib$nav_item(
      #     shiny$tags$a(
      #       shiny$tags$span(
      #         shiny$tags$i(class = "fa-brands fa-github me-1"),
      #         "UniDec GitHub"
      #       ),
      #       href = "https://github.com/michaelmarty/UniDec",
      #       target = "_blank",
      #       class = "nav-link"
      #     )
      #   ),
      #   bslib$nav_item(
      #     shiny$tags$a(
      #       shiny$tags$span(
      #         shiny$tags$img(
      #           src = "static/liora_logo.png",
      #           style = "height: 1em; margin-right: 5px;"
      #         ),
      #         "Liora Bioinformatics"
      #       ),
      #       href = "https://www.liora-bioinformatics.com",
      #       target = "_blank",
      #       class = "nav-link"
      #     )
      #   )
      # )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE = "1")
    ns <- session$ns

    # Kill server on session end
    session$onSessionEnded(function() {
      write_log("Session closed")
      shiny$stopApp()
    })

    shiny::observeEvent(input$quit_kiwims, {
      shiny::stopApp() # This signals the mother process that the app is done
    })

    # Initiate logging
    start_logging()
    write_log("Session started")

    # Log view server
    active_tab_reactive <- shiny$reactive({
      input$tabs
    })
    log_buttons <- log_sidebar$server("log_sidebar")
    log_view$server("logs", active_tab_reactive, log_buttons)

    # Gear button in log sidebar opens Settings modal scrolled to Logs section
    shiny$observeEvent(
      log_buttons$open_settings(),
      {
        open_settings_modal(open_section = "logs")
      },
      ignoreInit = TRUE
    )

    reset_button <- shiny$reactiveVal(0)
    configfile <- shiny$reactiveVal(NULL)
    pending_config <- shiny$reactiveVal(NULL)
    config_modal_state <- shiny$reactiveVal("upload")
    config_filename <- shiny$reactiveVal(NULL)
    # Modal opened from the Protein Conversion module — offers applying the
    # config to the samples table on top of activating it
    config_modal_from_conversion <- shiny$reactiveVal(FALSE)
    config_apply_trigger <- shiny$reactiveVal(0L)

    # User settings persistence
    settings_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS", "settings")
    dest_settings_file <- file.path(settings_dir, "default_dest_path.rds")
    dest_settings <- shiny$reactiveVal(
      if (file.exists(dest_settings_file)) {
        readRDS(dest_settings_file)
      } else {
        list(path = "", enabled = FALSE)
      }
    )

    # Reusable function to open the settings modal
    # initial_path: pre-fill dest folder from caller (e.g. currently active path)
    # A settings label with an on-hover explanation, styled like every other
    # hover tooltip in the app instead of a bespoke CSS-only popup
    settings_label_tooltip <- function(label, text) {
      bslib$tooltip(
        shiny$span(class = "settings-label-tooltip", label),
        text,
        placement = "top"
      )
    }

    open_settings_modal <- function(initial_path = NULL, open_section = NULL) {
      s <- dest_settings()
      base <- if (length(initial_path) == 1L && nzchar(initial_path)) {
        initial_path
      } else {
        s$path
      }
      us <- read_user_settings()

      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "Settings",
            size = "l",
            easyClose = TRUE,
            shiny$div(
              class = "settings-modal-body",

              # --- General ---
              shiny$div(
                class = "settings-collapse-header",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns("settings_general_body")),
                "General",
                shiny$icon("chevron-down", class = "settings-collapse-icon")
              ),
              shiny$div(
                id = ns("settings_general_body"),
                class = if (identical(open_section, "general")) {
                  "collapse show"
                } else {
                  "collapse"
                },
                shiny$tags$table(
                  class = "table table-sm table-bordered settings-table",
                  shiny$tags$tbody(
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Input Path"
                      ),
                      shiny$tags$td(
                        shiny$textInput(
                          ns("settings_input_path"),
                          label = NULL,
                          value = us$deconv_input_dir,
                          placeholder = "Absolute folder path",
                          width = "100%"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_input_path_display"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Output Path"
                      ),
                      shiny$tags$td(
                        shiny$textInput(
                          ns("settings_dest_path"),
                          label = NULL,
                          value = base,
                          placeholder = "Absolute folder path",
                          width = "100%"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_dest_path_display"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        settings_label_tooltip(
                          "Keep UniDec output files",
                          "Keeps the UniDec working files of every sample (<sample>_rawdata.txt and the <sample>_rawdata_unidecfiles folder) in the output folder. Off, the results are kept in the analysis database only."
                        )
                      ),
                      shiny$tags$td(
                        shiny$checkboxInput(
                          ns("settings_keep_raw_output"),
                          label = "Enable",
                          value = isTRUE(us$deconv_keep_raw_output)
                        )
                      ),
                      shiny$tags$td(class = "settings-table-feedback")
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(class = "settings-table-label"),
                      shiny$tags$td(
                        colspan = "2",
                        shiny$actionButton(
                          ns("reset_general"),
                          "Reset Section",
                          icon = shiny$icon("rotate-left"),
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              ),

              # --- Deconvolution Input Values ---
              shiny$div(
                class = "settings-collapse-header",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns("settings_deconv_body")),
                "Deconvolution Input Values",
                shiny$icon("chevron-down", class = "settings-collapse-icon")
              ),
              shiny$div(
                id = ns("settings_deconv_body"),
                class = if (identical(open_section, "deconv")) {
                  "collapse show"
                } else {
                  "collapse"
                },
                shiny$tags$table(
                  class = "table table-sm table-bordered settings-table",
                  shiny$tags$tbody(
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Min. charge state [z]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_startz"),
                          label = NULL,
                          min = 1,
                          max = 200,
                          value = us$deconv_startz,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_startz_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Max. charge state [z]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_endz"),
                          label = NULL,
                          min = 1,
                          max = 200,
                          value = us$deconv_endz,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_endz_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Lower deconvolution range [m/z]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_minmz"),
                          label = NULL,
                          min = 1,
                          max = 100000,
                          value = us$deconv_minmz,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_minmz_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Upper deconvolution range [m/z]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_maxmz"),
                          label = NULL,
                          min = 1,
                          max = 100000,
                          value = us$deconv_maxmz,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_maxmz_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Lower mass range [Da]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_masslb"),
                          label = NULL,
                          min = 1,
                          max = 2000000,
                          value = us$deconv_masslb,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_masslb_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Upper mass range [Da]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_massub"),
                          label = NULL,
                          min = 1,
                          max = 2000000,
                          value = us$deconv_massub,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_massub_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        settings_label_tooltip(
                          "Elution start time [min]",
                          "Default start of the retention time window that is summed for deconvolution. Leave blank to read from the first scan."
                        )
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_time_start"),
                          label = NULL,
                          min = 0,
                          max = 100,
                          value = us$deconv_time_start,
                          step = 0.05,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_time_start_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        settings_label_tooltip(
                          "Elution end time [min]",
                          "Default end of the retention time window that is summed for deconvolution. Leave blank to read to the last scan."
                        )
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_time_end"),
                          label = NULL,
                          min = 0,
                          max = 100,
                          value = us$deconv_time_end,
                          step = 0.05,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_time_end_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Detection window"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_peakwindow"),
                          label = NULL,
                          min = 1,
                          max = 500,
                          value = us$deconv_peakwindow,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_peakwindow_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Peak normalization"
                      ),
                      shiny$tags$td(
                        shiny$div(
                          class = "settings-peaknorm",
                          shiny$selectInput(
                            ns("settings_peaknorm"),
                            label = NULL,
                            choices = c(
                              "No normalization" = 0,
                              "Max Normalization" = 1,
                              "Normalization to Sum" = 2
                            ),
                            selected = us$deconv_peaknorm,
                            width = "200px"
                          )
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_peaknorm_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Peak threshold"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_peakthresh"),
                          label = NULL,
                          min = 0,
                          max = 1,
                          value = us$deconv_peakthresh,
                          step = 0.01,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_peakthresh_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(class = "settings-table-label"),
                      shiny$tags$td(
                        colspan = "2",
                        shiny$actionButton(
                          ns("reset_deconv"),
                          "Reset Section",
                          icon = shiny$icon("rotate-left"),
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              ),

              # --- Protein Conversion Input Values ---
              shiny$div(
                class = "settings-collapse-header",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns("settings_conv_body")),
                "Protein Conversion Input Values",
                shiny$icon("chevron-down", class = "settings-collapse-icon")
              ),
              shiny$div(
                id = ns("settings_conv_body"),
                class = if (identical(open_section, "conv")) {
                  "collapse show"
                } else {
                  "collapse"
                },
                shiny$tags$table(
                  class = "table table-sm table-bordered settings-table",
                  shiny$tags$tbody(
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Peak Tolerance [Da]"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_peak_tol"),
                          label = NULL,
                          value = us$peak_tolerance,
                          min = 0,
                          max = 20,
                          step = 0.1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_peak_tol_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Max. Stoichiometry"
                      ),
                      shiny$tags$td(
                        shiny$numericInput(
                          ns("settings_max_mult"),
                          label = NULL,
                          value = us$max_multiples,
                          min = 1,
                          max = 20,
                          step = 1,
                          width = "200px"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_max_mult_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        "Preferred Assignment"
                      ),
                      shiny$tags$td(
                        shiny$div(
                          class = "settings-select",
                          shiny$selectInput(
                            ns("settings_hit_pref"),
                            label = NULL,
                            choices = hit_preference_choices(),
                            selected = hit_preference_rule(us$hit_preference),
                            width = "200px"
                          )
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_hit_pref_feedback"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(class = "settings-table-label"),
                      shiny$tags$td(
                        colspan = "2",
                        shiny$actionButton(
                          ns("reset_conv"),
                          "Reset Section",
                          icon = shiny$icon("rotate-left"),
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              ),

              # --- Logs ---
              shiny$div(
                class = "settings-collapse-header settings-logs-section",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns("settings_logs_body")),
                "Logs",
                shiny$icon("chevron-down", class = "settings-collapse-icon")
              ),
              shiny$div(
                id = ns("settings_logs_body"),
                class = if (identical(open_section, "logs")) {
                  "collapse show"
                } else {
                  "collapse"
                },
                shiny$tags$table(
                  class = "table table-sm table-bordered settings-table",
                  shiny$tags$tbody(
                    shiny$tags$tr(
                      shiny$tags$td(
                        class = "settings-table-label",
                        settings_label_tooltip(
                          "Log Directory",
                          "Parent folder for daily log sub-folders. Takes effect on next app start."
                        )
                      ),
                      shiny$tags$td(
                        shiny$textInput(
                          ns("settings_log_dir"),
                          label = NULL,
                          value = if (nzchar(us$log_dir)) us$log_dir else "",
                          placeholder = paste0(
                            Sys.getenv("USERPROFILE"),
                            "\\Documents\\KiwiMS\\logs  (default)"
                          ),
                          width = "100%"
                        )
                      ),
                      shiny$tags$td(
                        class = "settings-table-feedback",
                        shiny$uiOutput(ns("settings_log_dir_display"))
                      )
                    ),
                    shiny$tags$tr(
                      shiny$tags$td(class = "settings-table-label"),
                      shiny$tags$td(
                        colspan = "2",
                        shiny$actionButton(
                          ns("reset_logs"),
                          "Reset Section",
                          icon = shiny$icon("rotate-left"),
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              ),

              # --- Reset All ---
              shiny$div(
                class = "settings-reset-all",
                shiny$actionButton(
                  ns("reset_default"),
                  "Reset All Settings",
                  icon = shiny$icon("rotate-left"),
                  width = "100%"
                )
              )
            ),
            footer = shiny$tagList(
              shiny$modalButton("Dismiss"),
              shiny$actionButton(
                ns("save_settings"),
                "Save",
                icon = shiny$icon("floppy-disk"),
                class = "load-db"
              ),
            )
          )
        )
      )
    }

    d <- get_default_user_settings()

    do_reset_general <- function() {
      shiny::updateTextInput(
        session,
        "settings_input_path",
        value = d$deconv_input_dir
      )
      shiny::updateTextInput(session, "settings_dest_path", value = "")
      shiny::updateCheckboxInput(
        session,
        "settings_keep_raw_output",
        value = d$deconv_keep_raw_output
      )
    }

    do_reset_deconv <- function() {
      shiny::updateNumericInput(
        session,
        "settings_startz",
        value = d$deconv_startz
      )
      shiny::updateNumericInput(session, "settings_endz", value = d$deconv_endz)
      shiny::updateNumericInput(
        session,
        "settings_minmz",
        value = d$deconv_minmz
      )
      shiny::updateNumericInput(
        session,
        "settings_maxmz",
        value = d$deconv_maxmz
      )
      shiny::updateNumericInput(
        session,
        "settings_masslb",
        value = d$deconv_masslb
      )
      shiny::updateNumericInput(
        session,
        "settings_massub",
        value = d$deconv_massub
      )
      shiny::updateNumericInput(
        session,
        "settings_time_start",
        value = d$deconv_time_start
      )
      shiny::updateNumericInput(
        session,
        "settings_time_end",
        value = d$deconv_time_end
      )
      shiny::updateNumericInput(
        session,
        "settings_peakwindow",
        value = d$deconv_peakwindow
      )
      shiny::updateSelectInput(
        session,
        "settings_peaknorm",
        selected = d$deconv_peaknorm
      )
      shiny::updateNumericInput(
        session,
        "settings_peakthresh",
        value = d$deconv_peakthresh
      )
      shiny::updateNumericInput(
        session,
        "settings_massbins",
        value = d$deconv_massbins
      )
    }

    do_reset_conv <- function() {
      shiny::updateNumericInput(
        session,
        "settings_peak_tol",
        value = d$peak_tolerance
      )
      shiny::updateNumericInput(
        session,
        "settings_max_mult",
        value = d$max_multiples
      )
      shiny::updateSelectInput(
        session,
        "settings_hit_pref",
        selected = d$hit_preference
      )
    }

    do_reset_logs <- function() {
      shiny::updateTextInput(session, "settings_log_dir", value = d$log_dir)
    }

    shiny$observeEvent(input$reset_general, do_reset_general())
    shiny$observeEvent(input$reset_deconv, do_reset_deconv())
    shiny$observeEvent(input$reset_conv, do_reset_conv())
    shiny$observeEvent(input$reset_logs, do_reset_logs())
    shiny$observeEvent(input$reset_default, {
      do_reset_general()
      do_reset_deconv()
      do_reset_conv()
      do_reset_logs()
    })

    # Resolve typed/pasted path from the text input
    settings_dest_picked <- shiny$reactive({
      p <- input$settings_dest_path
      trimws(if (!is.null(p)) p else dest_settings()$path)
    })

    settings_input_path_picked <- shiny$reactive({
      p <- input$settings_input_path
      trimws(if (!is.null(p)) p else read_user_settings()$deconv_input_dir)
    })

    settings_log_dir_picked <- shiny$reactive({
      p <- input$settings_log_dir
      trimws(if (!is.null(p)) p else "")
    })

    # Settings opened from nav button
    shiny$observeEvent(input$settings, {
      open_settings_modal()
    })

    # Live feedback for output path inside modal
    output$settings_dest_path_display <- shiny$renderUI({
      path <- settings_dest_picked()
      if (!nzchar(path)) {
        return(shiny$div(
          class = "settings-dest-feedback",
          shiny$icon("circle-minus"),
          " No default set"
        ))
      }
      if (dir.exists(path)) {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--valid",
          shiny$icon("circle-check"),
          " Folder exists"
        )
      } else {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--invalid",
          shiny$icon("triangle-exclamation"),
          " Folder not found"
        )
      }
    })

    # Live feedback for input folder path inside modal
    output$settings_input_path_display <- shiny$renderUI({
      path <- settings_input_path_picked()
      if (!nzchar(path)) {
        return(shiny$div(
          class = "settings-dest-feedback",
          shiny$icon("circle-minus"),
          " No default set"
        ))
      }
      if (has_ms_extension(path)) {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--invalid",
          shiny$icon("triangle-exclamation"),
          " Cannot use a sample as default input; pick the folder holding it"
        )
      } else if (dir.exists(path)) {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--valid",
          shiny$icon("circle-check"),
          " Folder exists"
        )
      } else {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--invalid",
          shiny$icon("triangle-exclamation"),
          " Folder not found"
        )
      }
    })

    # Live feedback for log directory path inside modal
    output$settings_log_dir_display <- shiny$renderUI({
      path <- settings_log_dir_picked()
      if (!nzchar(path)) {
        return(shiny$div(
          class = "settings-dest-feedback",
          shiny$icon("circle-minus"),
          " Using default"
        ))
      }
      if (dir.exists(path)) {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--valid",
          shiny$icon("circle-check"),
          " Folder exists"
        )
      } else {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--invalid",
          shiny$icon("triangle-exclamation"),
          " Folder not found"
        )
      }
    })

    # Helper tags for settings validation feedback
    settings_ok_tag <- function(msg = "Valid") {
      shiny$div(
        class = "settings-feedback settings-feedback--valid",
        shiny$icon("circle-check"),
        paste0(" ", msg)
      )
    }
    settings_err_tag <- function(msg) {
      shiny$div(
        class = "settings-feedback settings-feedback--invalid",
        shiny$icon("triangle-exclamation"),
        paste0(" ", msg)
      )
    }

    # Peak Tolerance [Da] — min 0, max 20
    output$settings_peak_tol_feedback <- shiny$renderUI({
      val <- input$settings_peak_tol
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 20) {
        return(settings_err_tag("Must be between 0 and 20 Da"))
      }
      settings_ok_tag("Valid")
    })

    # Max. Stoichiometry — min 1, max 20, integer
    output$settings_max_mult_feedback <- shiny$renderUI({
      val <- input$settings_max_mult
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1 || val > 20) {
        return(settings_err_tag("Must be between 1 and 20"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      settings_ok_tag("Valid")
    })

    # Preferred Assignment — always valid (fixed choices)
    output$settings_hit_pref_feedback <- shiny$renderUI({
      settings_ok_tag("Valid")
    })

    # Min. charge state [z] — min 1, integer, < endz
    output$settings_startz_feedback <- shiny$renderUI({
      val <- input$settings_startz
      endz <- input$settings_endz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(endz) && !is.na(endz) && val >= endz) {
        return(settings_err_tag("Must be less than max. charge state"))
      }
      settings_ok_tag("Valid")
    })

    # Max. charge state [z] — min 1, integer, > startz
    output$settings_endz_feedback <- shiny$renderUI({
      val <- input$settings_endz
      startz <- input$settings_startz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(startz) && !is.na(startz) && val <= startz) {
        return(settings_err_tag("Must be greater than min. charge state"))
      }
      settings_ok_tag("Valid")
    })

    # Lower deconvolution range [m/z] — min 1, max 100000, integer, < maxmz
    output$settings_minmz_feedback <- shiny$renderUI({
      val <- input$settings_minmz
      maxmz <- input$settings_maxmz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(maxmz) && !is.na(maxmz) && val >= maxmz) {
        return(settings_err_tag("Must be less than upper m/z"))
      }
      settings_ok_tag("Valid")
    })

    # Upper deconvolution range [m/z] — min 1, max 100000, integer, > minmz
    output$settings_maxmz_feedback <- shiny$renderUI({
      val <- input$settings_maxmz
      minmz <- input$settings_minmz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(minmz) && !is.na(minmz) && val <= minmz) {
        return(settings_err_tag("Must be greater than lower m/z"))
      }
      settings_ok_tag("Valid")
    })

    # Lower mass range [Da] — min 1, max 2000000, integer, < massub
    output$settings_masslb_feedback <- shiny$renderUI({
      val <- input$settings_masslb
      massub <- input$settings_massub
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1 Da"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(massub) && !is.na(massub) && val >= massub) {
        return(settings_err_tag("Must be less than upper mass"))
      }
      settings_ok_tag("Valid")
    })

    # Upper mass range [Da] — min 1, max 2000000, integer, > masslb
    output$settings_massub_feedback <- shiny$renderUI({
      val <- input$settings_massub
      masslb <- input$settings_masslb
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1 Da"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(masslb) && !is.na(masslb) && val <= masslb) {
        return(settings_err_tag("Must be greater than lower mass"))
      }
      settings_ok_tag("Valid")
    })

    # Elution start time [min] — blank = first scan, else 0-100, < time_end
    output$settings_time_start_feedback <- shiny$renderUI({
      val <- input$settings_time_start
      time_end <- input$settings_time_end
      if (is.null(val) || is.na(val)) {
        return(settings_ok_tag("Valid"))
      }
      if (val < 0 || val > 100) {
        return(settings_err_tag("Must be between 0 and 100 min"))
      }
      if (!is.null(time_end) && !is.na(time_end) && val >= time_end) {
        return(settings_err_tag("Must be earlier than end time"))
      }
      settings_ok_tag("Valid")
    })

    # Elution end time [min] — blank = last scan, else 0-100, > time_start
    output$settings_time_end_feedback <- shiny$renderUI({
      val <- input$settings_time_end
      time_start <- input$settings_time_start
      if (is.null(val) || is.na(val)) {
        return(settings_ok_tag("Valid"))
      }
      if (val < 0 || val > 100) {
        return(settings_err_tag("Must be between 0 and 100 min"))
      }
      if (!is.null(time_start) && !is.na(time_start) && val <= time_start) {
        return(settings_err_tag("Must be later than start time"))
      }
      settings_ok_tag("Valid")
    })

    # Detection window [Da] — min 1, max 500, integer
    output$settings_peakwindow_feedback <- shiny$renderUI({
      val <- input$settings_peakwindow
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1 || val > 500) {
        return(settings_err_tag("Must be between 1 and 500 Da"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      settings_ok_tag("Valid")
    })

    # Peak normalization — always valid (fixed choices)
    output$settings_peaknorm_feedback <- shiny$renderUI({
      settings_ok_tag("Valid")
    })

    # Peak threshold — min 0, max 1
    output$settings_peakthresh_feedback <- shiny$renderUI({
      val <- input$settings_peakthresh
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 1) {
        return(settings_err_tag("Must be between 0 and 1"))
      }
      settings_ok_tag("Valid")
    })

    shiny$observeEvent(input$save_settings, {
      # --- Output path ---
      path <- settings_dest_picked()
      if (!dir.exists(settings_dir)) {
        dir.create(settings_dir, recursive = TRUE)
      }
      new_dest <- list(path = path, enabled = nzchar(path))
      saveRDS(new_dest, dest_settings_file)
      dest_settings(new_dest)

      # --- Numeric defaults — only overwrite keys whose input passes validation.
      # Empty/invalid fields are left at their stored value (or built-in default).
      # Note: is.numeric(NA) is FALSE in R, so we use is.na() directly.
      current <- read_user_settings() # already NA-sanitised; contains stored or defaults
      ok <- function(v) !is.null(v) && !is.na(v)
      int_ok <- function(v) ok(v) && v == floor(v)

      pt <- input$settings_peak_tol
      if (ok(pt) && pt >= 0 && pt <= 20) {
        current$peak_tolerance <- pt
      }

      mm <- input$settings_max_mult
      if (int_ok(mm) && mm >= 1 && mm <= 20) {
        current$max_multiples <- mm
      }

      current$hit_preference <- hit_preference_rule(input$settings_hit_pref)

      pw <- input$settings_peakwindow
      if (int_ok(pw) && pw >= 1 && pw <= 500) {
        current$deconv_peakwindow <- pw
      }

      current$deconv_peaknorm <- as.numeric(input$settings_peaknorm)

      p2 <- input$settings_peakthresh
      if (ok(p2) && p2 >= 0 && p2 <= 1) {
        current$deconv_peakthresh <- p2
      }

      # Paired fields: save the pair only when both individually valid AND ordered;
      # save each side independently when its counterpart is absent/invalid.
      sz <- input$settings_startz
      sz_ok <- int_ok(sz) && sz >= 1
      ez <- input$settings_endz
      ez_ok <- int_ok(ez) && ez >= 1
      if (sz_ok && ez_ok) {
        if (sz < ez) {
          current$deconv_startz <- sz
          current$deconv_endz <- ez
        }
      } else {
        if (sz_ok) {
          current$deconv_startz <- sz
        }
        if (ez_ok) current$deconv_endz <- ez
      }

      mn <- input$settings_minmz
      mn_ok <- int_ok(mn) && mn >= 1
      mx <- input$settings_maxmz
      mx_ok <- int_ok(mx) && mx >= 1
      if (mn_ok && mx_ok) {
        if (mn < mx) {
          current$deconv_minmz <- mn
          current$deconv_maxmz <- mx
        }
      } else {
        if (mn_ok) {
          current$deconv_minmz <- mn
        }
        if (mx_ok) current$deconv_maxmz <- mx
      }

      lb <- input$settings_masslb
      lb_ok <- int_ok(lb) && lb >= 1
      ub <- input$settings_massub
      ub_ok <- int_ok(ub) && ub >= 1
      if (lb_ok && ub_ok) {
        if (lb < ub) {
          current$deconv_masslb <- lb
          current$deconv_massub <- ub
        }
      } else {
        if (lb_ok) {
          current$deconv_masslb <- lb
        }
        if (ub_ok) current$deconv_massub <- ub
      }

      # A blank elution bound is a valid choice (open window), so it is saved as
      # NA rather than skipped -- otherwise a stored bound could never be cleared.
      ts <- input$settings_time_start
      te <- input$settings_time_end
      if (!ok(ts)) {
        ts <- NA_real_
      }
      if (!ok(te)) {
        te <- NA_real_
      }
      ts_ok <- is.na(ts) || (ts >= 0 && ts <= 100)
      te_ok <- is.na(te) || (te >= 0 && te <= 100)
      if (ts_ok && te_ok) {
        if (is.na(ts) || is.na(te) || ts < te) {
          current$deconv_time_start <- ts
          current$deconv_time_end <- te
        }
      } else {
        if (ts_ok) {
          current$deconv_time_start <- ts
        }
        if (te_ok) current$deconv_time_end <- te
      }

      current$deconv_keep_raw_output <- isTRUE(input$settings_keep_raw_output)

      inp <- trimws(
        if (!is.null(input$settings_input_path)) {
          input$settings_input_path
        } else {
          ""
        }
      )
      current$deconv_input_dir <- inp

      current$log_dir <- trimws(
        if (!is.null(input$settings_log_dir)) input$settings_log_dir else ""
      )

      save_user_settings(current)

      shiny$removeModal()
      shinyWidgets::show_toast(
        "Settings saved.",
        text = NULL,
        type = "success",
        timer = 3000,
        timerProgressBar = TRUE
      )
    })

    # Deconvolution sidebar server
    deconvolution_sidebar_vars <- deconvolution_sidebar$server(
      "deconvolution_pars",
      reset_button = reset_button,
      config_file = configfile,
      config_filename = config_filename,
      default_dest_path = shiny$reactive({
        s <- dest_settings()
        if (nzchar(s$path) && dir.exists(s$path)) s$path else NULL
      }),
      default_input_path = shiny$reactive({
        p <- read_user_settings()$deconv_input_dir
        if (length(p) == 1L && nzchar(p) && dir.exists(p)) p else NULL
      })
    )

    # Save default input folder when floppy button next to Input is clicked
    shiny$observeEvent(
      deconvolution_sidebar_vars$save_input_dir_clicked(),
      {
        p <- deconvolution_sidebar_vars$dir()
        if (
          length(p) == 1L &&
            nzchar(p) &&
            has_ms_extension(p)
        ) {
          shinyWidgets::show_toast(
            title = "Cannot save a sample as default input; pick the folder holding it.",
            text = NULL,
            type = "error",
            timer = 3000,
            timerProgressBar = TRUE
          )
        } else if (length(p) == 1L && nzchar(p) && dir.exists(p)) {
          update_user_setting("deconv_input_dir", p)
          shinyWidgets::show_toast(
            title = "Default input folder saved.",
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Settings opened from sidebar gear button — pre-fill with currently active path
    shiny$observeEvent(
      deconvolution_sidebar_vars$open_settings_clicked(),
      {
        open_settings_modal(
          initial_path = deconvolution_sidebar_vars$targetpath(),
          open_section = "general"
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Deconvolution process server
    deconvolution_main_vars <- deconvolution_main$server(
      "deconvolution_main",
      deconvolution_sidebar_vars,
      conversion_main_vars,
      reset_button = reset_button,
      config_file = configfile
    )

    # Conversion sidebar server
    conversion_sidebar_vars <- conversion_sidebar$server(
      "conversion_sidebar",
      conversion_main_vars,
      deconvolution_main_vars,
      config_file = configfile,
      config_filename = config_filename
    )

    # Conversion main server
    conversion_main_vars <- conversion_main$server(
      "conversion_main",
      conversion_sidebar_vars,
      deconvolution_main_vars,
      config_file = configfile,
      config_apply_trigger = config_apply_trigger
    )

    # Check update availability
    version_info <- get_kiwims_version()

    local_version <- unname(version_info["version"])
    release <- unname(version_info["date"])
    url <- unname(version_info["url"])
    remote_version <- sub(".*=", "", check_github_version())

    if (identical(local_version, remote_version)) {
      # Variables for modal
      message <- "KiwiMS is up-to-date"
      hint <- "No action needed."
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiMS/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-info")
      label <- "Version"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    } else {
      # Variables for modal
      message <- "Update available"
      hint <- paste(
        "Download the latest version <strong>",
        remote_version,
        "</strong>from the release page:"
      )
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiMS/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-exclamation")
      label <- "Update"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    }

    output$update_button <- shiny$renderUI({
      shiny$req(icon, label)

      shiny::tags$a(
        icon,
        label
      )

      # shiny$actionButton(
      #   inputId = ns("open_update_modal"),
      #   label = label,
      #   icon = icon,
      #   class = "nav-link"
      # )
    })

    # Switch Protein Conversion tab when user forwards
    shiny$observeEvent(deconvolution_main_vars$forward_deconvolution(), {
      bslib::nav_select(
        "tabs",
        session = session,
        "Protein Conversion"
      )
    })

    # Switch back to Deconvolution module when user forwards
    shiny$observeEvent(conversion_main_vars$cancel_continuation(), {
      bslib::nav_select(
        "tabs",
        session = session,
        "Deconvolution"
      )
    })

    # Config Modal Window ----

    # Download handler for example config file
    output$download_example_config <- shiny$downloadHandler(
      filename = "example_config.csv",
      content = function(file) {
        example <- data.frame(
          Sample = c("sample_1", "sample_2", "sample_3"),
          Replicate = c("R1", "R1", "R2"),
          Protein = c("RACA", "RACA", "RACA"),
          Well = c("A1", "A2", "A3"),
          Compound_Concentration = c(100, 200, 100),
          Concentration_Unit = c("nM", "nM", "nM"),
          Incubation_Time = c(120, 120, 60),
          Time_Unit = c("s", "s", "s"),
          Compound_1 = c("Cmp1", "Cmp1", "Cmp2"),
          Compound_2 = c("Cmp2", "Cmp2", "Cmp3"),
          Compound_3 = c("Cmp3", "Cmp3", "Cmp4"),
          Compound_4 = c("Cmp4", "Cmp4", "Cmp5"),
          Compound_5 = c("Cmp5", "Cmp5", "Cmp6"),
          stringsAsFactors = FALSE
        )
        utils::write.csv2(example, file, row.names = FALSE)
      }
    )

    # Modal body — three pages: "upload", "preview", "confirmed"
    output$config_modal_body <- shiny$renderUI({
      state <- config_modal_state()

      if (state == "upload") {
        shiny$div(
          class = "config-modal-body",
          shiny$tags$p(
            "Upload a semicolon- or comma-separated ",
            shiny$tags$b(".csv"),
            " or ",
            shiny$tags$b(".xlsx"),
            " file that maps your sample files to experimental metadata."
          ),
          shiny$tags$table(
            class = "config-ref-table",
            shiny$tags$thead(
              shiny$tags$tr(
                shiny$tags$th("Column"),
                shiny$tags$th("Required"),
                shiny$tags$th("Format / Notes")
              )
            ),
            shiny$tags$tbody(
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Sample")),
                shiny$tags$td(class = "config-col-required", "Yes"),
                shiny$tags$td("Unique identifier per row, no duplicates")
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Replicate")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "Replicate series, e.g. R1, R2 \u00b7 at most 4 \u00b7 partial fill allowed"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Protein")),
                shiny$tags$td(class = "config-col-required", "Yes"),
                shiny$tags$td("Protein name, no empty values")
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Well")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "Valid well plate ID up to 384-well format (A1\u2013P24) \u00b7 all filled or all empty"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Compound_Concentration")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "Numeric \u00b7 all filled or all empty \u00b7 displayed as \u201cConcentration\u201d"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Concentration_Unit")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "M \u00b7 mM \u00b7 \u03bcM \u00b7 nM \u00b7 pM \u00b7 same value in every row \u00b7 preselects the Conc. Unit picker"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Incubation_Time")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "Numeric \u00b7 all filled or all empty \u00b7 displayed as \u201cTime\u201d"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Time_Unit")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "s \u00b7 min \u00b7 same value in every row \u00b7 preselects the Time Unit picker"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Compound_1 \u2013 Compound_5")),
                shiny$tags$td(class = "config-col-required", "Min. 1"),
                shiny$tags$td(
                  "Compound names \u00b7 no duplicates within a row"
                )
              )
            )
          ),
          shiny$div(
            class = "config-upload-row",
            shiny$fileInput(
              ns("experiment_config"),
              label = NULL,
              placeholder = "Select .csv or .xlsx",
              accept = c(".csv", ".xlsx")
            ),
            shiny$downloadButton(
              ns("download_example_config"),
              "Example Table",
              class = "btn-sm btn-default"
            )
          ),
          shiny$uiOutput(ns("config_check"))
        )
      } else if (state == "preview") {
        df <- pending_config()
        n_compounds <- length(grep("^Compound_\\d+$", names(df)))
        shiny$div(
          class = "config-modal-body",
          shiny$tags$p(
            class = "config-preview-intro",
            "Please verify the table below matches your experiment layout.",
            " Confirm to activate this config across all modules."
          ),
          config_badge(
            "ok",
            "Valid",
            paste0(
              nrow(df),
              " samples \u00b7 ",
              n_compounds,
              " compound column(s)"
            )
          ),
          shiny$hr(class = "config-section-hr"),
          shiny$div(
            class = "config-table-scroll",
            shinycssloaders::withSpinner(
              shiny$tableOutput(ns("config_table")),
              type = 1,
              color = "#7777f9"
            )
          )
        )
      } else {
        df <- configfile()
        n_compounds <- length(grep("^Compound_\\d+$", names(df)))
        shiny$div(
          class = "config-modal-body",
          shiny$tags$p("A configuration file is currently active."),
          config_badge(
            "ok",
            "Active",
            paste0(
              nrow(df),
              " samples \u00b7 ",
              n_compounds,
              " compound column(s)"
            )
          ),
          shiny$tags$p(
            class = "config-filename-label",
            shiny$icon("file"),
            shiny$tags$span(class = "config-filename-text", config_filename())
          ),
          shiny$hr(class = "config-section-hr"),
          shiny$div(
            class = "config-table-scroll",
            shiny$tableOutput(ns("confirmed_config_table"))
          )
        )
      }
    })

    # "Apply to samples" footer button — only offered when the modal was opened
    # from the Protein Conversion module. Follows the same sample table states
    # as the use_config button there; disabled with the reason as tooltip.
    config_apply_button <- function(id, label) {
      if (!isTRUE(config_modal_from_conversion())) {
        return(NULL)
      }
      block <- conversion_main_vars$config_apply_block()
      btn <- shiny$actionButton(
        ns(id),
        label,
        icon = config_icon(apply = TRUE),
        class = "btn btn-default config-apply-btn"
      )
      if (is.null(block)) {
        return(btn)
      }
      bslib$tooltip(
        shiny$div(class = "config-apply-btn-wrapper", shinyjs::disabled(btn)),
        block,
        placement = "top"
      )
    }

    # Modal footer — three states (always includes Dismiss)
    output$config_modal_footer <- shiny$renderUI({
      state <- config_modal_state()
      if (state == "upload") {
        shiny$modalButton("Dismiss")
      } else if (state == "preview") {
        shiny$tagList(
          shiny$actionButton(
            ns("confirm_config"),
            "Confirm",
            class = "btn btn-default"
          ),
          config_apply_button(
            "confirm_apply_config",
            "Confirm & Apply to Samples"
          ),
          shiny$modalButton("Dismiss")
        )
      } else {
        shiny$tagList(
          config_apply_button("apply_config", "Apply to Samples"),
          shiny$actionButton(
            ns("remove_config"),
            "Remove Config",
            class = "btn btn-default"
          ),
          shiny$modalButton("Dismiss")
        )
      }
    })

    # Preloaded modal bodies (defined here, referenced via uiOutput in showModal)
    output$licence_modal_body <- shiny$renderUI(licence_modal_body())
    output$unidec_modal_body <- shiny$renderUI(unidec_modal_body())
    output$update_modal_body <- shiny$renderUI({
      shiny$req(local_version, release, message, link, hint)
      update_modal_body(local_version, release, message, link, hint)
    })

    # Eagerly render all modal outputs even before the modal is opened
    shiny$outputOptions(output, "config_modal_body", suspendWhenHidden = FALSE)
    shiny$outputOptions(
      output,
      "config_modal_footer",
      suspendWhenHidden = FALSE
    )
    shiny$outputOptions(output, "licence_modal_body", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "unidec_modal_body", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "update_modal_body", suspendWhenHidden = FALSE)

    # Prepare config data frame for display: convert numeric cols to character
    # to prevent xtable rounding, drop all-empty columns, and rename display headers.
    config_table_df <- function(df) {
      for (col in intersect(
        c("Compound_Concentration", "Incubation_Time"),
        names(df)
      )) {
        if (is.numeric(df[[col]])) df[[col]] <- as.character(df[[col]])
      }
      empty_cols <- names(df)[sapply(names(df), function(col) {
        all(is.na(df[[col]]) | trimws(as.character(df[[col]])) == "")
      })]
      df <- df[, setdiff(names(df), empty_cols), drop = FALSE]
      names(df)[names(df) == "Compound_Concentration"] <- "Concentration"
      names(df)[names(df) == "Concentration_Unit"] <- "Conc. Unit"
      names(df)[names(df) == "Incubation_Time"] <- "Time"
      names(df)[names(df) == "Time_Unit"] <- "Time Unit"
      df
    }

    # Pending config table — Sys.sleep drives the spinner for 1 second
    output$config_table <- shiny$renderTable(
      {
        shiny$req(pending_config())
        Sys.sleep(1)
        config_table_df(pending_config())
      },
      striped = TRUE,
      hover = FALSE,
      bordered = TRUE,
      spacing = "xs",
      na = ""
    )

    # Confirmed config table (no spinner needed)
    output$confirmed_config_table <- shiny$renderTable(
      {
        shiny$req(configfile())
        config_table_df(configfile())
      },
      striped = TRUE,
      hover = FALSE,
      bordered = TRUE,
      spacing = "xs",
      na = ""
    )

    # Validate on upload — 1-second spinner buffer before showing preview page
    shiny$observeEvent(input$experiment_config, {
      shiny$req(input$experiment_config)

      path <- input$experiment_config$datapath
      ext <- tolower(tools::file_ext(input$experiment_config$name))
      df <- tryCatch(read_config_file(path, ext), error = function(e) NULL)

      if (is.null(df)) {
        output$config_check <- shiny$renderUI(config_badge(
          "err",
          "Error",
          "Failed to read file."
        ))
        return()
      }
      if (nrow(df) == 0) {
        output$config_check <- shiny$renderUI(config_badge(
          "err",
          "Error",
          "File is empty."
        ))
        return()
      }

      df <- normalize_colnames(df)
      df <- normalize_config_units(df)
      issues <- validate_config(df)

      if (length(issues) > 0) {
        output$config_check <- shiny$renderUI(
          config_badge("err", paste(length(issues), "issue(s)"), issues)
        )
        return()
      }

      # Clear pending first, switch UI (flush 1 → DOM element created with spinner),
      # then set data in the next flush so the spinner is visible.
      pending_config(NULL)
      config_modal_state("preview")
      df_captured <- df
      session$onFlushed(
        function() {
          pending_config(df_captured)
        },
        once = TRUE
      )
    })

    # Confirm — write to configfile, store filename, close modal, toast
    commit_pending_config <- function() {
      configfile(pending_config())
      config_filename(input$experiment_config$name)
      pending_config(NULL)
      shiny$removeModal()
    }

    shiny$observeEvent(input$confirm_config, {
      commit_pending_config()
      show_toast(
        "Config saved!",
        text = NULL,
        type = "success",
        timer = 2000,
        timerProgressBar = TRUE
      )
    })

    # Confirm & Apply — as Confirm, then autofill the samples table exactly as
    # the use_config button of the Protein Conversion module does
    shiny$observeEvent(input$confirm_apply_config, {
      shiny$req(is.null(conversion_main_vars$config_apply_block()))
      commit_pending_config()
      config_apply_trigger(config_apply_trigger() + 1L)
      show_toast(
        "Config saved and applied to samples!",
        text = NULL,
        type = "success",
        timer = 2000,
        timerProgressBar = TRUE
      )
    })

    # Apply — autofill the samples table from the already active config
    shiny$observeEvent(input$apply_config, {
      shiny$req(!is.null(configfile()))
      shiny$req(is.null(conversion_main_vars$config_apply_block()))
      config_apply_trigger(config_apply_trigger() + 1L)
      shiny$removeModal()
      show_toast(
        "Config applied to samples!",
        text = NULL,
        type = "success",
        timer = 2000,
        timerProgressBar = TRUE
      )
    })

    # Cancel — discard pending, close modal
    shiny$observeEvent(input$cancel_config, {
      pending_config(NULL)
      shiny$removeModal()
    })

    # Remove — clear confirmed config, reset check output, switch to upload page
    shiny$observeEvent(input$remove_config, {
      configfile(NULL)
      config_filename(NULL)
      pending_config(NULL)
      output$config_check <- shiny$renderUI(NULL)
      config_modal_state("upload")
      show_toast(
        "Config removed",
        text = NULL,
        type = "warning",
        timer = 3000,
        timerProgressBar = TRUE
      )
    })

    # Shared helper — opens the config modal (used by the sidebar shortcuts)
    open_config_modal <- function(force_upload = FALSE, from_conversion = FALSE) {
      config_modal_from_conversion(from_conversion)
      pending_config(NULL)
      output$config_check <- shiny$renderUI(NULL)
      if (!force_upload && !is.null(configfile())) {
        config_modal_state("confirmed")
      } else {
        config_modal_state("upload")
      }
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = shiny$tagList(config_icon(), " Experiment Configuration"),
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("config_modal_body")),
            footer = shiny$uiOutput(ns("config_modal_footer"))
          )
        )
      )
    }

    # Open modal via deconvolution sidebar shortcut
    shiny$observeEvent(
      deconvolution_sidebar_vars$open_config_clicked(),
      {
        open_config_modal()
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Open modal via conversion sidebar shortcut
    shiny$observeEvent(
      conversion_sidebar_vars$open_config_clicked(),
      {
        open_config_modal(from_conversion = TRUE)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Licence Modal Window ----
    shiny::observeEvent(input$licence, {
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "End-User License Agreement (GPL v3)",
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("licence_modal_body")),
            footer = shiny$modalButton("Dismiss")
          )
        )
      )
    })

    # Unidec Modal Window ----
    shiny::observeEvent(input$unidec_click, {
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "UniDec - Acknowledgement",
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("unidec_modal_body")),
            footer = shiny$modalButton("Dismiss")
          )
        )
      )
    })

    # Update modal
    shiny$observeEvent(input$open_update_modal, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            title = "Version and Update",
            easyClose = TRUE,
            shiny$uiOutput(ns("update_modal_body")),
            footer = shiny$modalButton("Dismiss")
          )
        )
      )
    })

    session$onFlushed(
      function() {
        waiter_hide()
      },
      once = TRUE
    )
  })
}
