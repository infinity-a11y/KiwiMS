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
      get_kiwims_version,
      get_latest_release_url,
      get_volumes,
      normalize_colnames,
      normalize_config_units,
      read_config_file,
      validate_config,
    ],
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
        var nodes = div.querySelectorAll('.infolayer .annotation, .infolayer .g-gtitle');
        Array.prototype.forEach.call(nodes, function(node) {
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

      // Plotly.downloadImage does NOT serialise the plot it is handed: it
      // clones the figure into a fresh element, re-plots it from layout/data and
      // serialises the clone. Anything written into the rendered SVG - the
      // enlarged legend key symbols, the widened legend clip - is on the
      // original node and is simply thrown away, which is why those fixes kept
      // looking correct on screen and kept coming out wrong in the file.
      // Snapshot.toSVG serialises the node it is given, so this hands it the
      // node that was actually measured and adjusted. It bakes any WebGL canvas
      // into the SVG as a raster image on the way, so 3D scenes survive too.
      function kiwiSaveImage(div, format, width, height, scale, filename) {
        var svg = Plotly.Snapshot.toSVG(div, format, 1);
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
          // multiple_spectra() sets no margin, so the figure falls back to
          // Plotly's defaults - a fixed pixel count that does not grow with
          // the labels and does not know how big this canvas is. The tick
          // labels at the ends of the mass axis sit right on the scene edge,
          // so as soon as they are scaled up they run past it and come out
          // clipped mid-glyph. Give the figure room in proportion to the
          // labels it is actually drawing; an explicit margin from R still
          // wins.
          var pad = Math.round(90 * scene3DScale);
          fig.layout.margin = Object.assign(
            { l: pad, r: pad, t: pad, b: pad, pad: 0 },
            fig.layout.margin || {}
          );
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
        document.body.style.cursor = 'progress';
        var div = document.createElement('div');
        div.style.width  = q.width  + 'px';
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
        Plotly.newPlot(div, fig.data, fig.layout, fig.config || {}).then(function() {
          // For WebGL/3D, wait for the GPU to finish rendering before screenshotting.
          return new Promise(function(resolve) { setTimeout(resolve, isCubicSpectra ? 800 : 0); });
        }).then(function() {
          return kiwiFitPaper(div);
        }).then(function() {
          return kiwiFitLegend(div, q.width);
        }).then(function(finalWidth) {
          kiwiScaleLegendSymbols(div, legendSymbolScale);
          // After the symbols, so the clip is fitted to the final contents.
          kiwiFitLegendClip(div);
          return kiwiSaveImage(
            div, msg.format, finalWidth, q.height,
            msg.format === 'svg' ? 1 : q.scale, msg.filename);
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
      bslib$nav_item(shiny::uiOutput(ns("config_nav_btn"))),
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
                        shiny$tags$span(
                          "Keep UniDec output files",
                          class = "settings-label-tooltip",
                          `data-tooltip` = "Keeps *_rawdata.txt and *_rawdata_unidecfiles/ after analysis"
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
                          max = 100,
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
                          max = 100,
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
                        "Elution start time [min]"
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
                        "Elution end time [min]"
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
                        shiny$tags$span(
                          "Log Directory",
                          class = "settings-label-tooltip",
                          `data-tooltip` = "Parent folder for daily log sub-folders. Takes effect on next app start."
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
      if (grepl("\\.raw$", path, ignore.case = TRUE)) {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--invalid",
          shiny$icon("triangle-exclamation"),
          " Cannot use a .raw folder as default input"
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

    # Min. charge state [z] — min 1, max 100, integer, < endz
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

    # Max. charge state [z] — min 1, max 100, integer, > startz
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

    # Elution start time [min] — min 0, max 100, < time_end
    output$settings_time_start_feedback <- shiny$renderUI({
      val <- input$settings_time_start
      time_end <- input$settings_time_end
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 100) {
        return(settings_err_tag("Must be between 0 and 100 min"))
      }
      if (!is.null(time_end) && !is.na(time_end) && val >= time_end) {
        return(settings_err_tag("Must be earlier than end time"))
      }
      settings_ok_tag("Valid")
    })

    # Elution end time [min] — min 0, max 100, > time_start
    output$settings_time_end_feedback <- shiny$renderUI({
      val <- input$settings_time_end
      time_start <- input$settings_time_start
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
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

      ts <- input$settings_time_start
      ts_ok <- ok(ts) && ts >= 0 && ts <= 100
      te <- input$settings_time_end
      te_ok <- ok(te) && te >= 0 && te <= 100
      if (ts_ok && te_ok) {
        if (ts < te) {
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
            grepl("\\.raw$", p, ignore.case = TRUE)
        ) {
          shinyWidgets::show_toast(
            title = "Cannot save a .raw folder as default input.",
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
      config_file = configfile
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

    # Nav button — filled green circle = active, outlined black circle = none
    output$config_nav_btn <- shiny$renderUI({
      indicator <- if (!is.null(configfile())) {
        shiny$tags$i(class = "fa-solid fa-circle config-nav-indicator--active")
      } else {
        shiny$tags$span(class = "config-nav-indicator--inactive")
      }
      shiny$actionButton(
        ns("config"),
        shiny$tagList(indicator, " Config"),
        class = "nav-link"
      )
    })

    # Download handler for example config file
    output$download_example_config <- shiny$downloadHandler(
      filename = "example_config.csv",
      content = function(file) {
        example <- data.frame(
          Sample = c("sample_1.raw", "sample_2.raw", "sample_3.raw"),
          Replicate = c("Rep1", "Rep1", "Rep2"),
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
                  "Replicate group label \u00b7 free text \u00b7 partial fill allowed"
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
          shiny$modalButton("Dismiss")
        )
      } else {
        shiny$tagList(
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
    shiny$observeEvent(input$confirm_config, {
      configfile(pending_config())
      config_filename(input$experiment_config$name)
      pending_config(NULL)
      shiny$removeModal()
      show_toast(
        "Config saved!",
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

    # Shared helper — opens the config modal (used by nav button and sidebar shortcut)
    open_config_modal <- function(force_upload = FALSE) {
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
            title = "Experiment Configuration",
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("config_modal_body")),
            footer = shiny$uiOutput(ns("config_modal_footer"))
          )
        )
      )
    }

    # Open modal via nav button
    shiny$observeEvent(input$config, {
      open_config_modal()
    })

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
        open_config_modal()
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
