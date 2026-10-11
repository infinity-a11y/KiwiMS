// Well plate heatmaps (Batch Control): the plate's geometry, fitted to the
// plot after every drawing and resize.
//
// plotly keeps the tiles square on its own (scaleanchor with constrain:
// "domain"), but the constrained domain sticks: drawn first while the tab is
// hidden, or at another size, the plate stayed that size in a larger card.
// So here the plate is laid out from the plot's real size each time:
// - the largest square tiles that fit beside the legend or colorbar on the
//   right and above the row of well states (legend2),
// - plate and legend centred as one in the plot, the plate at the top,
// - the well states right under the plate, the colorbar as long as it,
// - the tile labels sized to the tiles.
// `spec` comes from batch_plate_heatmap(): cols, rows and the margins l, t, b.
(function () {
  var MAX_PASSES = 4;

  function rightWidth(gd) {
    var bg = gd.querySelector('.infolayer g.legend rect.bg');
    var legendWidth = bg ? parseFloat(bg.getAttribute('width')) || 0 : 0;
    var cb = gd.querySelector('.infolayer g.colorbar');
    var cbWidth = cb ? cb.getBoundingClientRect().width : 0;
    return Math.max(legendWidth, cbWidth);
  }

  // Room under the plot for the row of well states, which wraps on a narrow
  // plate; plotly sets it 12 px below the plot
  function bottomMargin(gd, spec) {
    var bg = gd.querySelector('.infolayer g.legend2 rect.bg');
    var height = bg ? parseFloat(bg.getAttribute('height')) || 0 : 0;
    return height > 0 ? Math.max(spec.b, height + 12) : spec.b;
  }

  function near(a, b, tol) {
    return Math.abs((a || 0) - (b || 0)) <= tol;
  }

  function fit(gd, spec) {
    var fl = gd._fullLayout;
    if (!fl || !gd.offsetWidth || !gd.offsetHeight || gd._kiwiFitBusy) return;

    var W = fl.width;
    var H = fl.height;
    var key = W + 'x' + H;
    if (gd._kiwiFitKey !== key) {
      gd._kiwiFitKey = key;
      gd._kiwiFitPasses = 0;
    }
    if (gd._kiwiFitPasses >= MAX_PASSES) return;

    var right = rightWidth(gd);
    // The legend and colorbar sit a little off the plot (x = 1.02)
    var gap = right > 0 ? 14 : 4;
    var marginB = bottomMargin(gd, spec);
    var availW = W - spec.l - right - gap;
    var availH = H - spec.t - marginB;
    if (availW <= 0 || availH <= 0) return;

    var tile = Math.min(availW / spec.cols, availH / spec.rows);
    var plateW = tile * spec.cols;
    var plateH = tile * spec.rows;
    var marginL = spec.l + (availW - plateW) / 2;
    var marginR = W - marginL - plateW;
    var yd0 = Math.max(0, 1 - plateH / availH);

    var layout = {};
    if (!near(fl.margin.l, marginL, 0.5)) layout['margin.l'] = marginL;
    if (!near(fl.margin.r, marginR, 0.5)) layout['margin.r'] = marginR;
    if (!near(fl.margin.b, marginB, 0.5)) layout['margin.b'] = marginB;
    if (!near(fl.xaxis.domain[0], 0, 0.002) || !near(fl.xaxis.domain[1], 1, 0.002)) {
      layout['xaxis.domain'] = [0, 1];
    }
    if (!near(fl.yaxis.domain[0], yd0, 0.002) || !near(fl.yaxis.domain[1], 1, 0.002)) {
      layout['yaxis.domain'] = [yd0, 1];
    }
    // The domains are set here now: plotly's own constraint would shift them
    if (gd.layout.yaxis && gd.layout.yaxis.scaleanchor) {
      layout['yaxis.scaleanchor'] = null;
      layout['xaxis.constrain'] = null;
      layout['yaxis.constrain'] = null;
    }
    if (fl.legend2 && !near(fl.legend2.y, yd0, 0.002)) layout['legend2.y'] = yd0;

    // Colorbar as long as the plate; tile labels sized to the tiles
    var cbIdx = [];
    var textIdx = [];
    var cbLen = plateH / availH;
    var labelSize = Math.max(7, Math.min(12, Math.round(tile * 0.45)));
    gd._fullData.forEach(function (t) {
      if (t.showscale && t.colorbar && !near(t.colorbar.len, cbLen, 0.002)) {
        cbIdx.push(t.index);
      }
      if (t.texttemplate && t.textfont && t.textfont.size !== labelSize) {
        textIdx.push(t.index);
      }
    });

    if (!Object.keys(layout).length && !cbIdx.length && !textIdx.length) return;

    gd._kiwiFitPasses += 1;
    gd._kiwiFitBusy = true;
    var done = function () {
      gd._kiwiFitBusy = false;
    };
    var steps = Promise.resolve();
    if (cbIdx.length) {
      steps = steps.then(function () {
        return Plotly.restyle(gd, { 'colorbar.len': cbLen, 'colorbar.y': 1, 'colorbar.yanchor': 'top' }, cbIdx);
      });
    }
    if (textIdx.length) {
      steps = steps.then(function () {
        return Plotly.restyle(gd, { 'textfont.size': labelSize }, textIdx);
      });
    }
    if (Object.keys(layout).length) {
      steps = steps.then(function () {
        return Plotly.relayout(gd, layout);
      });
    }
    steps.then(done, done).then(function () {
      // The new layout may move the legend's width (a scrollbar): check again
      fit(gd, spec);
    });
  }

  window.kiwiMS_fitPlate = function (el, spec) {
    if (!el || typeof el.on !== 'function') return;
    if (el._kiwiFitHandler && el.removeListener) {
      el.removeListener('plotly_afterplot', el._kiwiFitHandler);
    }
    el._kiwiFitKey = null;
    el._kiwiFitHandler = function () {
      fit(el, spec);
    };
    el.on('plotly_afterplot', el._kiwiFitHandler);
    fit(el, spec);
  };
})();
