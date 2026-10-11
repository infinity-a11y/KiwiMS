# app/logic/conversion_constants.R

# Rhandsontable pasting js hook
#' @export
paste_hook_js <- "function(el, x) {
    var hot = this.hot;
    if (hot._pasteHookAttached) return;
    hot._pasteHookAttached = true;
    
    var parts = el.id.split('-');
    var base_id = parts.pop();
    var nsPrefix = parts.join('-') + (parts.length > 0 ? '-' : '');
    
    hot.addHook('beforePaste', function(data, coords) {
      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue(nsPrefix + 'table_paste_instant', {
          timestamp: Date.now(),
          rowCount: data.length,
          colCount: data[0] ? data[0].length : 0
        }, {priority: 'event'});
      }
      return true;
    });
    
    // === FIX GHOSTING: Hide td (no old content/bg visible) ===
    hot.addHook('afterBeginEditing', function(row, col) {
      var td = hot.getCell(row, col);
      if (td) {
        td.style.visibility = 'hidden';  // Hides td completely
      }
      // Optional: Force editor bg white (in case transparent)
      var editor = hot.getActiveEditor();
      if (editor && editor.TEXTAREA) {
        editor.TEXTAREA.style.backgroundColor = 'white';
        editor.TEXTAREA.style.opacity = '1';
      }
    });
    
    // === RESTORE AFTER EDIT ===
    hot.addHook('afterChange', function(changes, source) {
      if (source === 'edit') {
        setTimeout(function() {
          hot.render();  // Re-renders, restores visibility & applies styles
        }, 10);
      }
    });
    
    // Also restore on deselect (e.g., ESC/cancel)
    hot.addHook('afterDeselect', function() {
      setTimeout(function() {
        hot.render();
      }, 10);
    });
  }"

# Include column of the Samples table: values kept true/false, a select-all
# box in the column header (indeterminate while only some samples are
# included) and the rows of left-out samples greyed. Attached once per table
# instance, which outlives re-renders of the widget; the column is looked up
# on use, and read-only state on click, as both can change between renders.
#' @export
sample_include_js <- "function(el, x) {
    var hot = this.hot;
    if (hot._includeHookAttached) return;
    hot._includeHookAttached = true;

    var colOf = function(name) {
      var cols = hot.params && hot.params.rColnames;
      return cols ? cols.indexOf(name) : -1;
    };

    // A checkbox reads true/false only, as does the server; pasted or
    // deleted values are mapped onto them
    var toBool = function(v) {
      if (typeof v === 'boolean') return v;
      var s = v == null ? '' : String(v).trim().toLowerCase();
      return ['', 'false', 'f', '0', 'no', 'null', 'na'].indexOf(s) === -1;
    };
    hot.addHook('beforeChange', function(changes) {
      var col = colOf('Include');
      if (col < 0 || !changes) return;
      for (var i = 0; i < changes.length; i++) {
        if (changes[i] && hot.propToCol(changes[i][1]) === col) {
          changes[i][3] = toBool(changes[i][3]);
        }
      }
    });

    // Rows of left-out samples: greyed, the sample name struck through
    hot.addHook('afterRenderer', function(td, row, col) {
      var inc = colOf('Include');
      var off = inc >= 0 && col !== inc &&
        hot.getDataAtCell(row, inc) === false;
      td.classList.toggle('sample-excluded', off);
      td.classList.toggle(
        'sample-excluded-name',
        off && col === colOf('Sample')
      );
    });

    var includeState = function() {
      var col = colOf('Include');
      var data = col < 0 ? [] : hot.getDataAtCol(col);
      var n = data.length, k = 0;
      for (var i = 0; i < n; i++) if (data[i] !== false) k++;
      return {
        col: col, data: data, n: n, k: k,
        readOnly: n > 0 && !!hot.getCellMeta(0, col).readOnly
      };
    };
    var syncBox = function(box, s) {
      box.checked = s.n > 0 && s.k === s.n;
      box.indeterminate = s.k > 0 && s.k < s.n;
      box.disabled = s.readOnly;
    };
    var syncHeader = function() {
      var s = includeState();
      el.querySelectorAll('input.sample-include-all').forEach(function(box) {
        syncBox(box, s);
      });
    };

    // Handsontable rewrites the header label on every render, so the box is
    // put back each time (once per clone the fixed columns create)
    hot.addHook('afterGetColHeader', function(col, th) {
      if (col < 0 || col !== colOf('Include')) return;
      var label = th.querySelector('.colHeader');
      if (!label) return;
      label.textContent = '';
      var box = document.createElement('input');
      box.type = 'checkbox';
      box.className = 'sample-include-all';
      box.title = 'Include all samples in the conversion';
      box.setAttribute('aria-label', box.title);
      label.appendChild(box);
      syncBox(box, includeState());
    });
    hot.addHook('afterChange', function() { syncHeader(); });

    // The box sits in a header cell: keep the click from selecting the
    // column, and set every sample from it. Partly or not included turns all
    // samples on, all included turns them off.
    el.addEventListener('mousedown', function(e) {
      if (e.target.classList.contains('sample-include-all')) {
        e.stopPropagation();
      }
    }, true);
    el.addEventListener('click', function(e) {
      if (!e.target.classList.contains('sample-include-all')) return;
      e.stopPropagation();
      e.preventDefault();
      var s = includeState();
      if (s.col >= 0 && s.n > 0 && !s.readOnly) {
        var value = s.k < s.n;
        var changes = [];
        for (var r = 0; r < s.n; r++) {
          if ((s.data[r] !== false) !== value) changes.push([r, s.col, value]);
        }
        if (changes.length) hot.setDataAtCell(changes);
      }
      // After the click's own toggle of the box has been undone
      setTimeout(syncHeader, 0);
    }, true);
  }"

# Popover JS auto-close
#' @export
popover_autoclose <- shiny::HTML(
  "
      $(document).ready(function () {
        $('body').on('click', function (e) {
          $('[data-bs-toggle=\"popover\"]').each(function () {
            // Check if click is NOT on the trigger AND NOT inside any popover
            if (!$(this).is(e.target) && 
                $(this).has(e.target).length === 0 && 
                $('.popover').has(e.target).length === 0) {
              $(this).popover('hide');
            }
          });
        });
      });
      "
)

# Hits table variable names
#' @export
hits_table_names <- c(
  "Well",
  "Theor. Prot. [Da]",
  "Meas. Prot. [Da]",
  "Δ Prot. [Da]",
  "Int. Prot. [%]",
  "Peak Signal [Da]",
  "Int. Cmp [%]",
  "Theor. Cmp [Da]",
  "Δ Cmp [Da]",
  "Bind. Stoich.",
  "Binding [%]",
  "Tot. Binding [%]"
)

# Full column name tooltips for hits table headers (abbreviated -> full)
#' @export
hits_col_full_names <- c(
  "Well"             = "Well",
  "Sample ID"        = "Sample ID",
  "Protein"          = "Protein",
  "Cmp Name"         = "Compound Name",
  "Theor. Prot. [Da]" = "Theoretical Protein [Da]",
  "Meas. Prot. [Da]" = "Measured Protein [Da]",
  "Δ Prot. [Da]"     = "Δ Protein [Da]",
  "Int. Prot. [%]"   = "Protein Intensity [%]",
  "Peak Signal [Da]" = "Peak Signal [Da]",
  "Int. Cmp [%]"     = "Compound Intensity [%]",
  "Theor. Cmp [Da]"  = "Theoretical Compound [Da]",
  "Δ Cmp [Da]"       = "Δ Compound [Da]",
  "Bind. Stoich."    = "Binding Stoichiometry",
  "Preferred"        = "Preferred Hit",
  "Binding [%]"      = "Binding [%]",
  "Tot. Binding [%]" = "Total Binding [%]",
  "Prot. Binding [%]" = "Proteoform Binding [%]",
  "Unmatched [%]"    = "Unmatched [%]",
  "Correct [%]"      = "Correct [%]",
  "Replicate"        = "Replicate",
  "Conc. [M]"        = "Concentration [M]",
  "Conc. [mM]"       = "Concentration [mM]",
  "Conc. [μM]"       = "Concentration [μM]",
  "Conc. [nM]"       = "Concentration [nM]",
  "Conc. [pM]"       = "Concentration [pM]",
  "Time [s]"         = "Time [s]",
  "Time [min]"       = "Time [min]"
)

# Warning symbol
#' @export
warning_sym <- "\u26A0"

# JS renderer for bar charts in tables
#' @export
chart_js <- '
function(data, type, row, meta) {
  if (type === "display") {
    var val = parseFloat(data);
    var label = isNaN(val) ? "" : val.toFixed(2);
    var width = isNaN(val) ? 0 : val;
    if (val >= 50) {
      return "<div class=\'bar-chart-bar\'>" +
               "<div class=\'bar\' style=\'width:" + width + "%\'>" +
                 "<span class=\'bar-label-inside\'>" + label + "</span>" +
               "</div>" +
             "</div>";
    } else {
      return "<div class=\'bar-chart-bar\'>" +
               "<div class=\'bar\' style=\'width:" + width + "%\'></div>" +
               "<span class=\'bar-label-outside\'>" + label + "</span>" +
             "</div>";
    }
  }
  return data;
}
'

# Empty protein declaration table
#' @export
empty_protein_table <- data.frame(
  name = as.character(rep(NA, 9)),
  rep(list(as.numeric(rep(NA, 9))), 9)
) |>
  stats::setNames(c("Protein", paste("Mass", 1:9)))

# Kinetics fitting settings (see compute_kinact_ki()). A concentration gets a
# plateau of its own when its curve reaches plateau_min_reached of it within
# the measured time; the others share one. Plateaus spreading by more than
# plateau_max_spread percentage points raise a warning. kinact and KI are only
# reported separately when the global fit shows significant curvature
# (curvature_alpha) and KI is determinable: not at its search bound, a relative
# standard error of at most ki_max_rel_se and no further above the highest
# measured concentration than ki_max_over_conc. Otherwise only kinact/KI is
# reported.
#' @export
kinetics_settings <- list(
  curvature_alpha = 0.05,
  plateau_min_reached = 0.7,
  plateau_max_spread = 10,
  ki_max_rel_se = 0.5,
  ki_max_over_conc = 2,
  bootstrap_n = 200,
  bootstrap_min_success = 0.8,
  bootstrap_seed = 20260917L,
  curve_points = 500
)

# Hard caps of a run. A replicate series (R1, R2, ...) is one complete repeat
# of the experiment; every sample of a condition belongs to one series, so a
# condition has at most as many replicates as there are series. Four series
# keep the series apart in the plots (four marker fills). The sample cap holds
# for a deconvolution (the samples already in its database plus the new ones)
# and for the Samples table of the conversion: one 384-well plate.
#' @export
run_limits <- list(
  max_series = 4,
  max_replicates = 4,
  max_samples = 384
)

# Mass shifts that are multiples of each other ----
# One mass shift of a compound that is a whole multiple of another (266 Da =
# 2 x 133 Da), or the same shift twice, adds nothing: the stoichiometry search
# already looks for it, and no spectrum tells the two apart. The Compounds table
# refuses such shifts, and so does the start of a conversion. Compared at the
# precision of a typed mass, for factors up to the highest Max. Stoichiometry
# the app accepts, whatever the current setting: a setting changed after the
# table was saved can't let a multiple through. Shifts that are only close to
# a multiple are left to the preferred assignment rule.
#' @export
shift_multiple_limits <- list(
  precision = 0.01,
  max_factor = 20
)

# Preferred assignment of a peak ----
# A peak can fit one compound on one protein species in more than one way: two
# of its mass shifts at the same stoichiometry, or one shift at a stoichiometry
# matching another at a different one (133 Da x2 = 266 Da x1). Exactly one of
# these readings is preferred; it names the peak in the hits table, the spectra
# and the Mass Shifts card, while the binding of the peak is the same whichever
# reading names it. A rule sorts the readings by its criteria in turn:
#   multiple - lowest stoichiometry first
#   error    - smallest deviation of the peak from the predicted mass first
#   shift    - mass shift declared first (Mass 1 before Mass 2) first
# Every rule ends on the declared shift order, and no two readings of one
# compound on one species share both a shift and a stoichiometry, so exactly one
# reading is left at the top. The first rule is the default.
#' @export
hit_preference_rules <- list(
  stoichiometry = list(
    label = "Lowest stoichiometry",
    keys = c("multiple", "error", "shift")
  ),
  mass_error = list(
    label = "Closest mass",
    keys = c("error", "multiple", "shift")
  ),
  declared = list(
    label = "Declared shift order",
    keys = c("shift", "multiple")
  )
)
