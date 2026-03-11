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
  "Theor. Prot.",
  "Meas. Prot.",
  "Δ Prot.",
  "Ⅰ Prot.",
  "Peak Signal",
  "Ⅰ Cmp",
  "Theor. Cmp",
  "Δ Cmp",
  "Bind. Stoich.",
  "%-Binding",
  "Total %-Binding"
)

# Sequential color scales
#' @export
sequential_scales <- list(
  "YlOrRd",
  "YlOrBr",
  "YlGnBu",
  "YlGn",
  "RdPu",
  "PuRd",
  "PuBuGn",
  "PuBu",
  "OrRd",
  "GnBu",
  "BuPu",
  "BuGn",
  "Purples",
  "Reds",
  "Oranges",
  "Greys",
  "Greens",
  "Blues"
)

# Qualitative color scales
#' @export
qualitative_scales <- list(
  "Set1",
  "Set2",
  "Set3",
  "Pastel1",
  "Pastel2",
  "Dark2",
  "Accent"
)

# Gradient color scales
#' @export
gradient_scales <- list(
  "Magma" = "magma",
  "Inferno" = "inferno",
  "Plasma" = "plasma",
  "Viridis" = "viridis",
  "Cividis" = "cividis",
  "Rocket" = "rocket",
  "Mako" = "mako",
  "Turbo" = "turbo"
)

# Warning symbol
#' @export
warning_sym <- "\u26A0"

# Custom symbols for plotly plots
#' @export
symbols <- c(
  "circle",
  "triangle-up",
  "square",
  "star-triangle-down",
  "square-x-open",
  "asterisk-open",
  "diamond",
  "triangle-down",
  "square",
  "x",
  "hexagram",
  "hourglass"
)

# JS renderer for bar charts in tables
#' @export
chart_js <- '
function(data, type, row, meta) {
  if (type === "display") {
    var val = parseFloat(data);
    var width = isNaN(val) ? 0 : val;

    return "<div class=\'bar-chart-bar\'>" +
             "<div class=\'bar\' style=\'width: " + width + "%;\'></div>" +
           "</div>";
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
