# app/logic/main_ui.R
# Static / near-static modal body UI builders for app/main.R

box::use(
  shiny,
)

# GPL-3 requires KiwiMS to make its license text available to users of the
# app, not just to whoever reads the repo - this is what backs the "License"
# nav entry. The installer copies LICENSE next to the app (setup_script.iss,
# [Files]) and launch.ps1 runs R with that directory as its working directory,
# so an installed session always finds it. A missing file here therefore means
# either a dev checkout running without KIWIMS_DEV_MODE set (LICENSE lives only
# at the repo root, one level above KiwiMS_App) or a damaged install - read
# defensively either way instead of letting readLines() throw a raw warning
# into every session's log, and never leave the notice blank: falling back to
# the canonical GPL text is itself part of satisfying the requirement.
license_text <- function(path = "LICENSE") {
  fallback <- paste(
    "LICENSE file not found in the installation folder.",
    "KiwiMS is distributed under the GNU General Public License v3.",
    "The full license text is available at",
    "https://www.gnu.org/licenses/gpl-3.0.txt",
    "and in the project repository at",
    "https://github.com/infinity-a11y/KiwiMS/blob/master/LICENSE",
    sep = "\n"
  )

  if (!file.exists(path)) {
    return(fallback)
  }

  # file()'s open step warns before readLines() errors on an unreadable path
  # (permissions, or a directory where a file was expected) - catch both, the
  # same gap noted in user_settings.R's readRDS() guard, or the warning still
  # reaches the log even though the fallback text is correctly returned.
  tryCatch(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    warning = function(w) fallback,
    error = function(e) fallback
  )
}

#' @export
licence_modal_body <- function() {
  shiny$div(
    style = "font-size: 14px;",
    shiny$tags$p("KiwiMS is released under the following license:"),
    shiny$tags$pre(
      style = paste0(
        "height: 400px; overflow-y: scroll; background-color: #f8f9fa;",
        " font-size: 11px; padding: 30px; border: 1px solid #ddd;",
        " width: fit-content; margin: 0; justify-self: center;"
      ),
      if (Sys.getenv("KIWIMS_DEV_MODE") != "TRUE") {
        license_text()
      }
    )
  )
}

#' @export
unidec_modal_body <- function() {
  shiny$div(
    style = "font-size: 14px;",
    shiny$HTML(
      '
<p>The deconvolution and peak picking algorithms within this software are powered by
<b>UniDec</b> - Universal Deconvolution of Mass and Ion Mobility Spectra
(<a href="https://github.com/michaelmarty/UniDec" target="_blank">github.com/michaelmarty/UniDec</a>).</p>

<p>We gratefully acknowledge the work of <b>Marty et al.</b> in developing these
Bayesian deconvolution methods.</p>

<hr style="margin: 1rem 0;">

<h5 style="color: #2c3e50;">Citation Request</h5>
<p>If you utilize the deconvolution or peak picking results from this software in
your research or publications, the authors of UniDec request that you cite their original paper:</p>

<div style="background-color: #f8f9fa; padding: 15px; border-left: 5px solid #007bff; margin-bottom: 10px;">
  M. T. Marty, A. J. Baldwin, E. G. Marklund, G. K. A. Hochberg, J. L. P. Benesch, C. V. Robinson.
  <br><b>"UniDec: Universal Deconvolution of Mass and Ion Mobility Spectra."</b>
  <br><i>Anal. Chem.</i> 2015, 87, 4370-4376.
</div>
'
    ),
    shiny$div(
      shiny$tags$textarea(
        id = "bibtex_unidec",
        readonly = "readonly",
        style = paste0(
          "width: 100%; height: 140px; font-family: monospace; font-size: 12px;",
          " background-color: #f4f4f4; padding: 10px;",
          " resize: none; border: 1px solid #ccc; border-radius: 4px;"
        ),
        "@article{Marty2015UniDec,
  author = {Marty, Michael T. and Baldwin, Andrew J. and Marklund, Erik G. and Hochberg, Georg K. A. and Benesch, Justin L. P. and Robinson, Carol V.},
  title = {UniDec: Universal Deconvolution of Mass and Ion Mobility Spectra},
  journal = {Analytical Chemistry},
  volume = {87},
  number = {8},
  pages = {4370-4376},
  year = {2015},
  doi = {10.1021/acs.analchem.5b00140}
}"
      ),
      shiny$tags$button(
        "Copy",
        id = "copy_btn",
        class = "btn btn-default btn-sm",
        style = "position: absolute; bottom: 40px; right: 2.5rem; z-index: 10; opacity: 0.8;",
        onclick = "
var textArea = document.getElementById('bibtex_unidec');
textArea.select();
document.execCommand('copy');
var btn = document.getElementById('copy_btn');
btn.innerHTML = 'Copied!';
setTimeout(function(){ btn.innerHTML = 'Copy'; }, 2000);
"
      )
    )
  )
}

#' @export
update_modal_body <- function(local_version, release, message, link, hint) {
  shiny$fluidRow(
    shiny$br(),
    shiny$column(
      width = 11,
      shiny$fluidRow(
        shiny$column(width = 6, shiny$p("Current Version")),
        shiny$column(width = 6, shiny$p(local_version, style = "font-style: italic"))
      ),
      shiny$fluidRow(
        shiny$column(width = 6, shiny$p("Release date")),
        shiny$column(width = 6, shiny$p(release, style = "font-style: italic"))
      ),
      shiny$br(),
      shiny$fluidRow(
        shiny$column(
          width = 12,
          shiny$h6(message, style = "font-weight: bold"),
          shiny$p(shiny$HTML(hint), style = "font-style: italic; margin-top: 1rem;"),
          shiny$tags$a(href = link, link, target = "_blank")
        )
      )
    )
  )
}
