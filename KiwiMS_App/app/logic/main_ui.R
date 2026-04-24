# app/logic/main_ui.R
# Static / near-static modal body UI builders for app/main.R

box::use(
  shiny,
)

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
        paste(readLines("LICENSE"), collapse = "\n")
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
