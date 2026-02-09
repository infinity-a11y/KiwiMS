mass <- result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$mass
mass$intensity <- (mass$intensity - min(mass$intensity)) /
  (max(mass$intensity) - min(mass$intensity)) *
  100

peaks <- c(
  unique(
    result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$hits$`Measured Mw Protein [Da]`
  ),
  result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$hits$`Peak [Da]`
)

hits <- which(mass$mass %in% peaks)
peak_df <- mass[hits, ]

name <- c(
  unique(
    result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$hits$Protein
  ),
  result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$hits$Compound
)

mw <- c(
  unique(
    result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$hits$`Mw Protein [Da]`
  ),
  result_hits$`2025-08-12_RACA+P1-10_20250731_50_1h_01.raw`$hits$`Compound Mw [Da]`
)

peak_df <- cbind(peak_df, name, mw)

plotly::plot_ly(
  mass,
  x = ~mass,
  y = ~intensity,
  type = "scattergl",
  mode = "lines",
  color = I("black"),
  hoverinfo = "text",
  text = ~ paste0("Mass: ", mass, " Da\nIntensity: ", round(intensity, 2), "%")
) |>
  plotly::add_markers(
    data = peak_df,
    x = ~mass,
    y = ~intensity,
    marker = list(
      color = "#e8cb97",
      line = list(
        color = "#35357A",
        width = 2
      ),
      symbol = "circle",
      size = 10,
      zindex = 100
    ),
    hoverinfo = "text",
    text = ~ paste0(
      "Name: ",
      name,
      "\nMeasured: ",
      mass,
      " Da\nIntensity: ",
      round(intensity, 2),
      "%\n",
      "Theor. Mw: ",
      mw
    ),
    showlegend = FALSE
  ) |>
  plotly::layout(
    yaxis = list(
      title = "Intensity [%]",
      showgrid = TRUE,
      zeroline = FALSE,
      ticks = "outside",
      tickcolor = "transparent"
    ),
    xaxis = list(title = "Mass [Da]", showgrid = TRUE, zeroline = FALSE),
    margin = list(t = 0, r = 0, b = 0, l = 50),
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  ) |>
  plotly::config(
    displayModeBar = "hover",
    scrollZoom = FALSE,
    modeBarButtons = list(list(
      "zoom2d",
      "toImage",
      "autoScale2d",
      "resetScale2d",
      "zoomIn2d",
      "zoomOut2d"
    ))
    # ,
    # toImageButtonOptions = list(
    #   filename = paste0(
    #     Sys.Date(),
    #     "_",
    #     gsub("_rawdata", "", base),
    #     "_deconvoluted"
    #   )
    # )
  )
