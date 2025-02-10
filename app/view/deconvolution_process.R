box::use(
  shiny,
  shinyjs[enable, disable, disabled, runjs],
  shinyWidgets[progressBar, updateProgressBar],
  future[future, plan, multisession],
  promises[then, catch],
  fs[dir_ls],
)

# Import the deconvolution function
box::use(
  app/logic/deconvolution_functions[deconvolute],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  shiny$fluidRow(
    shiny$column(
      width = 12,
      shiny$fluidRow(
        shiny$column(
          width = 3,
          shiny$uiOutput(ns("deconvolute_start_ui"))
        ),
        shiny$column(
          width = 9,
          disabled(
            progressBar(
              id = ns("progressBar"),
              value = 0,
              total = 100,
              title = "Processing Files",
              display_pct = TRUE
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, waters_dir) {
  # Set up parallel processing
  plan(multisession)
  
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_dir <- shiny$reactive({waters_dir()})
    
    output$deconvolute_start_ui <- shiny$renderUI({
      shiny$validate(
        shiny$need(dir.exists(raw_dir()), "No Waters .raw selected"))
      shiny$actionButton(ns("deconvolute_start"), "Run Deconvolution")
    })
    
    values <- shiny$reactiveValues(
      isRunning = FALSE,
      completedFiles = 0,
      expectedFiles = 0,
      initialFileCount = 0,
      lastCheck = 0
    )
    
    checkProgress <- function(dir_path) {
      message("Checking progress at: ", Sys.time())
      files <- dir_ls(dir_path, glob = "*_rawdata.txt")
      count <- length(files)
      message("Found files: ", count)
      count
    }
    
    reset_progress <- function() {
      values$isRunning <- TRUE
      values$completedFiles <- 0
      values$lastCheck <- Sys.time()
      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Starting Process..."
      )
      output$progress <- shiny$renderText("Initializing...")
      output$status <- shiny$renderText("")
    }
    
    progress_observer <- NULL
    
    shiny$observeEvent(input$deconvolute_start, {
      disable(ns("deconvolute_start"))
      runjs('document.getElementById("blocking-overlay").style.display = "block";')
      
      reset_progress()
      
      raw_dirs <- list.dirs(raw_dir(), full.names = TRUE, recursive = FALSE)
      raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]
      
      if (length(raw_dirs) == 0) {
        shiny$showNotification(
          paste0("No .raw directories found in ", raw_dir()),
          type = "error",
          duration = NULL
        )
        enable(ns("deconvolute_start"))
        runjs('document.getElementById("blocking-overlay").style.display = "none";')
        return()
      }
      
      message(sprintf("Found %d .raw directories to process", length(raw_dirs)))
      shiny$showNotification(
        sprintf("Found %d .raw directories to process", length(raw_dirs)),
        type = "message", duration = NULL)
      
      values$expectedFiles <- length(raw_dirs)
      values$initialFileCount <- checkProgress(raw_dir())
      message("Initial file count: ", values$initialFileCount)
      
      if (!is.null(progress_observer)) {
        progress_observer$destroy()
      }
      
    enable(ns("progressBar"))
      
      # Start deconvolution
      future({
        message("Starting deconvolution in parallel session")
      deconvolute(raw_dirs)
      }) |>
        then(
          onFulfilled = function(value) {
            message("Future completed successfully")
            values$isRunning <- FALSE
            progress_observer$destroy()

            updateProgressBar(
              session = session,
              id = ns("progressBar"),
              value = 100,
              title = "Processing Complete!"
            )

            output$status <- shiny$renderText("Process completed successfully!")
            enable(ns("deconvolute_start"))
            runjs('document.getElementById("blocking-overlay").style.display = "none";')
          }
        ) |>
        catch(
          function(error) {
            message("Future failed with error: ", error$message)
            values$isRunning <- FALSE
            progress_observer$destroy()

            updateProgressBar(
              session = session,
              id = ns("progressBar"),
              value = 0,
              title = "Process Failed!"
            )

            output$status <- shiny$renderText(
              paste("Process failed!", error$message)
            )
            enable(ns("deconvolute_start"))
            runjs('document.getElementById("blocking-overlay").style.display = "none";')
          }
        )
        
      progress_observer <- shiny$observe({
          shiny$req(values$isRunning)
          shiny$invalidateLater(500)
          
          if (difftime(Sys.time(), values$lastCheck, units = "secs") >= 0.5) {
            current_total_files <- checkProgress(raw_dir())
            values$completedFiles <- current_total_files - values$initialFileCount
            values$lastCheck <- Sys.time()
            
            progress_pct <- min(100, round(
              100 * values$completedFiles / values$expectedFiles))
            
            message("Updating progress: ", progress_pct, "% (", 
                    values$completedFiles, "/", values$expectedFiles, ")")
            
            updateProgressBar(
              session = session,
              id = ns("progressBar"),
              value = progress_pct,
              title = sprintf("Processing Files (%d/%d)", 
                              values$completedFiles, values$expectedFiles)
            )
          }
        })
    })
  })
}