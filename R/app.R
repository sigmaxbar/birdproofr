#'
#'Runs birdproofr Shiny app
#'@export
#'
run_birdproofr_app <- function() {
  require("dplyr")
  require("shiny")
  require("shinycssloaders")
  require("shinythemes")
  require("shinyWidgets")

  ui <- navbarPage(theme = shinytheme("sandstone"),
    title = "birdproofr 1.0.2",
    tabPanel("Data",
      sidebarLayout(
        sidebarPanel(
          h4("Upload Data"),
          helpText("Select a songbird banding data file."),
          fileInput("csvInput", "Upload CSV File", accept=c("text/csv","text/comma-separated-values",".csv"))
        ),
        mainPanel(
          tableOutput("data")
        )
      )#sidebarLayout
    ),
    tabPanel("Flags",
             sidebarPanel(
               h4("Filter Flags"),
               helpText("Selecting a few flags at a time is recommended to reduce lag."),
               textOutput("numFlags"),
               uiOutput("filter"),
               h4("Download All Flags"),
               downloadButton("download", "Download")
             ),
             mainPanel(
               tableOutput("flags")
             ))

  ) #ui

  server <- function(input, output) {

    get_data <- reactive({
      if(is.null(input$csvInput)) {
        return()
      }
      filestr <- input$csvInput
      suppressWarnings(clean_df(read.csv(filestr$datapath, na.strings=c("", " "), header = TRUE)))
    })

    output$data <- renderTable(
      head(get_data(), 100)
    )

    get_flags <- reactive({
      req(get_data)

      in_df <- get_data()

      validate_all(in_df)
    })

    output$filter <- renderUI({
      req(get_flags)

      flag_list <- as.list(as.character(unique(get_flags()$Issue)))

      pickerInput("filtered_issues", "Select Flag(s)",
                  choices = flag_list,
                  selected = NULL,
                  multiple = TRUE,
                  inline = FALSE, options = list(`actions-box` = TRUE)
      )
    })

    get_filtered_flags <- reactive({
      req(input$filtered_issues)

      get_flags() %>%
        dplyr::select(Issue, everything()) %>%
        dplyr::filter(Issue %in% input$filtered_issues)
    })

    output$flags <- renderTable(
      get_filtered_flags()
    )

    output$numFlags <- renderText(
      paste0(nrow(get_flags()), " potential issues detected.")
    )

    output$download <- downloadHandler(
        filename = function() {
          if(is.null(input$csvInput)) {
            showModal(modalDialog(
              title = "File Not Found",
              paste0("Please upload a CSV file before validating."),
              easyClose = TRUE,
              footer = NULL
            ))
          }
          else {
            filestr <- input$csvInput
            paste(tools::file_path_sans_ext(filestr), "-flags", ".csv", sep="")
          }
        },
        content = function(file) {
          if(is.null(input$csvInput)) {
            showModal(modalDialog(
              title = "File Not Found",
              paste0("Please upload a CSV file before validating."),
              easyClose = TRUE,
              footer = NULL
            ))
          }
          else {
            out_df <- get_flags()
            out_df <- out_df[order(out_df$Original.Order.on.hard.copy),]
            write.csv(out_df, file)
          }
        } #content
    ) #downloadHandler


  } #server

  shinyApp(ui = ui, server = server)
} #app

