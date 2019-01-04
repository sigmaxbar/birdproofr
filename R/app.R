#'
#'Runs birdproofr Shiny app
#'@export
#'
run_birdproofr_app <- function() {
  ui <- fluidPage(
    titlePanel("birdproofr (v1.0.1)"),
    sidebarLayout(
      sidebarPanel(
        fileInput("csvInput", "Upload (.csv)", accept=c("text/csv","text/comma-separated-values",".csv")),
        #selectInput("birdInput", "Bird", choices = c("Songbird", "Hummingbird")),
        actionButton("validateButton", "Show Flags"),
        downloadButton("download", "Download Flags (.csv)")
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data", tableOutput("data")),
                    tabPanel("Flagged Issues", tableOutput("flags"))
        ) #tabsetPanel
      ) #mainPanel
    )#sidebarLayout
  ) #ui

  server <- function(input, output) {
    observeEvent(input$csvInput, {
      filestr <- input$csvInput
      output$data <- renderTable(
        read.csv(filestr$datapath)
      )
    }) #observeEvent

    observeEvent(input$validateButton, {
      filestr <- input$csvInput
      if(is.null(filestr)) {
        showModal(modalDialog(
          title = "File Not Found",
          paste0("Please upload a CSV file before validating."),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      else {
        in_df <- read.csv(filestr$datapath, na.strings=c("", " "), header = TRUE)
        colnames(in_df)[colnames(in_df) == "Original.Order.on.hard.copy"] <- "Order"
        colnames(in_df)[colnames(in_df) == "proofing.and.data.entry.notes"] <- "Issue"
        out_df <- data.frame()
  
        ### Validating
  
        to_validate <- validate_all_list(in_df)
  
        for(df in to_validate) {
          if(nrow(df) != 0) {
            df <- subset(df, select=c(Order, Issue))
            out_df <- rbind(out_df, df)
          }
        }
  
        out_df <- out_df[order(out_df$Order),]
        output$flags <- renderTable(
          out_df
        )
      }
    }) #observeEvent
    
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
            filestr <- input$csvInput
            in_df <- read.csv(filestr$datapath, na.strings=c("", " "), header = TRUE)
            colnames(in_df)[colnames(in_df) == "Original.Order.on.hard.copy"] <- "Order"
            colnames(in_df)[colnames(in_df) == "proofing.and.data.entry.notes"] <- "Issue"
            out_df <- data.frame()
            to_validate <- validate_all_list(in_df)
            
            for(df in to_validate) {
              if(nrow(df) != 0) {
                df <- subset(df, select=c(Order, Issue))
                out_df <- rbind(out_df, df)
              }
            }
            
            out_df <- out_df[order(out_df$Order),]
            
            write.csv(out_df, file)
          }
        } #content
    ) #downloadHandler
    

  } #server

  shinyApp(ui = ui, server = server)
} #app

