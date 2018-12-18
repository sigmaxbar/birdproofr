ui <- fluidPage(
  titlePanel("BirdProofR"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csvInput", "CSV Input"),
      #selectInput("birdInput", "Bird", choices = c("Songbird", "Hummingbird")),
      actionButton("validateButton", "Validate!")
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
    in_df <- read.csv(filestr$datapath)
    colnames(in_df)[colnames(in_df) == "Original.Order.on.hard.copy"] <- "Order"
    colnames(in_df)[colnames(in_df) == "proofing.and.data.entry.notes"] <- "Issue"
    out_df <- data.frame()

    ### Validate species column

    valid_species_list <- c("amgo", "amke", "amre", "amro", "auwa", "bade", "balo", "bcch", "bewr", "bggn",
                          "bhco", "bhgr", "brcr", "brsp", "btyw", "buor", "bush", "cafi", "cahu", "canw", "caqu", "cavi", "cbch",
                          "cedw", "chsp", "coha", "coni", "copo", "deju", "dowo", "dufl", "evgr", "flow", "fosp", "gcki", "gcsp",
                          "grca", "grfl", "gtto", "gwcs", "hafl", "hawo", "heth", "hewa", "hofi", "howr", "lazb", "lefl", "lego",
                          "lisp", "mgwa", "moch", "mwcs", "mywa", "nawa", "nofl", "nopo", "ocwa", "orju", "osfl", "pawr", "pisi",
                          "rbnu", "rcki", "recr", "rnsa", "rowr", "rsfl", "sath", "savs", "sosp", "spto", "ssha", "stja", "swth",
                          "tewa", "toso", "towa", "udej", "uyrw", "vath", "vesp", "wavi", "wbnu", "wcsp", "webl", "wefl", "weta",
                          "wewp", "wifl", "wiwa", "ybch", "yewa", "yrwa")

    species <- tolower(in_df$SPECIES)
    species_issues <- filter(in_df, !(species %in% valid_species_list))
    if(nrow(species_issues) != 0) {
      species_issues[,"Issue"] <- "Species is rare or does not exist"
      species_issues <- subset(species_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, species_issues)
    }

    ### Validate age column

    valid_age_list <- c(0,1,2,3,4,5,6)
    age_issues <- filter(in_df, !(AGE %in% valid_age_list))
    if(nrow(age_issues) != 0) {
      age_issues[,"Issue"] <- "Invalid age. Age must be 0 1 2 4 5 or 6"
      age_issues <- subset(age_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, age_issues)
    }

    ### Validate sex column

    sex_issues <- filter(in_df, !(SEX == "M" | SEX == "F" | SEX == "U"))
    if(nrow(sex_issues) != 0) {
      sex_issues[,"Issue"] <- "Invalid sex. Sex must be M F or U"
      sex_issues <- subset(sex_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, sex_issues)
    }


    ### Check that age and BP/CP match

    bpcp_issues <- filter(in_df, AGE == 0 | AGE == 2 | AGE == 4, BP != 0, CP != 0)
    if(nrow(bpcp_issues) != 0) {
      bpcp_issues[,"Issue"] <- "BP and CP must both be 0 if age = 0 2 or 4."
      bpcp_issues <- subset(bpcp_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, bpcp_issues)
    }

    ### Validate how sexed for females

    fhs_issues <- filter(in_df, SEX == "F", !(HS == "PL" | HS == "BP" | HS == "WL"))
    if(nrow(fhs_issues) != 0) {
      fhs_issues[,"Issue"] <- "Invalid how sexed value. Acceptable hs values for females are PL BP and WL"
      fhs_issues <- subset(fhs_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, fhs_issues)
    }

    ### Validate how sexed and BP for females

    fhs_bp_issues <- filter(in_df, SEX == "F", HS == "BP", BP == 0 | BP == "")
    if(nrow(fhs_bp_issues) != 0) {
      fhs_bp_issues[,"Issue"] <- "If sexed by BP BP value cannot be blank or 0"
      fhs_bp_issues <- subset(fhs_bp_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, fhs_bp_issues)
    }

    ### Validate how sexed for males

    mhs_issues <- filter(in_df, SEX == "M", !(HS == "PL"| HS == "CL"| HS == "WL"))
    if(nrow(mhs_issues) != 0) {
      mhs_issues[,"Issue"] <- "Invalid how sexed value. Acceptable hs values for males are PL CL and WL."
      mhs_issues <- subset(mhs_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, mhs_issues)
    }

    ### Validate how sexed and CP for males

    mhs_cp_issues <- filter(in_df, SEX == "M", HS == "CL", !(CP == 2 | CP == 3))
    if(nrow(mhs_cp_issues) != 0) {
      mhs_cp_issues[,"Issue"] <- "If sexed by CL CP value must be 2 or 3."
      mhs_cp_issues <- subset(mhs_cp_issues, select=c(Order, Issue))
      out_df <- rbind(out_df, mhs_cp_issues)
    }


    # Unknown
    #validUHSList <- c("", "ic")
    #uhs <- tolower(banding$HS[tolower(banding$SEX)=="u"])
    #validUHS <- uhs %in% validUHSList
    #banding$proofing.and.data.entry.notes[!validUHS] <- "Invalid how sexed value. Acceptable hs values for sex = U are blank and IC."



    out_df <- out_df[order(out_df$Order),]
    output$flags <- renderTable(
      out_df
    )


  })

} #server

shinyApp(ui = ui, server = server)
