library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(hotr)
library(visNetwork)
library(tidyverse)
library(rio)
library(htmlwidgets)

# ya subí unos datos; quiero ver una cosa de los de muestra, los escojo y entonces pierdo los míos
# input$colorpalette devuelve el hex sin numeral



#' @export
tableInputUI0 <- function(id,
                          choices = c("pasted", "fileUpload", "sampleData", "googleSheets"),
                          choicesInline = FALSE,
                          selected = "pasted", ...) {
  # UI
  ns <- shiny::NS(id)
  #choiceNames <-  choiceNames %||% choices
  #names(choices) <- choiceNames
  
  #info_style <- ifelse(is.null(uiOutput(ns("tableInputInfo"))), "display:flex;", "display:none;")
  
  shiny::tagList(shiny::div(id = ns("tableInput"),class="tableInput",
                            shiny::radioButtons(ns("tableInput"), "",
                                                choices = choices, selected = selected, inline = choicesInline),
                            shiny::uiOutput(ns("tableInputControls"))),
                 shiny::div(class = "box-tableInputInfo", #style = info_style,
                            shiny::uiOutput(ns("tableInputInfo"))))
  
}

#' @export
tableInput0 <- function(input, output, session,
                        infoList = NULL,
                        pasteLabel = "Paste", pasteValue = "", pastePlaceholder = "Select your data and paste it here", pasteRows = 5,
                        uploadLabel = "Choose CSV File", uploadButtonLabel = "Browse...", uploadPlaceholder = "No file selected",
                        sampleLabel = "Select a sample data", sampleFiles = NULL, sampleSelected = NULL,
                        googleSheetLabel = "Data from Google Sheet", googleSheetValue = "", googleSheetPlaceholder = "https://docs.google.com/spreadsheets/...",
                        googleSheetPageLabel = "Sheet",
                        ...) {
  
  output$tableInputControls <- shiny::renderUI({
    
    # str(session)
    # if(!exists(session))
    #   stop("No session defined in server.")
    
    ns <- session$ns
    
    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()
    
    if(input$tableInput == "sampleData"){
      if(!all(map_lgl(sampleFiles,file.exists)))
        stop("All Sample Files must exist")
    }
    
    tableInputControls <- list(pasted = textAreaInput(ns("inputDataPasted"), label = pasteLabel, value = pasteValue, placeholder = pastePlaceholder, rows = pasteRows),
                               fileUpload =  fileInput(ns("inputDataUpload"), uploadLabel, buttonLabel = uploadButtonLabel, placeholder = uploadPlaceholder,
                                                       accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv", ".xls", ".xlsx")),
                               sampleData = selectInput(ns("inputDataSample"), sampleLabel, choices = sampleFiles, selected = sampleSelected),
                               googleSheets = list(shiny::textInput(ns("inputDataSheet"), label = googleSheetLabel, value = googleSheetValue, placeholder = googleSheetPlaceholder),
                                                   shiny::numericInput(ns("inputDataGoogleSheetSheet"), googleSheetPageLabel, 1))
    )
    tableInputControls[[input$tableInput]]
  })
  
  output$tableInputInfo <- shiny::renderUI({
    ns <- session$ns
    tableInputInfo <- infoList[[input$tableInput]]
    if (is.null(tableInputInfo)) return()
    tableInputInfo
  })
  
  inputData <- shiny::reactive({
    inputType <- input$tableInput
    #readDataFromInputType(inputType)
    if(inputType == "pasted"){
      if (is.null(input$inputDataPasted)) return()
      if(input$inputDataPasted == "")
        return()
      df <- tryCatch(read_tsv(input$inputDataPasted), error = function(e) e)
    }
    if(inputType ==  "fileUpload"){
      if(is.null(input$inputDataUpload)) return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(),input$inputDataUpload$name)
      file.copy(old_path,path)
      df <- rio::import(path)
    }
    if(inputType ==  "sampleData"){
      if (is.null(input$inputDataSample)) return()
      file <- as.character(input$inputDataSample)
      df <- readr::read_csv(file)
    }
    if (inputType == "googleSheets") {
      if (is.null(input$inputDataSheet)) return()
      if (input$inputDataSheet == "") return()
      library(googlesheets4)
      googlesheets4::sheets_deauth()
      id_file <- gsub(".*\\/d\\/|\\/edit.*", '', input$inputDataSheet)
      googlesheets4::sheets_get(id_file)
      df <- googlesheets4::read_sheet(id_file)
    }
    return(df)
  })
  
  inputData
}







ui <- panelsPage(useShi18ny(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       # body = tabsetPanel(id = "data_preview",
                       #                    tabPanel("connections",
                       #                             uiOutput("connections_preview")),
                       #                    tabPanel("nodes",
                       #                             uiOutput("nodes_preview")),
                       #                    type = "pills")),
                       
                       body = div(radioButtons("tab", "", c("connections", "nodes")),
                                  uiOutput("data_preview"))),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  visNetworkOutput("result", height = "80vh"),
                                  shinypanels::modal(id = "download",
                                                     title = ui_("download_net"),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download network", modal_id = "download")))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  data_input <- reactiveValues(cn = NULL,
                               nd = NULL)
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI0("initial_data",
                  choices = choices,
                  # selected is important for inputs not be re-initialized
                  selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    # req(input$data_preview)
    if (input$tab == "connections") {
      sm_f <- i_(c("sample_ch_cn_0", "sample_ch_cn_1"), lang())
    } else {
      sm_f <- i_(c("sample_ch_nd_0", "sample_ch_nd_1"), lang())
    }
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFile = sm_f,
         
         pasteLabel = i_("paste", lang()),
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5, 
         
         uploadLabel = i_("upload_lb", lang()), 
         uploadButtonLabel = i_("upload_bt_lb", lang()),
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()), 
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang()))
  })
  
  # observeEvent(list(labels(), input$`initial_data-tableInput`), {
  inputData <- eventReactive(list(labels(), input$`initial_data-tableInput`), {
    do.call(callModule, c(tableInput0, "initial_data", labels()))
  })
  
  # observeEvent(list(labels(), input$`initial_data-tableInput`), {
  output$data_preview <- renderUI({
    if (input$tab == "connections") {
      # data_input$con_up <- do.call(callModule, c(tableInput, "initial_data", labels()))()
      # if (!is.null(f0()))
      #   data_input$con <- f0()
      uiOutput("connections_preview")
    } else {
      # f0 <- do.call(callModule, c(tableInput, "initial_data", labels()))
      # if (!is.null(f0()))
      #   data_input$nod <- f0()
      # data_input$con <- f0
      uiOutput("nodes_preview")
    }
  })
  
  observe({
    req(input$`initial_data-tableInput`)
    d0 <- inputData()()
    if (!is.null(d0)) {
      if (input$tab == "connections") {
        if (!identical(d0, data_input$nd)) {
          data_input$cn <- d0
        }
      } else {
        if (!identical(d0, data_input$cn)) {
          data_input$nd <- d0
        }  
      }
    }
  })
  
  # observe({
  #   req(input$`initial_data-tableInput`)
  #   if (input$tab == "connections") {
  #     # f0 <- do.call(callModule, c(tableInput, "initial_data", labels()))
  #     # print(f0())
  #     # if (!is.null(f0())) {
  #       data_input$con <- do.call(callModule, c(tableInput, "initial_data", labels()))
  #     # }
  #       
  #   } else {
  #     f0 <- do.call(callModule, c(tableInput, "initial_data", labels()))
  #     if (!is.null(f0())) 
  #       data_input$nod <- f0
  #     # data_input$con <- f0
  #   }
  # })
  
  output$connections_preview <- renderUI({
    # req(data_input$con)
    suppressWarnings(hotr("hotr_con_input", data = data_input$cn, order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  output$nodes_preview <- renderUI({
    # req(data_input())
    suppressWarnings(hotr("hotr_nod_input", data = data_input$nd, order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  observeEvent(lang(), {
    ch0 <- as.character(parmesan$nodes$inputs[[1]]$input_params$choices)
    names(ch0) <- i_(ch0, lang())
    # ch1 <- as.character(parmesan$size$inputs[[3]]$input_params$choices)
    # names(ch1) <- i_(ch1, lang())
    updateSelectizeInput(session, "nd_shape", choices = ch0, selected = input$nd_shape)
    # updateRadioButtons(session, "full_width", choices = ch1, selected = input$full_width)
  })
  
  cols_cn <- reactive({
    c0 <- c("no", names(data_input$cn))
    names(c0) <- c(i_("no_lb", lang()), names(data_input$cn))
    c0
  })
  
  cols_nd <- reactive({
    c0 <- c("no", names(data_input$nd))
    names(c0) <- c(i_("no_lb", lang()), names(data_input$nd))
    c0
  })
  
  observe({
    print(input$nd_lb)
  })
  
  
  
  ntwrk <- reactive({
    # req(data_input$cn, data_input$nd)
    # sl <- NULL
    # if (sum(input$selection) > 0) 
    #   sl <- "multiple"
    # 
    # st <- paste0("color: #", input$color, "; font-family: ", input$font_family, "; font-size: ", input$font_size, "px;")# font-weight: bold;")
    
    # ¿Proxy?
    nodes <- data.frame(id = 1:3, label = LETTERS[1:3])
    edges <- data.frame(from = c(1,2), to = c(1,3))
      
    # visNetwork(nodes = data_input$nd, edges = data_input$cn) %>%
    v0 <- visNetwork(nodes, 
               edges,
               main = input$title) %>%
      visNodes(shape = input$nd_shape,
               size = input$nd_size,
               borderWidth = input$nd_border,
               color = list(background = paste0("#", input$nd_color),
                            border = paste0("#", input$nd_border_color)),
               font = list(color = paste0("#", input$nd_lb_color),
                           size = input$nd_lb_size),
               shadow = input$nd_shadow) %>%
      visEdges(arrows = input$ed_arrows,
               smooth = input$ed_smooth,
               width = input$ed_size, 
               color = list(color = paste0("#", input$ed_color)),
               font = list(color = paste0("#", input$nd_lb_color),
                           size = input$nd_lb_size),
               shadow = input$ed_shadow) %>%
      visInteraction(dragNodes = input$drag_nodes, 
                     dragView = input$drag_view, 
                     zoomView = input$zoom)
    if (input$layout == "hierarchical") {
      v0 <- v0 %>%
        visHierarchicalLayout()
    } else if (input$layout == "igraph") {
      v0 <- v0 %>%
        visIgraphLayout()
    } else if (input$layout == "circle") {
      v0 <- v0 %>%
        visIgraphLayout("layout_in_circle")
    }
    v0
  })
  
  # renderizando reactable
  output$result <- renderVisNetwork({
    # session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadHtmlwidget"))
    req(ntwrk())
    ntwrk()
  })
  
  output$modal <- renderUI({
    dw <- i_("download", lang())#Download HTML
    downloadHtmlwidgetUI("download_data_button", paste(dw, "HTML"))
  })
  
  # descargas
  callModule(downloadHtmlwidget, "download_data_button", widget = reactive(ntwrk()), name = "network")
  
  
  
  
  
}



shinyApp(ui, server)




