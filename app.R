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
library(igraph)

# dsmodules poner paquetes rio, knitr, pander, en imports
# en parmesan o dsmodules hay un map_lgl de tidyverse
# loaders hablar con crml

# ya subí unos datos; quiero ver una cosa de los de muestra, los escojo y entonces pierdo los míos

# si los datos tienen columnas de title, color, ... los grafica; se podría explicar esto en algún lado
# por ahora sólo se pueden subir connections

# falta tooltip
# colorear por columna
# tomar datos de la tabla (después de que -tal vez- sean editados)
# ¿traducir nombres de columnas?
# tamaño edges y nodes



ui <- panelsPage(useShi18ny(),
                 tags$head(tags$style(HTML("
                 #tab {
                 margin-bottom: 27px;
                 }
                 #tab div.shiny-options-group {
                 display: flex;
                 }
                 #tab div.radio label input + span {
                 border-radius: 0.35rem;
                 cursor: pointer;
                 margin: 6px 2px 6px 0;
                 padding: 10px;
                 }
                 #tab div.radio label input:checked + span {
                 background-color: rgb(195, 113, 155);
                 }
                 #tab input[type='radio'] {
                 display: none;
                 }"))),
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
                                  # dropdownActionInput("dn", "Download", choices = c("png", "jpge", "pdf")),
                                  visNetworkOutput("result", height = "80vh"),
                                  shinypanels::modal(id = "download",
                                                     title = ui_("download_net"),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download network", modal_id = "download")))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  data_input <- reactiveValues(up = NULL,
                               cn = NULL,
                               nd = NULL)
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 # selected is important for inputs not seem re-initialized
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    req(input$tab)
    # if (input$tab == "connections") {
    sm_f <- i_(c(#"sample_ch_cn_0", 
      "sample_ch_cn_1"), lang())
    # } else {
    #   sm_f <- i_(c(#"sample_ch_nd_0",
    #                "sample_ch_nd_1"), lang())
    # }
    names(sm_f) <- i_(c(#"sample_ch_nm_0",
      "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFile = sm_f,
         sampleSelected = input$`initial_data-inputDataSample`, 
         
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
  
  observeEvent(list(labels(), input$`initial_data-tableInput`), {
    # observe({
    # do.call(callModule, c(tableInput, "initial_data"))
    # inputData <- eventReactive(list(labels(), input$`initial_data-tableInput`), {
    data_input$up <- do.call(callModule, c(tableInput, "initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    if (input$tab == "connections") {
      uiOutput("connections_preview")
    } else {
      uiOutput("nodes_preview")
    }
  })
  
  # observe({
  #   req(input$`initial_data-tableInput`, data_input$up)
  #   # d0 <- inputData()()
  #   d0 <- data_input$up()
  #   if (!is.null(d0)) {
  #     if (input$tab == "connections") {
  #       if (!identical(d0, data_input$nd)) {
  #         data_input$cn <- d0
  #       }
  #     } else {
  #       if (!identical(d0, data_input$cn)) {
  #         data_input$nd <- d0
  #       }
  #     }
  #   }
  #   if (is.null(data_input$nd) & !is.null(data_input$cn)) {
  #     if (all(c("to", "from") %in% names(data_input$cn))) {
  #       data_input$nd <- data.frame(id = unique(c(data_input$cn$to, data_input$cn$from)))
  #     }
  #   }
  # })
  
  observe({
    req(input$`initial_data-tableInput`, data_input$up)
    # d0 <- inputData()()
    d0 <- data_input$up()
    if (!is.null(d0)) {
      data_input$cn <- d0
      if (all(c("to", "from") %in% names(d0))) {
        data_input$nd <- data.frame(id = unique(c(d0$to, d0$from)))
      }
      
      #   if (!identical(d0, data_input$nd)) {
      #     data_input$cn <- d0
      #   }
      # } else {
      #   if (!identical(d0, data_input$cn)) {
      #     data_input$nd <- d0
      #   }
      # }
    }
  })
  
  
  observe({
    # print(input$hotr_cn_input)
    # data_input$nd <- hotr_table(input$hotr_nd_input)
    # data_input$cn <- hotr_table(input$hotr_cn_input))
  })
  
  output$connections_preview <- renderUI({
    req(data_input$cn)
    suppressWarnings(hotr("hotr_cn_input", data = data_input$cn, order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  output$nodes_preview <- renderUI({
    req(data_input$nd)
    suppressWarnings(hotr("hotr_nd_input", data = data_input$nd, order = NULL, options = list(height = 470), enableCTypes = FALSE))
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
    ch1 <- as.character(parmesan$network$inputs[[2]]$input_params$choices)
    names(ch1) <- i_(ch1, lang())
    ch2 <- as.character(parmesan$edges$inputs[[3]]$input_params$choices)
    names(ch2) <- i_(ch2, lang())
    ch3 <- c("connections", "nodes")
    names(ch3) <- i_(ch3, lang())
    updateSelectizeInput(session, "nd_shape", choices = ch0, selected = input$nd_shape)
    updateSelectizeInput(session, "layout", choices = ch1, selected = input$layout)
    updateRadioButtons(session, "ed_arrows", choices = ch2, selected = input$ed_arrows)
    updateRadioButtons(session, "tab", choices = ch3, selected = input$tab)
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
  
  cn <- reactive({
    hotr_table(input$hotr_cn_input)
  })
  
  nd <- reactive({
    hotr_table(input$hotr_nd_input)
  })
  
  # observe({
  ntwrk <- reactive({
    # ntwrk <- eventReactive(list(nd(), cn()), {
    req(data_input$cn, data_input$nd)
    # Sys.sleep(10)
    # req(cn())
    # req(input$hotr_nd_input$data, input$hotr_cn_input$data)
    # data_input$cn
    # data_input$nd
    # ¿Proxy?
    # nodes <- data.frame(id = 1:3, label = LETTERS[1:3])
    # edges <- data.frame(from = c(1,2), to = c(1,3))
    nd <- data_input$nd
    cn <- data_input$cn
    # nd <- hotr_table(input$hotr_nd_input)
    # cn <- hotr_table(input$hotr_cn_input)
    # if (!is.null(cn) & !is.null(nd)) {
    tl0 <- input$nd_tooltip
    tl1 <- input$ed_tooltip
    if (nzchar(tl0)) {
      invisible(map(names(nd), function(e) {
        rp <- paste0("\\{", e, "}")
        if (grepl(rp, tl0[1])) {
          tl0[1] <- gsub(rp, paste0("\\{", e, "} "), tl0[1])
          s0 <- strsplit(tl0[1], rp)[[1]]
          tl2 <- c()
          map(seq_along(s0), function(w) {
            if (w + 1 <= length(s0)) {
              tl2 <<- paste0(tl2, s0[w], nd[[e]])
            } else {
              tl2 <<- paste0(tl2, s0[w])
            }
          })
          tl0 <<- tl2
        }
      }))
    } else {
      tl0 <- c()
      map(names(nd), function(i) {
        tl0 <<- paste0(tl0, "<span style = 'font-size:15px;'><strong>", i, ": </strong> ", nd[[i]], "</span><br/>")
      }) 
    }
    nd$title <- tl0
    if (nzchar(tl1)) {
      invisible(map(names(cn), function(e) {
        rp <- paste0("\\{", e, "}")
        if (grepl(rp, tl1[1])) {
          tl1[1] <- gsub(rp, paste0("\\{", e, "} "), tl1[1])
          s0 <- strsplit(tl1[1], rp)[[1]]
          tl3 <- c()
          map(seq_along(s0), function(w) {
            if (w + 1 <= length(s0)) {
              tl3 <<- paste0(tl3, s0[w], cn[[e]])
            } else {
              tl3 <<- paste0(tl3, s0[w])
            }
          })
          tl1 <<- tl3
        }
      }))
    } else {
      tl1 <- c()
      map(names(cn), function(i) {
        tl1 <<- paste0(tl1, "<span style = 'font-size:15px;'><strong>", i, ": </strong> ", cn[[i]], "</span><br/>")
      }) 
    }
    cn$title <- tl1
    if (input$nd_lb != "no") {
      nd$label <- nd[[input$nd_lb]]
    }
    if (input$ed_lb != "no") {
      cn$label <- cn[[input$ed_lb]]
    }
    # v0 <- visNetwork(nodes, edges, main = input$title) %>%
    visNetwork(nodes = nd, edges = cn, main = input$title) %>%
      visNodes(shape = input$nd_shape,
               size = input$nd_size,
               borderWidth = input$nd_border,
               # color = list(background = paste0("#", input$nd_color),
               #              border = paste0("#", input$nd_border_color)),
               # font = list(color = paste0("#", input$nd_lb_color),
               color = list(background = input$nd_color,
                            border = input$nd_border_color),
               font = list(color = input$nd_lb_color,
                           size = input$nd_lb_size),
               shadow = input$nd_shadow) %>%
      visEdges(arrows = input$ed_arrows,
               smooth = input$ed_smooth,
               width = input$ed_size,
               # color = list(color = paste0("#", input$ed_color)),
               # font = list(color = paste0("#", input$nd_lb_color),
               color = list(color = input$ed_color),
               font = list(color = input$nd_lb_color,
                           size = input$nd_lb_size),
               shadow = input$ed_shadow) %>%
      visInteraction(dragNodes = input$drag_nodes,
                     dragView = input$drag_view,
                     zoomView = FALSE) %>%
      visInteraction(navigationButtons = input$zoom) %>%
      visLayout(randomSeed = 32) %>%
      visIgraphLayout(input$layout)
    # }
  })
  
  # renderizando reactable
  output$result <- renderVisNetwork({
    # session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadHtmlwidget"))
    req(ntwrk())
    # ntwrk()
    # req(r0$n)
    # r0$n
  })
  
  output$modal <- renderUI({
    dw <- i_("download", lang())#Download HTML
    downloadHtmlwidgetUI("download_data_button", paste(dw, "HTML"))
  })
  
  # descargas
  callModule(downloadHtmlwidget, "download_data_button", widget = reactive(ntwrk()), name = "network")
  
  
  
  
  
}



shinyApp(ui, server)




