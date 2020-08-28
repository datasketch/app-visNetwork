library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(dspins)
library(hotr)
library(visNetwork)
library(tidyverse)
library(htmlwidgets)
library(igraph)
library(shinycustomloader)

# cambiar color tabs

ui <- panelsPage(useShi18ny(),
                 showDebug(),
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
                 background-color: #da1c95;
                 color: #ffffff;
                 font-size: 13px;
                 font-weight: 700;
                 letter-spacing: 0.7px;
                 }
                 #tab input[type='radio'] {
                 display: none;
                 }"))),
                 
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       body = div(radioButtons("tab", "", c("connections", "nodes")),
                                  uiOutput("data_preview"))),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  withLoader(visNetworkOutput("result", height = "80vh"), type = "image", loader = "loading_gris.gif"))))



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
                 label = "",
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
    names(sm_f) <- i_(c("sample_ch_nm_1"), lang())
    
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
    data_input$up <- do.call(tableInputServer, c("initial_data", labels()))
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
      if (!all(c("to", "from") %in% names(d0))) {
        names(d0)[1:2] <- c("from", "to")
      }
      data_input$nd <- data.frame(id = unique(c(d0$to, d0$from)))
      data_input$cn <- d0
    }
  })
  
  output$connections_preview <- renderUI({
    req(data_input$cn)
    suppressWarnings(hotr("hotr_cn_input", data = data_input$cn, order = NULL, options = list(height = "82vh"), enableCTypes = FALSE))
  })
  
  output$nodes_preview <- renderUI({
    req(data_input$nd)
    suppressWarnings(hotr("hotr_nd_input", data = data_input$nd, order = NULL, options = list(height = "82vh"), enableCTypes = FALSE))
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
    names(ch3) <- toupper(i_(ch3, lang()))
    
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
               color = list(background = input$nd_color,
                            border = input$nd_border_color),
               font = list(color = input$nd_lb_color,
                           size = input$nd_lb_size),
               shadow = input$nd_shadow) %>%
      visEdges(arrows = input$ed_arrows,
               smooth = input$ed_smooth,
               width = input$ed_size,
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
  
  output$download <- renderUI({
    lb <- i_("download_net", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = "html",
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"))
  })
  
  # renderizando reactable
  output$result <- renderVisNetwork({
    req(ntwrk())
  })
  
  # url params
  par <- list(user_name = "brandon", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  # prepare element for pining (for htmlwidgets or ggplots)
  # función con user board connect y set locale
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    dv <- dsviz(x,
                name = nm,
                description = input$`download_data_button-modal_form-description`,
                license = input$`download_data_button-modal_form-license`,
                tags = input$`download_data_button-modal_form-tags`,
                category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  # descargas
  observe({
    downloadDsServer("download_data_button", element = reactive(ntwrk()), formats = "html",
                     errorMessage = i_("gl_error", lang()),
                     modalFunction = pin_, reactive(ntwrk()),
                     bkt = url_par()$inputs$user_name)
  })
  
}



shinyApp(ui, server)