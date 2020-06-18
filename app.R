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

# ya subí unos datos; quiero ver una cosa de los de muestra, los escojo y entonces pierdo los míos
# input$colorpalette devuelve el hex sin numeral no se puede hexa?


# poner botones en vez de radiobotones
# better sample data
# ¿quitar hierarchical porque también se hace con níveles?



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
    if (input$tab == "connections") {
      sm_f <- i_(c(#"sample_ch_cn_0", 
                   "sample_ch_cn_1"), lang())
    } else {
      sm_f <- i_(c(#"sample_ch_nd_0",
                   "sample_ch_nd_1"), lang())
    }
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
    #   do.call(callModule, c(tableInput, "initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    if (input$tab == "connections") {
      uiOutput("connections_preview")
    } else {
      uiOutput("nodes_preview")
    }
  })
  
  observe({
    req(input$`initial_data-tableInput`, data_input$up)
    # d0 <- inputData()()
    d0 <- data_input$up()
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
    if (is.null(data_input$nd) & !is.null(data_input$cn)) {
      if (all(c("to", "from") %in% names(data_input$cn))) {
        data_input$nd <- data.frame(id = unique(c(data_input$cn$to, data_input$cn$from)))
      }
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
  
  r0 <- reactiveValues(n = NULL)
  
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
    # nd <- nd()
    # cn <- cn()
    if (input$nd_lb != "no") {
      nd$label <- nd[[input$nd_lb]]
    }
    if (input$ed_lb != "no") {
      cn$label <- cn[[input$ed_lb]]
    }
    # v0 <- visNetwork(nodes, edges, main = input$title) %>%
    v0 <- visNetwork(nodes = nd, edges = cn, main = input$title) %>%
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
                     zoomView = input$zoom) %>%
      visLayout(randomSeed = 32)
    if (input$layout == "hierarchical") {
      v0 <- v0 %>%
        visHierarchicalLayout()
    } else if (input$layout == "igraph") {
      # v0 <- v0 %>%
      #   visIgraphLayout()
    } else if (input$layout == "circle") {
      v0 <- v0 %>%
        visIgraphLayout("layout_in_circle")
    }
    # v0
    r0$n <- v0
  })
  
  # renderizando reactable
  output$result <- renderVisNetwork({
    # session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadHtmlwidget"))
    req(ntwrk())
    ntwrk()
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




