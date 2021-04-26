library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(dspins)
library(hotr)
library(shinycustomloader)
library(shinydisconnect)
library(shinybusy)

library(V8)
library(visNetwork)
library(tidyverse)
library(htmlwidgets)
library(igraph)

#webshot::install_phantomjs()

styles <- "
.panel {
  flex-shrink: unset;
}

.panel-header {
  position: inherit;
  z-index: inherit;
}

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
}
"


ui <-  panelsPage(useShi18ny(),
                  styles = styles,
                  disconnectMessage(
                    text = "Tu sesión ha finalizado, si tienes algún problema trabajando con la app por favor contáctanos y cuéntanos qué ha sucedido // Your session has ended, if you have any problem working with the app please contact us and tell us what happened.",
                    refresh = "REFRESH",
                    background = "#ffffff",
                    colour = "#435b69",
                    size = 14,
                    overlayColour = "#2a2e30",
                    overlayOpacity = 0.85,
                    refreshColour = "#ffffff",
                    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
                  ),
                  busy_start_up(
                    loader = tags$img(
                      src = "img/loading_gris.gif",
                      width = 100
                    ),
                    mode = "auto",
                    color = "#435b69",
                    background = "#FFF"
                  ),
                  langSelectorInput("lang", position = "fixed"),
                  panel(title = ui_("upload_data"),
                        collapse = TRUE,
                        width = 300,
                        body = uiOutput("dataInput")),
                  panel(title = ui_("dataset"),
                        width = 450,
                        body = div(radioButtons("tab", "", c("connections", "nodes")),
                                   uiOutput("data_preview"))),
                  panel(title = ui_("options"),
                        width = 350,
                        color = "chardonnay",
                        body = uiOutput("controls")),
                  panel(title = ui_("viz"),
                        title_plugin = uiOutput("download"),
                        color = "chardonnay",
                        can_collapse = FALSE,
                        body = div(langSelectorInput("lang", position = "fixed"),
                                   withLoader(visNetworkOutput("result", height = "80vh"), 
                                              type = "image", loader = "img/loading_gris.gif")))
)





server <- function(input, output, session) {
  
  
  i18n <- list(defaultLang = "en",
               availableLangs = c("en", "es", "pt_BR"),
               localeDir = "locale/",  
               customTranslationSource = "yaml"
  )
  
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  
  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })

  # Modulo de carga de datos ------------------------------------------------

  data_input <- reactiveValues(up = NULL,
                               cn = NULL,
                               nd = NULL)

  output$dataInput <- renderUI({
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
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  
  parmesan_lang <- reactive({
    i_(parmesan, lang(), keys = c("label", "choices", "text"))
  })
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input, 
                  output = output, 
                  session = session,
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
  
  ntwrk <- reactive({
    req(data_input$cn, data_input$nd)

    nd <- data_input$nd
    cn <- data_input$cn
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
  
  
  # renderizando reactable
  output$result <- renderVisNetwork({
    req(ntwrk())
  })
  
  
  par <- list(user_name = "test", org_name = NULL, plan = "basic")
  url_par <- reactive({
    url_params(par, session)
  })
  
  output$download <- renderUI({
    
    downloadDsUI("download_data_button",
                 display = "dropdown",
                 formats = c("html","jpeg", "pdf", "png"),
                 dropdownWidth = 170,
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 text = i_("download", lang()),
                 dropdownLabel = i_("download_viz", lang()),
                 getLinkLabel = i_("get_link", lang()),
                 modalTitle = i_("get_link", lang()),
                 modalButtonLabel = i_("gl_save", lang()),
                 modalLinkLabel = i_("gl_url", lang()),
                 modalIframeLabel = i_("gl_iframe", lang()),
                 nameLabel = i_("gl_name", lang()),
                 descriptionLabel = i_("gl_description", lang()),
                 sourceLabel = i_("gl_source", lang()),
                 sourceTitleLabel = i_("gl_source_name", lang()),
                 sourcePathLabel = i_("gl_source_path", lang()),
                 licenseLabel = i_("gl_license", lang()),
                 tagsLabel = i_("gl_tags", lang()),
                 tagsPlaceholderLabel = i_("gl_type_tags", lang()),
                 categoryLabel = i_("gl_category", lang()),
                 categoryChoicesLabels = i_("gl_no_category", lang())
    )
  })
  
  
  observe({
    req(ntwrk())
    user_name <- url_par()$inputs$user_name
    org_name <- url_par()$inputs$org_name
    if (is.null(user_name) & is.null(user_name)) return()
    downloadDsServer(id = "download_data_button",
                     element = reactive(ntwrk()),
                     formats = c("html", "jpeg", "pdf", "png"),
                     errorMessage = i_("error_down", lang()),
                     elementType = "dsviz",
                     user_name = user_name,
                     org_name = org_name)
  })
  
}



shinyApp(ui, server)