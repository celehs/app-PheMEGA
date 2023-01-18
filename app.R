
library(DBI)
library(dplyr)
library(DT)
library(echarts4r)
library(ggplot2)
library(htmlwidgets)
library(jsonlite)
library(plotly)
library(rintrojs)
library(RPostgres)
library(shiny)
library(shinybrowser)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyhelper)
library(shinyWidgets)



source("getData.R")
source("plotly_signal.R")
source("plotly_hetero.R")
source("hetero_polar.R")
source("out_table.R")
source("utils.R")

steps <- readRDS("doc/steps.rds")
dict <- getTable("dict_v20220905_2")
num_samples <- getTable("num_samples")
init_Phe250_11 <- getTable("init_Phe250_11_v20220905_2")

# header ====
header <- shinydashboardPlus::dashboardHeader(
  title = "PheMEGA",
  titleWidth = "300pt",
  leftUi = tagList(
    h4("Phenotype Multi-ethnicity Genetic Architecture", style="color: white; margin-top: 5px !important; margin-bottom: 0px !important;"),
    actionButton("instruct", " About",
                 icon = icon("book"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction of the app."
    ),
    actionButton("help", " Help",
                 icon = icon("question"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction tour."
    )
  ),
  disable = FALSE
)

ui <- shinydashboardPlus::dashboardPage(
  header,
  ## Sidebar ====
  dashboardSidebar(
    width = "300pt",
    disable = FALSE,
    minified = FALSE,
    div(id = "ui_searchbox",
      searchInput(
        inputId = "searchbox",
        label = "Enter your search: ",
        placeholder = "PheCode:250",
        value = NULL,
        btnSearch = icon("search"),
        width = "100%"
      )),
    uiOutput("ui_table") %>%
      shinyhelper::helper(type = "markdown",
                          colour = "white",
                          title = "Notes",
                          content = "helper_input_table",
                          size = "m",
                          style = "margin-right: 5px;"),
    uiOutput("ui_checkbox_ances"),
    div(id = "check_box",
      checkboxGroupInput("inCheckboxGroup1", "Selected SNPs:"),
      checkboxGroupInput("inCheckboxGroup2", "Selected traits:")),
    fluidRow(column(6,
               div(actionButton("refresh", "Unselect",
                                icon = tags$i(class = "fa fa-refresh",
                                              style="font-size: 10px")),
                   align = "center")),
             column(6,
               div(actionButton("goButton", "Submit",
                                icon = tags$i(class = "far fa-play-circle",
                                              style="font-size: 10px"),
                                class = "btn-success"),
                   align = "center"))),
    # filter ====
    div(id = "ui_filter",
      sliderTextInput(
        inputId = "thr_p",
        label = "Filter by P-value:",
        choices = c(0, 10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9,
        10^-8, 5 * 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2),
        selected = 5 * 10^-8,
        grid = TRUE,
        width = "100%"))),

  # body ====
  dashboardBody(
    includeCSS("www/style.css"),
    includeScript("www/polar_hetero.js"),
    shinyWidgets::useSweetAlert("minimal"),
    shinybrowser::detect(),
    introjsUI(),
    bsModal(
      id = "instruction", title = "About", trigger = FALSE,
      size = "large",
      includeMarkdown("doc/about.md")
      # includeMarkdown(app_sys("app/doc/documentation.md")
    ),
    tabsetPanel(id = "tab_comp",
                tabPanel(title = "Significant signals",
                         uiOutput("ui_num_2"),
                         uiOutput("ui_signal_2")),
                tabPanel(title = "Shared signals",
                         uiOutput("ui_num_comm"),
                         uiOutput("ui_signal_comm")),
                tabPanel(title = "Heterogeneous signals",
                         br(),
                         fluidRow(
                           column(3,
                                  sliderTextInput(
                                    inputId = "p1_hetero",
                                    label = "Filter by FDR adjusted P-value of heterogeneity test:",
                                    choices = c(0, 1e-30, 1e-20, 1e-15, 1e-10, 1e-5, 1e-4, 1e-3, 0.01, 0.05, 0.1),
                                    selected = 0.05,
                                    grid = TRUE,
                                    width = "100%"
                                  )),
                           column(3, uiOutput("ui_slider_maf")),
                           column(3, div(align = "center",
                                  radioButtons(
                                    inputId = "s_ance_1",
                                    label = "To compare with EUR:",
                                    choices = c("AFR", "AMR", "ESA"),
                                    selected = "AFR",
                                    inline = TRUE
                                  ))),
                           column(3, uiOutput("ui_hetero_groups"))),
                         fluidRow(uiOutput("ui_barplot")),
                         hr(),
                         fluidRow(uiOutput("ui_polar"))
                ),
                tabPanel(title = "Table",
                         br(),
                         column(8,
                           radioButtons("sort_by", "Sort the table by:",
                                        choices = c("Variable", "P-value", 
                                          "Heterogeneity Adjusted P-value (to EUR)"),
                                        selected = "Variable", inline = TRUE)),
                         column(4, 
                           shinyWidgets::switchInput("sort_decreasing", "Decreasing", 
                             value = FALSE)),
                         DT::dataTableOutput("set_table"))
    )
  )
)

server <- function(input, output, session) {

  # input table ====
  df_input <- eventReactive(input$searchbox, {
    print("df_input")
    print(input$searchbox)
    if (isTruthy(input$searchbox)) {
      searchDict(input$searchbox, "dict_v20220905_2")
    } else {
      searchDict("PheCode:250", "dict_v20220905_2")
    }
  }, ignoreNULL = FALSE)

  output$ui_table <- renderUI({
    print("ui_table")
    req(df_input())
    print(nrow(df_input()))
    if (isTruthy(df_input())) {
      shinycssloaders::withSpinner(
        DT::dataTableOutput("table"), type = 6
      )
    } else {
      ""
    }
  })

  output$table <- DT::renderDataTable(DT::datatable({
    df <- df_input()[, c("id", "desc", "group")]
    if (grepl("^\\d.+", df$id[1])) {
      colnames(df) <- c("chr:pos:alt:ref", "rsid", "gene")
    }
    df
  }, rownames = FALSE,
    selection = list(mode = "single",
                     selected = 2,
                     target = "row"),
  options = list(pagingType = "simple", pageLength = 5, dom = "lrtip")
    ), server = TRUE)

  # cbox_ances ====
  observeEvent(input$table_rows_selected,{
    if (length(input$table_rows_selected) == 1) {
      output$ui_checkbox_ances <- renderUI({
        checkboxGroupInput("checkbox_ances", "Select ancestries",
                           choiceNames = c("META", "African(AFR)",
                                           "Admixed American(AMR)",
                                           "East Asian(ESA)",
                                           "European(EUR)"),
                           choiceValues = c("META", "AFR", "AMR", "ESA", "EUR"),
                           inline = TRUE,
                           selected = c("AFR", "EUR"))
      })
    }
  })

  # center_nodes ====
  s1 <- eventReactive(input$goButton, {
    input$inCheckboxGroup1
  })

  s2 <- eventReactive(input$goButton, {
    # init data
    if(!isTruthy(input$goButton)){
      c("AFR_PheCode:250.11", "EUR_PheCode:250.11")
    } else {
      input$inCheckboxGroup2
    }
  })

  print("center_nodes()")
  center_nodes <- reactive({
    c(s1(), s2())
  })

  ids <- reactive({unique(gsub("^[A-Z]+_", "", center_nodes(), perl = TRUE))})
  ances <- reactive({gsub("_.+", "", center_nodes(), perl = TRUE)})

  df_data <- reactive({
    req(ids())
    print("df_data")
    Reduce(rbind, lapply(ids(), getData, "df_edges_v20220905_2"))
  })

  df_centers <- reactive({
    if (isTruthy(input$goButton)) {
      print("df_centers")
      print(center_nodes())
      print(ances())
      print(unique(df_data()$ance))
      df <- df_data()[df_data()$ance %in% ances(),]
      left_join(df, dict, by = c("to" = "id"))
    } else {
      init_Phe250_11
    }
  })

  observeEvent(input$goButton, {
    ance <- setdiff(ances(),
                    unique(df_centers()$ance[!is.na(df_centers()$pval)]))
    if (length(ance) > 0) {
      show_alert(
        title = "Warning",
        text = paste0("No data with ", paste(ance, collapse = ", "), "!"),
        type = "warning"
      )
    }
  })

  df_p <- reactive({
    req(input$thr_p)
    df <- df_centers()[!is.na(df_centers()$pval), ]
    df_t <- df[df$pval < input$thr_p, ]
    print("df_pval")
    print(length(unique(df_t$to)))
    df[df$to %in% unique(df_t$to), ]
  })

  # manhattan ====

  output$ui_num_2 <- renderUI({
    if(nrow(df_p()) > 1000){
      # print("num_points_2")
      sliderInput("num_points_2",
                  label = "Maximum number of points to show:",
                  max = nrow(df_p()), min = 100,
                  value = 500,
                  width = "500px")
    } else {
      ""
    }
  })

  topN <- function(df, topn){

    print("topN")
    print(unique(df$from))
    print(!grepl("^\\d.+", unique(df$from), perl = TRUE))
    if(!grepl("^\\d.+", unique(df$from), perl = TRUE)){
      df$chrom <- gsub("^(\\d+):.+", "\\1", df$to, perl = TRUE)
    } else {
      df$chrom <- df$dataset
    }
    df_n <- df %>% group_by(chrom) %>% summarise(n = n())
    df_n$c <- topn/length(unique(df$ance)) * df_n$n/sum(df_n$n)

    df_n$c <- ifelse(df_n$c < 5,
                     sapply(df_n$n, function(x) min(c(x, 5))),
                     df_n$c)
    print(head(df_n))
    df <- df[order(df$pval), ]
    df_top_n <- NULL
    for(i in 1:nrow(df_n)){
      df_t <- df[df$chrom == df_n$chrom[i], ]
      df_t <- df_t[1:df_n$c[i], ]
      df_top_n <- rbind(df_top_n, df[df$to %in% unique(df_t$to), ])
    }
    print("topN")
    print(nrow(df_top_n))
    df_top_n
  }

  df_pval_2 <- reactive({
    req(df_p())
    if (nrow(df_p()) > 1000) {
      print("df_pval_2")
      req(input$num_points_2)
      topN(df_p(), input$num_points_2)
    } else {
      df_p()
    }
  })

  signal_height <- reactive({
    if (grepl("^\\d.+", unique(df_pval_2()$from))) {
      paste0(shinybrowser::get_height() - 150,"px")
    } else {
      paste0(min(600, shinybrowser::get_height() - 150),"px")
    }
  })

  # signal_plotly ====
  output$ui_signal_2 <- renderUI({
    print("ui_signal_2")
    req(df_pval_2())
    print(head(df_pval_2()))
    print(dim(df_pval_2()))
    if (isTruthy(df_pval_2()) & nrow(df_pval_2()) > 0) {
      shinycssloaders::withSpinner(
        plotlyOutput("plot_signal_2", width = "100%",
                     height = signal_height()), type = 6
      )
    } else {
      "No significant signals!"
    }
  })

  output$plot_signal_2 <-  renderPlotly({
    if (isTruthy(df_pval_2()) & nrow(df_pval_2()) > 0) {
      signal_plotly(df_pval_2(), thr_pval = input$thr_p, type = "scatter")
    }
  })

  # plot_comm ====
  observeEvent(df_comm(), {
    if (nrow(df_comm()) > 10) {
      showTab(inputId = "tab_comp", target = "Shared signals")
    } else {
      hideTab(inputId = "tab_comp", target = "Shared signals")
    }
  })

  output$ui_signal_comm <- renderUI({
    if (isTruthy(df_pval_comm()) & nrow(df_pval_comm()) > 0) {
      print("ui_signal_comm")
      print(nrow(df_comm()))
      shinycssloaders::withSpinner(
        plotlyOutput("plot_signal_comm", width = "100%",
                     height = signal_height()), type = 6
      )
    } else {
      "No shared signals at the given signficance level!"
    }
  })

  df_comm <- reactive({
    req(input$thr_p)
    df <- df_centers()
    to <- unique(df$to)
    print("df_comm")
    print(as.numeric(input$thr_p))
    for (a in unique(df$ance)) {
      df1 <- df[!is.na(df$pval) & df$ance == a, ]
      to <- intersect(to, unique(df1$to[df1$pval < as.numeric(input$thr_p)]))
    }
    df[df$to %in% unique(to), ]
  })

  output$ui_num_comm <- renderUI({
    if (nrow(df_comm()) > 500) {
      sliderInput("num_points_comm", 
                  label = "Maximum number of points to show:",
                  max = nrow(df_comm()), min = 500,
                  value = 500,
                  width = "500px")
    } else {
      ""
    }
  })

  df_pval_comm <- reactive({
    if (nrow(df_comm()) > 500) {
      req(input$num_points_comm)
      topN(df_comm(), input$num_points_comm)
    } else {
      df_comm()
    }
  })

  output$plot_signal_comm <-  renderPlotly({
    signal_plotly(df_pval_comm(), thr_pval=as.numeric(input$thr_p), type = "scatter")
  })


  ## hetero ====
  df_hetero <- reactive({
    print("df_hetero")
    if (isTruthy(input$goButton)) {
      df <- df_data()[df_data()$ance %in% input$s_ance_1, ]
      df <- df[!is.na(df$p_heter_adj), ]
      df <- df[df$p_heter_adj < as.numeric(input$p1_hetero), ]
      df <- df_data()[df_data()$to %in% df$to & df_data()$ance %in% c("EUR", input$s_ance_1), ]
      left_join(df, dict, by = c("to" = "id"))
    } else {
      print("init df_hetero")
      df <- df_centers()
      df <- df[!is.na(df$p_heter_adj), ]
      df <- df[df$p_heter_adj < as.numeric(input$p1_hetero), ]
      df_centers()[df_centers()$to %in% df$to, ]
    }
  })

  output$ui_slider_maf <- renderUI({
    if (nrow(df_hetero()) > 0) {
      print("output$ui_slider_maf")
      sliderInput("thr_maf", "Min MAF:",
                  min = floor(min(df_hetero()$maf) * 100) / 100,
                  max = ceiling(max(df_hetero()$maf) * 100) / 100,
                  value = floor(min(df_hetero()$maf) * 100) / 100)
    } else {
      show_alert(
        title = "Warning",
        text = paste0("No heterogeneity detected at the given FDR level!"),
        type = "warning"
      )
      ""
    }
  })

  df_hetero_maf <- reactive({
    req(input$thr_maf)
    print("df_hetero_maf")
    id <- df_hetero()$to
    for (a in unique(df_hetero()$ance)) {
      df <- df_hetero()[df_hetero()$ance == a & df_hetero()$maf >= input$thr_maf, ]
      id <- intersect(id, df$to)
    }
    df_hetero()[df_hetero()$to %in% id, ]
  })

  observe({
    req(df_hetero_maf())
    print("output$ui_hetero_groups")
    if (nrow(df_hetero_maf()) > 0) {
      if (grepl("^\\d.+", unique(df_hetero_maf()$from))) {
        output$ui_hetero_groups <- renderUI({
          checkboxGroupInput(
            "checkbox_groups", "Select Phenotype Categories:",
            choices = unique(df_hetero_maf()$dataset),
            inline = TRUE,
            selected = unique(df_hetero_maf()$dataset))
        })
      } else {
        df <- df_hetero_maf()
        df$dataset <- gsub("\\:.+", "", df$to, perl = TRUE)
        datasets <- sort(unique(as.numeric(df$dataset)))
        output$ui_hetero_groups <- renderUI({
          shinyWidgets::pickerInput(
            "checkbox_groups", "Select chromosomes to show:",
            choices = datasets,
            selected = datasets[1],
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            width = "100%"
          )
        })
      }
    }
  })

  # hetero bar ====

  observe({
    if (nrow(df_hetero_maf()) > 0) {
      print("ui_barplot")
      print(nrow(df_hetero_maf()))
      if (!grepl("^\\d.+", unique(df_hetero_maf()$from))) {
        print("ui_barplot_snp")
        output$ui_barplot <- renderUI({tagList(
          shinycssloaders::withSpinner(
            plotlyOutput("hetero_bt",
                         width = ifelse(nrow(df_hetero_maf()) < 10, "600px", "100%"),
                         height = "800px"), type = 6)
        )})
        output$ui_polar <- renderUI({tagList(
          # includeScript("www/polar_hetero.js"),
          shinycssloaders::withSpinner(
            echarts4rOutput("polar_bt", width = "100%", height = "700px"),
            type = 6)
        )})
      } else {
        print("2 hetero")
        if (nrow(df_hetero_maf()[df_hetero_maf()$variable == "or", ]) > 0) {
          plot1 <- shinycssloaders::withSpinner(
            plotlyOutput("hetero_bt", width = "100%", height = "800px"),
            type = 6)
          plot3 <- shinycssloaders::withSpinner(
            echarts4rOutput("polar_bt", width = "100%", height = "700px"),
            type = 6)
        } else {
          plot1 <- h4("No heterogeneity detected at the given FDR level!")
          plot3 <- NULL
        }
        if (nrow(df_hetero_maf()[df_hetero_maf()$variable == "beta", ]) > 0) {
          print("plot2")
          print(nrow(df_hetero_maf()[df_hetero_maf()$variable == "beta", ]))
          plot2 <- shinycssloaders::withSpinner(
            plotlyOutput("hetero_qt", width = "100%",
                         height = "800px"),
            type = 6)
          plot4 <- shinycssloaders::withSpinner(
            echarts4rOutput("polar_qt", width = "100%", height = "700px"),
            type = 6)
        } else {
          plot2 <- h4("No heterogeneity detected at the given FDR level!")
          plot4 <- NULL
        }
        output$ui_barplot <- renderUI({tagList(fluidRow(
          column(6, h4("Binary traits"), plot1),
          column(6, h4("Quantitative traits"), plot2))
        )})
        output$ui_polar <- renderUI({tagList(
          # includeScript("www/polar_hetero.js"),
          fluidRow(
          column(6, h4("Binary traits"), plot3),
          column(6, h4("Quantitative traits"), plot4))
        )})
      }
    } else {
      output$ui_barplot <- renderUI({
        h4("No heterogeneity detected at the given FDR level!")
      })
      output$ui_polar <- renderUI({""})
    }
  })

  output$hetero_bt <- renderPlotly({
    req(input$checkbox_groups)
    req(df_hetero_maf())
    if (nrow(df_hetero_maf()) > 0) {
      if (!grepl("^\\d.+", unique(df_hetero_maf()$from))) {
        df <- df_hetero_maf()
        df$dataset <- gsub("\\:.+", "", df$to, perl = TRUE)
      } else {
        df <- df_hetero_maf()[df_hetero_maf()$variable == "or", ]
      }
      print("df_bt")
      if (nrow(df) > 0 & sum(input$checkbox_groups %in% df$dataset) > 0){
        print(as.numeric(input$p1_hetero))
        print(input$checkbox_groups)
        plotly_hetero(df, thr_p = as.numeric(input$p1_hetero),
                      groups = input$checkbox_groups)
      } else {
        plot_ly() %>%
          layout(title = "No heterogeneity detected at the given FDR level!")
      }
    }
  })

  output$hetero_qt <- renderPlotly({
    req(input$checkbox_groups)
    df_qt <- df_hetero_maf()[df_hetero_maf()$variable == "beta", ]
    print("df_qt")
    if (nrow(df_qt) > 0 & sum(input$checkbox_groups %in% df_qt$dataset) > 0) {
      print("hetero_qt")
      print(head(df_qt))
      plotly_hetero(df_qt, thr_p = as.numeric(input$p1_hetero),
                    groups = input$checkbox_groups)
    } else{
      plot_ly() %>%
        layout(title = "No heterogeneity detected at the given FDR level!")
    }
  })

  # hetero polar ====
  output$polar_bt <- renderEcharts4r({
    req(input$checkbox_groups)
    req(df_hetero_maf())
    if (nrow(df_hetero_maf()) > 0) {
      if (!grepl("^\\d.+", unique(df_hetero_maf()$from))) {
        df <- df_hetero_maf()
        df$dataset <- gsub("\\:.+", "", df$to, perl = TRUE)
      } else {
        df <- df_hetero_maf()[df_hetero_maf()$variable == "or", ]
      }
      print("df_bt")
      if (nrow(df) > 0 & sum(input$checkbox_groups %in% df$dataset) > 0) {
        print(as.numeric(input$p1_hetero))
        print(input$checkbox_groups)
        hetero_polar(df, thr_p = as.numeric(input$p1_hetero),
                     groups = input$checkbox_groups)
      }
    }
  })

  output$polar_qt <- renderEcharts4r({
    req(input$checkbox_groups)
    df_qt <- df_hetero_maf()[df_hetero_maf()$variable == "beta", ]
    print("df_qt")
    if (nrow(df_qt) > 0 & sum(input$checkbox_groups %in% df_qt$dataset) > 0) {
      hetero_polar(df_qt, thr_p=as.numeric(input$p1_hetero),
                   groups = input$checkbox_groups)
    }
  })


  ## set table =====

  output$set_table <- DT::renderDataTable(DT::datatable({
    print("set_table")
    print(nrow(df_p()))
    if (isTruthy(df_p()) & nrow(df_p()) > 0) {
      out_table(df_p(), dict, num_samples, sortby = input$sort_by,
                decreasing = input$sort_decreasing)
    } else {
      df_p()
    }
  }, rownames = FALSE,
  options = list(
    pageLength = 10,
    dom = "Blfrtip"),
  ),  server = TRUE)

  ## hideTab ====
  observeEvent(input$goButton, {
    if (length(input$checkbox_ances) == 1) {
      hideTab(inputId = "tab_comp", target = "Shared signals")
    }
  })

  # update ====

  observeEvent(input$refresh, {
    reloadData(
      dataTableProxy("table"),
      resetPaging = TRUE,
      clearSelection = c("all"))

    x <- character(0)
    updateCheckboxGroupInput(session, "inCheckboxGroup1",
                             "Selected SNP:",
                             choices = x,
                             selected = x)
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             "Selected traits:",
                             choices = x,
                             selected = x)
  })

  observeEvent(df_input(), {
    reloadData(
      dataTableProxy("table"),
      resetPaging = TRUE,
      clearSelection = c("all"))

    x <- character(0)
    updateCheckboxGroupInput(session, "inCheckboxGroup1",
                             "Selected SNP:",
                             choices = x,
                             selected = x)
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             "Selected traits:",
                             choices = x,
                             selected = x)
  })


  observe({
    print("input$table_rows_selected")
    print(input$table_rows_selected)
    x <- character(0)
    if (is.null(input$table_rows_selected)) {
      output$ui_checkbox_ances <- renderUI({
        ""
      })
      updateCheckboxGroupInput(session, "inCheckboxGroup1",
                               "Selected SNP:",
                               choices = x,
                               selected = x)
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               "Selected traits:",
                               choices = x,
                               selected = x)
    } else {
      df <- df_input()[input$table_rows_selected, ]
      df_s <- df[grepl("^\\d.+", df$id), ]
      df_p <- df[!grepl("^\\d.+", df$id),]
      if(isTruthy(df_s) & nrow(df_s) > 0) {
        updateCheckboxGroupInput(session, "inCheckboxGroup1",
                                 "Selected SNP:",
                                 choiceValues = paste0(input$checkbox_ances, "_", df_s$id),
                                 choiceNames = paste0(input$checkbox_ances, ": ", df_s$desc),
                                 selected = paste0(input$checkbox_ances, "_", df_s$id))
      } else {
        updateCheckboxGroupInput(session, "inCheckboxGroup1",
                                 "Selected SNP:",
                                 choices = x,
                                 selected = x)
      }
      if(isTruthy(df_p) & nrow(df_p) > 0) {
        updateCheckboxGroupInput(session, "inCheckboxGroup2",
                                 "Selected traits:",
                                 choiceValues = paste0(input$checkbox_ances, "_", df_p$id),
                                 choiceNames = paste0(input$checkbox_ances, ": ", df_p$desc),
                                 selected = paste0(input$checkbox_ances, "_", df_p$id))
      } else {
        updateCheckboxGroupInput(session, "inCheckboxGroup2",
                                 "Selected traits:",
                                 choices = x,
                                 selected = x)
      }
    }
  })

  # help ====

  observeEvent(input$help, {
    introjs(session,
            options = list(
              steps = steps[, -1],
              showBullets = FALSE
            )
    )
  }, ignoreNULL = FALSE)

  shinyhelper::observe_helpers(help_dir = "doc/")

  observeEvent(input$instruct, {
    toggleModal(session, "instruction", toggle = "open")
  })

  observeEvent(input$bookmark, {
    session$doBookmark()
  })

}

shinyApp(ui = ui, server = server, enableBookmarking  = "server")