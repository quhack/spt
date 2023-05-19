# to use - source('factored_app.R')
# when running in dev, setwd first
# setwd("~/R/spt")

library(yaml)
library(shiny)
library(DT)
library(ggplot2)
# library(purrr) # for pmap
library(patchwork) # for stitching plots together
source('helpers.R')

confidence_levels = list(A=0.95, B=0.8, C=0.99, D=0.5)

params <- yaml.load_file('config.yml')

indat <- lapply(params, get_data_from_list)
indat <- lapply(indat, add_est_total_columns)
indat <- lapply(indat, add_confidence_columns, confidence_levels)

temp_progress_values <- lapply(indat, get_unaverage_progress_scores, confidence_levels)

indat <- lapply(indat, calc_all_mean_sd)

year_names <- names(indat)
indat <- lapply(names(indat), calc_all_cutoffs,
                indat = indat,
                progress_data = temp_progress_values,
                ci_vals=confidence_levels)
indat <- setNames(indat, year_names)
rm(temp_progress_values, year_names)

indat <- lapply(indat, apply_bands, ci_vals = confidence_levels) # this is the function that takes a little time
indat <- lapply(indat, add_graph_func, ci_vals = confidence_levels)


displaydat <- lapply(indat, generate_display_data)


# TODO - work out why we have conf_level and p-value recorded separately


# chart_data <- lapply(indat, generate_chart_data)


s_x_band <- lapply(indat, function(x){
  plots <- generate_size_x_band(x, ci_vals = confidence_levels)
  return (lapply(plots, function(y){
    renderPlot(
      wrap_plots(y)
    )
  }))
})

n_size_x_band <- do.call(tagList, s_x_band)
rm(s_x_band)

bp <- lapply(indat, function(x){
  plots <- generate_boxplot(x, ci_vals = confidence_levels)
  return(lapply(plots, function(y){
    renderPlot(
      wrap_plots(y)
    )
  }))
})

n_boxplot <- do.call(tagList, bp)
rm(bp)


shinyApp(
  ui = bootstrapPage(
    theme=bslib::bs_theme(version=5),
    tags$head(
      HTML(paste('<title>Progress visuals</title>',favicon_html,sep="\n"))),
    div(class="container",
        div(class="row",
            tabsetPanel(type = "tabs",
                  tabPanel("Selection", DTOutput('tbl')),
                  tabPanel("Summary", #div(class="row", plotOutput("summary_plots")),
                           div(class="row", uiOutput('flex_summary_plots')),
                           div(class="row", uiOutput('boxplots'))
                           ),
                  tabPanel("Progress",
                           # probably need to wrap each iteration in a div ob but holding off while waiting for 
                           div(class="row",uiOutput('all_progplots'))
                           ),
                  tabPanel("Selections",
                           actionButton('clear1', 'Clear Rows'),
                           DTOutput('selected_schools'))
                  )
        )
    )
  ),
  server = function(input, output) {
    output$tbl = renderDT(
      datatable(displaydat[["2019"]], #################################################
                plugins = "ellipsis",
                class = 'table table-bordered table-sm',
                filter = list(position = 'top', clear = FALSE),
                options = list(dom = 'ltpr',
                               # lengthChange = FALSE,
                               autoWidth = TRUE,
                               columnDefs = list(list(
                                 targets = 2,
                                 render = JS("$.fn.dataTable.render.ellipsis( 24, false )")
                                 ),
                                 list(width = '200px', targets = 2)
                                 ),
                               scrollX = TRUE
                               ),
                # selection = 'none')
                )
    )
    
    proxy = dataTableProxy('tbl')
    observeEvent(input$clear1, {
      proxy %>% selectRows(NULL)
    })

    output$selected_schools = renderDT({
      rows_sel <- input$tbl_rows_selected
      if (length(rows_sel)){
        datatable(
          displaydat[["2019"]] %>% filter(row_number() %in% rows_sel) %>%
            select(c(URN, SCHNAME, READPROG, READPROG_DESCR)), ################################
          colnames = c('band' = 'READPROG_DESCR'), ################################
          rownames = FALSE,
          class = 'table-bordered table-sm',
          options = list(dom = 'tr',
                         lengthChange = FALSE,
                         ordering = FALSE),
          selection = 'none'
        )
      }
    })
    
    # output$summary_plots = renderPlot({
    #   size_x_band / boxplot
    # })

    output$flex_summary_plots = renderUI({n_size_x_band})
    
    output$boxplots = renderUI(n_boxplot)
    
    make_plots <- reactive({
      lapply(indat, \(x){
        plots <- generate_progress_plots(x,
                                         selections = input$tbl_rows_selected,
                                         ci_vals = confidence_levels)
        lapply(plots, \(x){
          renderPlot(
            wrap_plots(x)
          )
        })
      })
    })
    output$all_progplots <- renderUI({
      do.call(tagList, make_plots())
    })
    
})


