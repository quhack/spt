# required libraries
library(tidyverse)
library(DT)
library(shiny)
library(ggplot2)
library(patchwork)

# defaults
pup_read_sd = 6.17358373
pup_writ_sd = 5.69410663
pup_mat_sd = 5.38985717
conf_level = 0.95
#

two_tail_p = qnorm(1 - ((1 - conf_level) / 2))

fscp19 = 'https://www.find-school-performance-data.service.gov.uk/download-data?download=true&regions=0&filters=KS2&fileformat=csv&year=2018-2019&meta=false'
raw_df <- read_csv(fscp19,
             col_types = list(
               URN=col_character(),
               TELIG = col_integer(),
               READPROG=col_double(),
               READPROG_DESCR=col_integer()
              ),
             na=c("NA","LOWCOV","SUPP"," ",""))
# restrict to schools - RECTYPE < 3
df <- raw_df %>%
  filter(RECTYPE <3) %>% select(c(
  URN,
  SCHNAME,
  TELIG,
  READPROG, READPROG_LOWER, READPROG_UPPER, READCOV,
  READPROG_DESCR,
  WRITPROG, WRITPROG_UPPER, WRITPROG_LOWER, WRITCOV,
  WRITPROG_DESCR,
  MATPROG, MATPROG_LOWER, MATPROG_UPPER, MATCOV,
  MATPROG_DESCR))

# school-level descriptives
sc_sd = df %>%
  summarise(r = sd(READPROG, na.rm=TRUE),
            w = sd(WRITPROG, na.rm=TRUE),
            m = sd(MATPROG, na.rm=TRUE))
all_school_n <- nrow(df)

# calculate T*PROG
df <- df %>% mutate(
  TREADPROG = case_when (grepl('%',READCOV,fixed=TRUE) ~
                  round(TELIG * as.numeric(sub("%", "", READCOV))/100,0),
                  .default = NA),
  TWRITPROG = case_when (grepl('%',WRITCOV,fixed=TRUE) ~
                  round(TELIG * as.numeric(sub("%", "", WRITCOV))/100,0),
                  .default = NA),
  TMATPROG = case_when (grepl('%',MATCOV,fixed=TRUE) ~
                  round(TELIG * as.numeric(sub("%", "", MATCOV))/100,0),
                  .default = NA)
)
  # calculate upper and lower ci
df <- df %>% mutate(
  USER_READPROG_LOWER = round(READPROG - (pup_read_sd/sqrt(TREADPROG)*two_tail_p),1),
  USER_READPROG_UPPER = round(READPROG + (pup_read_sd/sqrt(TREADPROG)*two_tail_p),1),
  USER_WRITPROG_LOWER = round(WRITPROG - (pup_writ_sd/sqrt(TWRITPROG)*two_tail_p),1),
  USER_WRITPROG_UPPER = round(WRITPROG + (pup_writ_sd/sqrt(TWRITPROG)*two_tail_p),1),
  USER_MATPROG_LOWER = round(MATPROG - (pup_mat_sd/sqrt(TMATPROG)*two_tail_p),1),
  USER_MATPROG_UPPER = round(MATPROG + (pup_mat_sd/sqrt(TMATPROG)*two_tail_p),1)
)


  # calculate cutoffs for progress bands

# calculate cutoffs by creating a temporary vector
#  recode the NAs and upper >0 / lower < 0 to max/min value
temp_df <- df %>% transmute(
  lowread = case_when(
    READPROG < 0 & USER_READPROG_UPPER < 0 ~ READPROG,
    .default = 999
  ),
  highread = case_when(
    READPROG > 0 & USER_READPROG_LOWER > 0 ~ READPROG,
    .default = -999
  ),
  lowwrit = case_when(
    WRITPROG < 0 & USER_WRITPROG_UPPER < 0 ~ WRITPROG,
    .default = 999
  ),
  highwrit = case_when(
    WRITPROG > 0 & USER_WRITPROG_LOWER > 0 ~ WRITPROG,
    .default = -999
  ),
  lowmat = case_when(
    MATPROG < 0 & USER_MATPROG_UPPER < 0 ~ MATPROG,
    .default = 999
  ),
  highmat = case_when(
    MATPROG > 0 & USER_MATPROG_LOWER > 0 ~ MATPROG,
    .default = -999
  )
)

#  use quantile to find the 10% position to set band
cutoffs = temp_df %>% summarise(
  read_low = quantile(lowread, prob=0.1),
  read_high = quantile(highread, prob=0.9),
  writ_low = quantile(lowwrit, prob=0.1),
  writ_high = quantile(highwrit, prob=0.9),
  mat_low = quantile(lowmat,prob=0.1),
  mat_high = quantile(highmat,prob=0.9)
)

# functions for plotting
limitRange <- function(fun, min, max) {
  function(x) {
    y <- fun(x)
    y[x < min  |  x > max] <- NA
    return(y)
  }
}
draw_range <- function(l, h){
  limitRange(function(x) dnorm(x, 0, pup_read_sd) * all_school_n/10, l, h)
}

#calculate new progress bands
df <- df %>% mutate(
  USER_READPROG_DESCR = case_when(
    USER_READPROG_LOWER > 0 & READPROG >= cutoffs$read_high ~ factor('Well above'),
    USER_READPROG_LOWER > 0 & READPROG > 0 &
      READPROG < cutoffs$read_high ~ factor('Above'),
    USER_READPROG_LOWER <= 0 & USER_READPROG_UPPER >= 0 ~ factor('Average'),
    USER_READPROG_UPPER < 0 & READPROG < 0 &
      READPROG >= cutoffs$read_low ~ factor('Below'),
    USER_READPROG_UPPER < 0 & READPROG < cutoffs$read_low ~ factor('Well below')
  ),
  USER_WRITPROG_DESCR = case_when(
    USER_WRITPROG_LOWER > 0 & WRITPROG >= cutoffs$writ_high ~ factor('Well above'),
    USER_WRITPROG_LOWER > 0 & WRITPROG > 0 &
      WRITPROG < cutoffs$writ_high ~ factor('Above'),
    USER_WRITPROG_LOWER <= 0 & USER_WRITPROG_UPPER >= 0 ~ factor('Average'),
    USER_WRITPROG_UPPER < 0 & WRITPROG < 0 &
      WRITPROG >= cutoffs$writ_low ~ factor('Below'),
    USER_WRITPROG_UPPER < 0 & WRITPROG < cutoffs$writ_low ~ factor('Well below')
  ),
  USER_MATPROG_DESCR = case_when(
    USER_MATPROG_LOWER > 0 & MATPROG >= cutoffs$mat_high ~ factor('Well above'),
    USER_MATPROG_LOWER > 0 & MATPROG > 0 &
      MATPROG < cutoffs$mat_high ~ factor('Above'),
    USER_MATPROG_LOWER <= 0 & USER_MATPROG_UPPER >= 0 ~ factor('Average'),
    USER_MATPROG_UPPER < 0 & MATPROG < 0 &
      MATPROG >= cutoffs$mat_low ~ factor('Below'),
    USER_MATPROG_UPPER < 0 & MATPROG < cutoffs$mat_low ~ factor('Well below')
  ),
# and the fill colours for the schools normal curve chart
  read_band_colour = case_match(
    USER_READPROG_DESCR,
    'Well above' ~ "green",
    'Above' ~ "lightgreen",
    'Average' ~ "yellow",
    'Below' ~ "red",
    'Well below' ~ "darkred",
  ),
  writ_band_colour = case_match(
    USER_WRITPROG_DESCR,
    'Well above' ~ "green",
    'Above' ~ "lightgreen",
    'Average' ~ "yellow",
    'Below' ~ "red",
    'Well below' ~ "darkred"
  ),
  mat_band_colour = case_match(
    USER_MATPROG_DESCR,
    'Well above' ~ "green",
    'Above' ~ "lightgreen",
    'Average' ~ "yellow",
    'Below' ~ "red",
    'Well below' ~ "darkred"
  ),
  read_fun = mapply(
    draw_range, 
    USER_READPROG_LOWER, 
    USER_READPROG_UPPER,
    SIMPLIFY = TRUE),
  writ_fun = Map(
    draw_range, 
    USER_WRITPROG_LOWER, 
    USER_WRITPROG_UPPER),
  mat_fun = Map(
    draw_range, 
    USER_WRITPROG_LOWER, 
    USER_WRITPROG_UPPER)
)

read_display_columns <- c(urn = "URN",
                         schoolname = "SCHNAME",
                         n = "TELIG",
                         read_low = "USER_READPROG_LOWER",
                         read_prog = "READPROG",
                         read_high = "USER_READPROG_UPPER",
                         new_band = "USER_READPROG_DESCR",
                         pub_band = "READPROG_DESCR",
                         fill = "read_band_colour",
                         fun = "read_fun"
                         )
read_data <- df %>% select(all_of(read_display_columns))

shinyApp(
  ui = bootstrapPage(
    theme=bslib::bs_theme(version=5),
    tags$head(
      HTML("<title>Progress visuals</title>"),
      HTML('<link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
<link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
<link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
<link rel="manifest" href="/site.webmanifest">
<link rel="mask-icon" href="/safari-pinned-tab.svg" color="#5bbad5">
<meta name="msapplication-TileColor" content="#da532c">
<meta name="theme-color" content="#ffffff">')),
    div(class="container", 
      div(class="row",
        div(class="col-1",
            p('p-value:')
        ),
        div(class="col-1",
            numericInput(
              "p", 
              NULL, 
              value = conf_level, 
              min = 0, max = 1, step=0.05),
            ),
        ),
      div(class="row", 
        div(class="col",
            # plotOutput('size_x_band'),
            # plotOutput('boxplot')
            plotOutput('plots')
        ),
        div(class="col",
          plotOutput('plt'),),
        div(class="col",
          DTOutput('schlist'),
    ))),
    div(class="container",
      DTOutput('tbl')
    ),
  ),
  server = function(input, output) {
    output$tbl = renderDT(
      datatable(read_data %>% select(-c(fill,fun)),
      class = 'table table-bordered table-sm',
      filter = list(position = 'top', clear = FALSE),
      options = list(dom = 'tpr', lengthChange = FALSE))
    )
    output$schlist = renderDT({
      rows_sel <- input$tbl_rows_selected
      if (length(rows_sel)){
        datatable(
          read_data %>% filter(row_number() %in% rows_sel) %>%
            select(c(urn, schoolname, read_prog, new_band)),
          colnames = c('band' = 'new_band'),
          rownames = F,
          class = 'table-bordered table-sm',
          options = list(dom = 'tr', lengthChange = FALSE, ordering = F),
          selection = 'none'
        )
        }
      })
    size_x_band = ggplot(read_data, aes(x=n, fill=new_band)) +
      geom_histogram(binwidth=10) +
      scale_fill_manual(
        values = c("green","lightgreen","yellow","red","darkred")
      )
    boxplot = ggplot(read_data, aes(
      group=new_band,
      x=new_band,
      y=n,
      fill=new_band)) +
      geom_boxplot() +
      guides(fill=FALSE)
    output$plots = renderPlot({
      size_x_band / boxplot
    })
    range_layers <- reactive({
      s = input$tbl_rows_selected
      if (length(s)) {
#https://stackoverflow.com/questions/15987367/how-to-add-layers-in-ggplot-using-a-for-loop
        read_data[s, , drop=FALSE] %>%
          select(fill,fun) %>%
          pmap(stat_function, geom="area",alpha=0.2)}
    })
    output$plt = renderPlot({
      p <- ggplot(data = read_data, aes(read_prog)) +
        stat_function(fun = function(x) dnorm(
          x, mean=0, sd=pup_read_sd) * all_school_n / 10,
          colour="darkred", linewidth=1) +
        stat_function(fun = function(x) dnorm(
          x, mean=mean(read_data$read_prog, na.rm=TRUE),
          sd=sd(read_data$read_prog,na.rm=TRUE)) * all_school_n / 10, 
          colour="darkblue", linewidth=1) +
        geom_vline(xintercept = c(cutoffs$read_low, cutoffs$read_high), linetype='dashed', colour='blue') +
        geom_text(aes(x=cutoffs$read_low + 0.3, label=cutoffs$read_low, y=0), colour='blue', angle=90) +
        geom_text(aes(x=cutoffs$read_high + 0.3, label=cutoffs$read_high, y=0), colour='blue', angle=90)
      if (!is.null(range_layers())) p + range_layers()
      else p
    })
  }
)

