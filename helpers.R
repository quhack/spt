library(readr)
library(dplyr)
library(purrr) # for pmap
library(magrittr)

favicon_html = '<link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
<link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
<link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
<link rel="manifest" href="/site.webmanifest">
<link rel="mask-icon" href="/safari-pinned-tab.svg" color="#5bbad5">
<meta name="msapplication-TileColor" content="#da532c">
<meta name="theme-color" content="#ffffff">'

get_data_from_url_with_filter <- function(
    url, col_types, na_values, filtercol, filtervals){
  df <- read_csv(file = url, col_types=col_types, na=na_values) %>%
    filter(is_in(.data[[filtercol]], filtervals))
  return(df)
}

recode_coltype <- function(x){
  switch(x,
         col_character = col_character(),
         col_integer = col_integer(),
         col_double = col_double(),
         col_character = col_character(),
         col_number = col_number()
  )
}

# take a list of dataset specs
#  (url, coltypes, na values, filtercolumn and filtervalues)
# get the data, apply the transformations and return as a named list of datasets
get_data_from_list <- function(x){
  col_types = lapply(x$col_types, recode_coltype)
  indat <- list()
  indat[['df']] <- get_data_from_url_with_filter(
    url=x$url, col_types=col_types, na_values=x$na, filtercol=x$filtercol,
    filtervals = x$filtervals)
  indat$n = nrow(indat[['df']])
  indat$params <- x
  return(indat)
}

# progress_columnnames_from_subparams <- function(x){
#   columnnames <- list()
#   for (s in x){
#     year_str <- toString(s$year)
#     columnnames[[year_str]] <- c(columnnames[[year_str]],s$prog_col)
#   }
#   return(columnnames)
# }

add_est_total_columns <- function(year_ob){
  # calculate T*PROG
  total_col <- year_ob$params$total_col
  for (sub in year_ob$params$subjects){
    prog_col <- sub$prog_col
    cov_col <- sub$cov_col
    year_ob$df <- year_ob$df %>% mutate(
      !!paste0('T',prog_col) := case_when (
        grepl('%',.[[cov_col]],fixed=TRUE)
        ~round(.[[total_col]] * as.numeric(sub("%","",.[[cov_col]]))/100,0),
        .default = NA)
    )
  }
  return(year_ob)
}

add_confidence_columns <- function(year_ob, ci_vals=list(A = 0.95)){
  # calculate USER_*PROG_LOWER/_UPPER and low_/high_*PROG
  # assert(length(names(ci_vals))>0)
  for (ci_label in names(ci_vals)){
    two_tail_p = two_tail_p = qnorm(1 - ((1 - ci_vals[[ci_label]]) / 2))
    total_col <- year_ob$params$total_col
    for (sub in year_ob$params$subjects){
      prog_col <- sub$prog_col
      pupil_sd <- sub$pupil_sd
      year_ob$df <- year_ob$df %>% mutate(
        !!paste0("USER_",prog_col,"_LOWER_", ci_label) := case_when (
          !is.na(.[[paste0('T',prog_col)]])
          ~round(.[[prog_col]] - (pupil_sd/
                                      sqrt(.[[paste0("T",prog_col)]])*two_tail_p),1),
          .default = NA),
        !!paste0("USER_",prog_col,"_UPPER_", ci_label) := case_when (
          !is.na(.[[paste0('T',prog_col)]])
          ~round(.[[prog_col]] + (pupil_sd/
                                      sqrt(.[[paste0("T",prog_col)]])*two_tail_p),1))
      )
    }
  }
  return(year_ob)
}

# recode progress scores when not significantly different from 0
get_unaverage_progress_scores <- function(year_ob, ci_vals=list(A = 0.95)){
  l <- list()
  for (ci_name in names(ci_vals)){
    for (sub in year_ob$params$subjects){
      prog_col <- sub$prog_col
      l <- c(l, year_ob$df %>% transmute(
        !!paste0('low_',prog_col, '_', ci_name) := case_when(
          (.[[prog_col]] < 0 & .[[paste0('USER_',prog_col,'_UPPER_', ci_name)]] < 0) ~
            .[[prog_col]],
          .default = 999
        ),
        !!paste0('high_',prog_col,'_', ci_name) := case_when(
          (.[[prog_col]] > 0 & .[[paste0('USER_',prog_col,'_LOWER_', ci_name)]] > 0) ~
            .[[prog_col]],
          .default = -999
        )
      ))
    }
  }
  return(bind_cols(l))
}

calc_all_mean_sd <- function(year_ob) {
  prog_colnames <- sapply(year_ob$params$subjects, function(x) x$prog_col)
  s_sd <- year_ob$df %>% summarise_at(prog_colnames, sd, na.rm=TRUE)
  s_mean <- year_ob$df %>% summarise_at(prog_colnames, mean, na.rm=TRUE)
  for (s in names(year_ob$params$subjects)){
    year_ob$params$subjects[[s]]$school_sd <- s_sd[[s]]
    year_ob$params$subjects[[s]]$school_mean <- s_mean[[s]]
  }
  return(year_ob)
}

calc_all_cutoffs <- function(year, indat, progress_data, ci_vals=list(A = 0.95)){
#usage indat <- lapply(names(indat), calc_all_cutoffs, indat = indat, progress_data = temp_progress_values)
#      setNames(indat, names)
  prog_colnames <- sapply(indat[[year]]$params$subjects, function(x) x$prog_col)
  for (ci_name in names(ci_vals)){
    low_colnames <- sapply(prog_colnames, \(x) paste0("low_",x,"_",ci_name))
    high_colnames <- sapply(prog_colnames, \(x) paste0("high_",x, "_", ci_name))
    # note columns are named based on the named list passed to sapply for prog_colnames
    # so this is implicitly the subjects
    l_band <- progress_data[[year]] %>% summarise(across(all_of(low_colnames), \(x) quantile(x, prob=0.1)))
    h_band <- progress_data[[year]] %>% summarise(across(all_of(high_colnames), \(x) quantile(x, prob=0.9)))
    
    for (s in names(indat[[year]]$params$subjects)){
      indat[[year]]$params$subjects[[s]][[paste0('low_band_cutoff_',ci_name)]] <- unname(l_band[[s]])
      indat[[year]]$params$subjects[[s]][[paste0('high_band_cutoff_', ci_name)]] <- unname(h_band[[s]])
    }
  }
  return(indat[[year]])
}

apply_bands <- function(year_ob, ci_vals=list(A = 0.95)){
  for (ci_name in names(ci_vals)){
    for (sub in year_ob$params$subjects){
      prog_col <- sub$prog_col
      lo_co <- sub[[paste0('low_band_cutoff_',ci_name)]]
      hi_co <- sub[[paste0('high_band_cutoff_', ci_name)]]
      lo_id <- paste0("USER_",prog_col,"_LOWER_", ci_name)
      hi_id <- paste0("USER_",prog_col,"_UPPER_", ci_name)
      descr_id <- paste0("USER_",prog_col,"_DESCR_", ci_name)
      fill_id <- paste0("USER_",prog_col,"_fill_", ci_name)
      year_ob$df <- year_ob$df %>% mutate(
        !!descr_id := case_when (
          .[[lo_id]] > 0 &
            .[[prog_col]] >= hi_co ~ factor('Well above'),
          .[[lo_id]] > 0 &
            .[[prog_col]] >= 0 ~ factor('Above'),
          .[[lo_id]] <= 0 &
            .[[hi_id]] >= 0 ~ factor('Average'),
          .[[hi_id]] < 0 &
            .[[prog_col]] >= lo_co ~ factor('Below'),
          .[[hi_id]] < 0 &
            .[[prog_col]] < lo_co ~ factor('Well below')
        ),
        !!fill_id := case_when (
          .[[lo_id]] > 0 &
            .[[prog_col]] >= hi_co ~ "green",
          .[[lo_id]] > 0 &
            .[[prog_col]] >= 0 ~ "lightgreen",
          .[[lo_id]] <= 0 &
            .[[hi_id]] >= 0 ~ "yellow",
          .[[hi_id]] < 0 &
            .[[prog_col]] >= lo_co ~ "red",
          .[[hi_id]] < 0 &
            .[[prog_col]] < lo_co ~ "darkred",
        )
      )
    }
  }
  return(year_ob)
}
############################################################
# functions for plotting
limitRange <- function(fun, min, max) {
  function(x) {
    y <- fun(x)
    y[x < min  |  x > max] <- NA
    return(y)
  }
}

draw_range <- function(l, h, sd, n){
  limitRange(function(x) dnorm(x, 0, sd) * n, l, h)
}
############################################################

add_graph_func <- function(year_ob, ci_vals=list(A = 0.95)){
  n <- year_ob$n
  for (ci_label in names(ci_vals)){
    for (sub in year_ob$params$subjects){
      pup_read_sd <- sub$pupil_sd
      prog_col = sub$prog_col
      year_ob$df <- year_ob$df %>% mutate(
        !!paste0("USER_", prog_col,"_fun_",ci_label) := mapply(
          draw_range, 
          .[[paste0("USER_", prog_col, "_LOWER_", ci_label)]],
          .[[paste0("USER_", prog_col, "_UPPER_", ci_label)]],
          pup_read_sd,
          n,
          SIMPLIFY = TRUE),
      )
    }
  }
  return(year_ob)
}

get_subject_column_names <- function(x){
  col_names <- list()
  for (s in x$params$subjects){
    col_names <- c(col_names, s$displ_cols)
  }
  return(col_names)
}

generate_display_data <- function(year_ob){
  columnnames <- unname(unlist(year_ob$params$displ_cols))
  columnnames <- c(columnnames, unname(unlist(get_subject_column_names(year_ob))))
  return(year_ob$df %>% select(all_of(columnnames)))
}

## selections = selected_schools
generate_progress_plots <- function(year_ob, selections, ci_vals=list(A = 0.95)){
  all_school_n = year_ob$n
  g <- function(subject_params, data, v, p) {
    prog_col <- subject_params$prog_col
    chart_title <- subject_params$chart_title
    if (length(selections)) {
      #https://stackoverflow.com/questions/15987367/how-to-add-layers-in-ggplot-using-a-for-loop
      range_layers <- data[selections, , drop=FALSE] %>%
      select(fill := paste0("USER_",prog_col, "_fill_", v),
             fun := paste0("USER_", prog_col, "_fun_", v)) %>%
      pmap(stat_function, geom="area",alpha=0.2)}

    f_dnorm <- function(x, m, sd) dnorm(x, mean=m, sd=sd) * all_school_n
    c <- ggplot(data = data, aes(.data[[prog_col]])) +
    stat_function(fun = f_dnorm, args = list(m = 0, sd = subject_params$pupil_sd),
      colour="darkred", linewidth = 1) +
      stat_function(fun = f_dnorm, args = list(m = subject_params$school_mean,
        sd=subject_params$school_sd),
        colour="darkblue", linewidth=1) +
      geom_vline(xintercept = c(subject_params[[paste0('low_band_cutoff_',v)]],
                                subject_params[[paste0('high_band_cutoff_',v)]]),
                 linetype='dashed', colour='blue') +
      geom_text(aes(x=subject_params[[paste0('low_band_cutoff_', v)]] + 0.3,
                    label=subject_params[[paste0('low_band_cutoff_', v)]], y=0),
                colour='blue', angle=90) +
      geom_text(aes(x=subject_params[[paste0('high_band_cutoff_', v)]] + 0.3,
                    label=subject_params[[paste0('high_band_cutoff_', v)]], y=0),
                colour='blue', angle=90) +
      coord_cartesian (xlim = c(-20,20), ylim = c(0,1200)) +
      ggtitle(paste0(chart_title, ' (p=',p,')'))
    if (exists("range_layers")) c + range_layers
    else c
  }
  output_charts = list()
  for (ci_label in names(ci_vals)){
    output_charts[[ci_label]] <- lapply(year_ob$params$subjects,
                                            g,
                                            data=year_ob$df,
                                            v=ci_label,
                                            p=ci_vals[[ci_label]])
  }
  return(output_charts)
}

generate_size_x_band <- function(year_ob, ci_vals=list(A = 0.95)){
  g <- function(subject_params, data,v, p){
    prog_col <- subject_params$prog_col
    band_col <- paste("USER_",prog_col,"_DESCR_", v, sep="")
    p <- ggplot(data, aes(x = TELIG, fill = .data[[band_col]])) +
      geom_histogram(binwidth=10) +
      scale_fill_manual(
        values = c("green","lightgreen","yellow","red","darkred")
      ) +
      ggtitle(paste0(subject_params$chart_title, ' (p=',p,')'))
    return(p)
  }
  # return(lapply(year_ob$params$subjects, g, data=year_ob$df))
  output_charts = list()
  for (ci_label in names(ci_vals)){
    output_charts[[ci_label]] <- lapply(year_ob$params$subjects,
                                        g,
                                        data=year_ob$df,
                                        v=ci_label,
                                        p=ci_vals[[ci_label]])
  }
  return(output_charts)
}

generate_boxplot <- function(year_ob, ci_vals=list(A = 0.95)){
  g <- function(subject_params, data, v, p){
    prog_col <- subject_params$prog_col
    band_col <- paste("USER_",prog_col,"_DESCR_", v, sep="")
    p <- ggplot(data, aes(x = .data[[band_col]], y = TELIG,
                                            group = .data[[band_col]],
                                            fill = .data[[band_col]])) +
      geom_boxplot() +
      guides(fill="none") +
      ggtitle(paste0(subject_params$chart_title, ' (p=',p,')'))
    return(p)
  }
  # return(lapply(year_ob$params$subjects, g, data=year_ob$df))
  output_charts = list()
  for (ci_label in names(ci_vals)){
    output_charts[[ci_label]] <- lapply(year_ob$params$subjects,
                                        g,
                                        data=year_ob$df,
                                        v=ci_label,
                                        p=ci_vals[[ci_label]])
  }
  return(output_charts)
}

