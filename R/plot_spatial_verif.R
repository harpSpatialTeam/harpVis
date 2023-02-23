#' Plot spatial verification scores
#'
#' @param verif_data Output from \link[harpSPatial]{spatial_verify}
#' @param score The score to plot.
#' @param filter_by Filter the data before plotting. Must be wrapped inside the
#'   \link[dplyr]{vars} function. This can be useful for making a single plot
#'   where there are many groups. For example, if the data contains various models
#'   the data can be filtered with
#'   e.g. \code{filter_by = vars(det_model == "be13", fctime == 0)}.
#' @param show_data Shows input structure before plotting
#' @param plot_opts A list of plotting options that may be passed from outside, some may not be used
#' @export

plot_spatial_verif <- function(
  verif_data,
  score,
  filter_by = NULL,
  show_data = FALSE,
  plot_opts = list(),
  colour_theme = "bw",
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  legend_position = "right",
  tag_position = "left",
  num_legend_rows = NULL,
  plot_caption = "",
  leadtimes_by = "hours",
  
  ...) {

  score_quo  <- rlang::enquo(score)
  score_name <- rlang::quo_name(score_quo)

  ################
  if (!is.object(verif_data) && is.character(verif_data)) {
    if (grepl(".sqlite",verif_data)) {
      message("Input is an .sqlite, not an object.")
      message(verif_data)
      sql_object <- harpIO:::dbopen(verif_data)
      verif_data <- as.data.frame(harpIO:::dbquery(sql_object,paste("SELECT * FROM ",score_name)))
      harpIO:::dbclose(sql_object)
    }
  }
  ################

  # the column(s) to filter data for before plotting
  filter_by_err  <- paste("filter_by must be wrapped in vars and unquoted,\n",
    "e.g. filter_by = vars(leadtime == 12, threshold == 280).")
  filter_by_null <- try(is.null(filter_by), silent = TRUE)
  filter_vars    <- ""
  if (inherits(filter_by_null, "try-error")) {
    stop(filter_by_err, call. = FALSE)
  } else {
    if (filter_by_null) {
      filtering <- FALSE
    } else {
      if (inherits(filter_by, "quosures")) {
        filtering <- TRUE
        filter_vars <- names(purrr::map_chr(rlang::eval_tidy(filter_by), rlang::quo_name))
      } else {
        stop(filter_by_err, call. = FALSE)
      }
    }
  }

  # select the right data set if verif_data is a list of tables

  if (!is.data.frame(verif_data) && is.list(verif_data)) {
    # If list of tables, select the table of selected score,
    # each score has it's own table
    plot_data <- as_tibble(verif_data$score_name)
  } else {
    plot_data <- as_tibble(verif_data)
  }

  if (any(!is.element(c("fcdate","leadtime"),names(plot_data)))) {
    bdate <- NULL
    edate <- NULL
    message("columns named fcdate and leadtime are missing!")
  } else {
    plot_data <- plot_data %>% mutate(fcdates = plot_data$fcdate+plot_data$leadtime) #forecast datetime
    plot_data$fcdate <- lubridate::as_datetime(plot_data$fcdate, origin = lubridate::origin, tz="UTC")
    plot_data$fcdates <- lubridate::as_datetime(plot_data$fcdates, origin = lubridate::origin, tz="UTC")
    bdate <- strftime(min(plot_data$fcdates, na.rm=TRUE),format = "%d-%m-%Y %H:%M")
    edate <- strftime(max(plot_data$fcdates, na.rm=TRUE),format = "%d-%m-%Y %H:%M")
    valid_hours <- unique(lubridate::hour(plot_data$fcdate))
  }

  if (is.element("leadtime", names(plot_data))) {
    if (leadtimes_by == "hours") {
       ldtconv = 3600
    } else if (leadtimes_by == "minutes") {
       ldtconv = 60
    } else { ldtconv = 1}
    plot_data <- plot_data %>% dplyr::mutate(leadtime = leadtime / ldtconv) # convert seconds to hours or minutes?
  }

  # apply filtering
  if (filtering) {
    plot_data <- dplyr::filter(plot_data, !!! filter_by)
  }

  if (is.element("leadtime",names(plot_data)) && length(unique(plot_data$leadtime)) > 1) {
    message("Multiple leadtimes found: ", paste(unique(plot_data$leadtime),collapse=" "))
  }

  # at this point: only 1 model and 1 prm should be in the data
  # in fact, the function should also allow input data without these columns!
  if (is.element("model", names(plot_data))) {
    myModel <- unique(plot_data$model)
    if (length(myModel) > 1) stop("You are mixing several models in a single SAL plot.")
  } else {
    myModel <- NULL
  }

  if (is.element("prm", names(plot_data))) {
    myParam <- unique(plot_data$prm)
    if (length(myParam) > 1) stop("You are mixing several parameters in a single SAL plot.")
  } else {
    myParam <- NULL
  }

  if (show_data) {
      message("=========================================================================")
      print(verif_data)
      message("=========================================================================")
      message("Score name: ",score_name)
      print(plot_data,n=Inf)
      message("Start date: ",bdate)
      message("End date: ",edate)
      message("Model: ",myModel)
      message("Parameter: ",myParam)
      message("Plotting options: ")
      print(plot_opts)
      message("=========================================================================")
  }

  ########## PLOTTING FUNCTION SELECTION  
  myPlotFunc = spatial_scores(score=score_name)$plot_func
  gg <- do.call(myPlotFunc,c(list(plot_data,score_name),plot_opts))
  
  ### Plot background, stolen from plot_point_verif

  if (is.function(colour_theme)) {
    theme_func <- colour_theme
  } else {
    if (!grepl("^theme_[[:alpha:]]", colour_theme)) colour_theme <- paste0("theme_", colour_theme)
    if (grepl("harp", colour_theme)) {
      function_env <- "harpVis"
    } else {
      function_env <- "ggplot2"
    }
    theme_func <- get(colour_theme, envir = asNamespace(function_env))
  }

  gg <- gg + theme_func(
    base_size      = base_size,
    base_family    = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  if (!is.null(legend_position) && !is.null(num_legend_rows)) {
      gg <- gg + ggplot2::theme(legend.position = legend_position,
                                plot.tag.position = tag_position,
                                )
      gg <- gg + ggplot2::guides(
        colour   = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
        shape    = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
        fill     = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
        linetype = ggplot2::guide_legend(title = NULL)
      )
  }
  ####

  plot_subtitle <- paste("Period:",bdate,"-",edate,":: Hours {",paste(c(sprintf("%02d", valid_hours)), collapse=", "),"}")

  gg <- gg + ggplot2::labs(subtitle=plot_subtitle, caption=plot_caption)

  # finished
  message("Plots complete!")
  print(gg)
}
