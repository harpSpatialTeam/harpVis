#' Plot scores from neighborhood based contingency tables
#' based on Stein and Stoop article, 2019 Jan
#' Neighborhood-Based Contingency Tables Includig Errors Compensation
#'
#' \code{plot_spatial_verif} is used to plot verification scores computed by
#' functions from the harpSpatial package.
#'
#' @param score_name In this context, table name
#' @param nact_scores Actual score to plot. Must be included in plot_opts()
#'
#'
#' @export

plot_nact <- function(
  plot_data, 
  score_name="NACT", 
  point_size=1.2, 
  extend_y_to_zero = TRUE, 
  line_width = 1, 
  y_label = "", 
  x_label = "", 
  flip_axes = FALSE, 
  nact_scores = list("all"), 
  colour_by = "scale", 
  
  ...) {
  
  if (is.null(plot_data)) stop("No data found.")
  if (any(!is.element(c("threshold", "scale", "a", "b", "c", "d"), names(plot_data)))) stop("plot_data must have columns named threshold, scale, a, b, c and d!")
  if ("all" %in% nact_scores || is.na(nact_scores) || length(nact_scores) < 1) {
    nact_scores <- list("fbias","pod","far","pss","hss","sedi") #list all available scores
  } 
  if (colour_by == "scale" || colour_by == "auto") {
    colour_by <- "scale"
    x_data <- "threshold"
  } else if (colour_by == "threshold") {
    x_data <- "scale"
  } else {
    message(paste("colour_by should either be 'scale' or 'threshold'"))
    colour_by <- "scale"
    x_data <- "threshold"
  }

  message("Plotting score: ",paste(nact_scores, collapse=", "))

  plot_data <- plot_data %>% dplyr::group_by(model,prm,threshold,scale) %>% dplyr::summarise(a = mean(a,na.rm=TRUE),b = mean(b,na.rm=TRUE),c = mean(c,na.rm=TRUE),d = mean(d,na.rm=TRUE))

  if ("fbias" %in% nact_scores) {
      nact_score = "fbias" # Frequency bias
      plot_data[nact_score] <- (plot_data$a+plot_data$b)/(plot_data$a+plot_data$c)
  }
  if ("pod" %in% nact_scores) {
      nact_score = "pod" # Probability of detection
      plot_data[nact_score] <- plot_data$a/(plot_data$a+plot_data$c)
  }
  if ("far" %in% nact_scores) {
      nact_score = "far" # False-alarm ratio
      plot_data[nact_score] <- plot_data$b/(plot_data$a+plot_data$b)
  }
  if ("pss" %in% nact_scores) {
      nact_score = "pss" # Pierce skill score
      plot_data[nact_score] <- (plot_data$a/(plot_data$a+plot_data$b)-(plot_data$b/(plot_data$b+plot_data$d)))
  }
  if ("hss" %in% nact_scores) {
      nact_score = "hss" # Heidke skill score
      Ar <- ((plot_data$a+plot_data$b)*(plot_data$a+plot_data$c))/(plot_data$a+plot_data$b+plot_data$c+plot_data$d) 
      Dr <- ((plot_data$b+plot_data$d)*(plot_data$c+plot_data$d))/(plot_data$a+plot_data$b+plot_data$c+plot_data$d) 
      plot_data[nact_score] <- ((plot_data$a+plot_data$d-Ar-Dr)/(plot_data$a+plot_data$b+plot_data$c+plot_data$d-Ar-Dr))
  }
  if ("sedi" %in% nact_scores) {
      nact_score = "sedi" #Symmetric extremal dependency index
      plot_data[nact_score] <- (
      (
      log(plot_data$b/(plot_data$b+plot_data$d)) - 
      log(plot_data$a/(plot_data$a+plot_data$c)) +
      log(plot_data$c/(plot_data$a+plot_data$c)) -
      log(plot_data$d/(plot_data$b+plot_data$d))
      ) / 
      (
      log(plot_data$b/(plot_data$b+plot_data$d)) + 
      log(plot_data$a/(plot_data$a+plot_data$c)) +
      log(plot_data$c/(plot_data$a+plot_data$c)) +
      log(plot_data$d/(plot_data$b+plot_data$d))
      )
      )
  }

  ## At this point we will have a table of N-number of scores (columns) from above
  plot_data <- plot_data %>% gather("score","value",c(paste(nact_scores))) #convert to long table for facet_wrap
  plot_data$value <- replace(plot_data$value,is.na(plot_data$value),NA) # NaN to NA
  plot_data$value <- replace(plot_data$value,is.infinite(plot_data$value),NA) # Inf to NA
  
  gg <- ggplot2::ggplot(plot_data, aes(x = get(x_data), y = value, colour = as.character(get(colour_by)))) + 
        ggplot2::geom_line(size=line_width) +
        ggplot2::geom_point(size=point_size) + 
        ggplot2::labs(title=paste("Score: ",toupper(score_name),", Param: ",unique(plot_data$prm)), y = y_label, x = x_label, colour=str_to_title(colour_by)) + 
        facet_wrap(. ~ score, ncol = 3)
  
  ## Other settings
  
  if (extend_y_to_zero) {gg <- gg + ggplot2::ylim(-0.025,NA)}
  if (flip_axes) {gg <- gg + ggplot2::coord_flip()}
  if (y_label == "auto") {gg <- gg + ggplot2::labs(y=str_to_title("value"))}
  if (x_label == "auto") {gg <- gg + ggplot2::labs(x=str_to_title(x_data))}

  gg

}