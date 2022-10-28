#' Plot FSS scores
#'
#' \code{plot_spatial_verif} is used to plot verification scores computed by
#' functions from the harpSpatial package.
#'
#' @export

plot_fss <- function(
  plot_data, 
  score_name="FSS", 
  flip_axes = FALSE, 
  colour_by = "fss", 
  point_size=1.2, 
  line_width = 1, 
  
  ...) {
  
  if (is.null(plot_data)) stop("No data found.")
  if (any(!is.element(c("threshold", "scale", "fss"), names(plot_data)))) {
    stop("plot_data must have columns named threshold, scale and fss !")
  }

  if (colour_by == "fss" || colour_by == "auto" || is.na(colour_by)) {
    colour_by <- "fss"
    x_data <- "threshold"
    y_data <- "scale"
    plot_type = "area"
  } else if (colour_by == "threshold") {
    x_data <- "scale"
    y_data <- "fss"
    plot_type = "line"
  } else if (colour_by == "scale") {
    x_data <- "threshold"
    y_data <- "fss"
    plot_type = "line"
  } else {
    message(paste("colour_by should either be 'scale' or 'threshold'"))
    colour_by <- "fss"
    x_data <- "threshold"
    y_data <- "scale"
    plot_type = "area"
  }

  ### calculate mean of every threshold/scale pair 
  plot_data <- plot_data %>% dplyr::group_by(model,prm,threshold, scale) %>% dplyr::summarize_at("fss", mean, na.rm=TRUE)

  if (plot_type == "area") {
        gg <- ggplot2::ggplot(plot_data, aes(x = get(x_data), y = get(y_data), fill = fss, label = sprintf("%1.2f", fss))) +
        ggplot2::geom_tile() + 
        ggplot2::geom_text(colour = "black", size = 4) +
        ggplot2::scale_fill_gradient2(low = "red", 
                                      mid="yellow", 
                                      high = "darkgreen", 
                                      limits=c(0, 1), 
                                      midpoint=0.5, 
                                      name = score_name)
  }
  if (plot_type == "line") {
        gg <- ggplot2::ggplot(plot_data, aes(x = get(x_data), y = get(y_data), colour = as.character(get(colour_by)))) +
        ggplot2::geom_line(size=line_width) +
        ggplot2::geom_point(size=point_size) +
        ggplot2::labs(colour=str_to_title(colour_by))
  }

  gg <- gg + ggplot2::labs(
                    title=paste("Score: ",score_name,", Model: ",unique(plot_data$model),", Param: ",unique(plot_data$prm)),
                    x = str_to_title(x_data), 
                    y = str_to_title(y_data)
                    )

  ## Other settings
  
  if (flip_axes) {gg <- gg + ggplot2::coord_flip()}

  gg
}