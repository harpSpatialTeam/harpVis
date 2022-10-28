#' Plot "basic" scores that have only one value column,
#' such as mse, bias, mae etc
#'
#' \code{plot_spatial_verif} is used to plot verification scores computed by
#' functions from the harpSpatial package.
#'
#' @param score_name Name of the score to plot
#'
#'
#' @export

plot_basic <- function(
  plot_data, 
  score_name, 
  point_size=1.2, 
  extend_y_to_zero = TRUE, 
  line_width = 1, 
  y_label = "", 
  x_label = "Forecast length", 
  flip_axes = FALSE, 
  plot_num_cases = FALSE, 

  ...) {
  
  if (is.null(plot_data)) stop("No data found.")
  if (is.null(score_name)) stop("No score_name given.")
  if (any(!is.element(score_name, names(plot_data)))) {
    stop("plot_data must have columns named ",score_name," !")
  }

  plot_data[score_name] <- c(runif(length(plot_data[score_name]),0,2)) # for testing purposes generate random numbers instead of NA

  message("Plotting score: ",score_name)
  ### grouping across all fcdates by leadtime,parameter and model
  plot_data <- plot_data %>% group_by(model,prm,leadtime) %>% summarise(score_name = mean(get(score_name),na.rm=TRUE))
  ###

  gg <- ggplot2::ggplot(plot_data, aes(x = leadtime, y = score_name, colour = model)) +
  ggplot2::geom_line(size=line_width) + 
  ggplot2::geom_point(size=point_size) + 
  ggplot2::scale_x_continuous(breaks = seq(min(plot_data$leadtime),na.rm=TRUE, max(plot_data$leadtime,na.rm=TRUE), by = plot_data$leadtime[2]-plot_data$leadtime[1])) +
  ggplot2::labs(title=paste("Score: ",str_to_title(score_name),", Param: ",unique(plot_data$prm)), y = y_label, x = x_label, colour="Model")
  
  ## Other settings
  
  if (extend_y_to_zero) {gg <- gg + ggplot2::ylim(-0.025,NA)}
  if (flip_axes) {gg <- gg + ggplot2::coord_flip()}
  if (y_label == "auto") {gg <- gg + ggplot2::labs(y="")}
  if (x_label == "auto") {gg <- gg + ggplot2::labs(x="Forecast length")}

  gg
}