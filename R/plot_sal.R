#' Plot SAL scores
#'
#' \code{plot_spatial_verif} is used to plot verification scores computed by
#' functions from the harpSpatial package.
#'
#' @export

plot_sal <- function(
  plot_data, 
  score_name="SAL", 
  point_size=4, 
  extend_y_to_zero = TRUE, 
  line_width = 1, 
  line_colour= "grey80", 
  quants=seq(0.25,0.75,0.05), 
  xylim = 2, 
  
  ...) {
  
  if (is.null(plot_data)) stop("No data found.")
  # check that you have columns S, A, L
  if (any(!is.element(c("S", "A", "L"), names(plot_data)))) stop("plot_data must have columns named S, A and L !")

  message("Plotting score: ",score_name)
  
  ### Assumption is made that days without precipitation will have NA values, 
  ### since it does not work well with a large majority of zero values 
  ### when there are neither observed or forecasted precipitation

  S_percs <- quantile(plot_data$S, probs = quants, na.rm=TRUE)
  A_percs <- quantile(plot_data$A, probs = quants, na.rm=TRUE)
  #L_percs <- quantile(plot_data$L, probs = quants, na.rm=TRUE)

  medianS <- median(plot_data$S, na.rm=TRUE)
  medianA <- median(plot_data$A, na.rm=TRUE)
  medianL <- median(plot_data$L, na.rm=TRUE)

  meanS <- mean(plot_data$S, na.rm=TRUE)
  meanA <- mean(plot_data$A, na.rm=TRUE)
  meanL <- mean(plot_data$L, na.rm=TRUE)
  
  gg <- ggplot2::ggplot(plot_data, aes(x = S, y = A, colour = L))
  gg <- gg + 
            ggplot2::geom_point(size=point_size) + 
            ggplot2::annotate("rect", xmin = min(S_percs),xmax = max(S_percs), ymin =  min(A_percs),ymax = max(A_percs), fill = "grey80", alpha = 0.3) +
            ggplot2::geom_hline(yintercept = 0, colour = line_colour) + 
            ggplot2::geom_vline(xintercept = 0, colour = line_colour) +
            ggplot2::geom_hline(yintercept = medianA, linetype = "dashed", colour = line_colour) + 
            ggplot2::geom_vline(xintercept = medianS, linetype = "dashed", colour = line_colour) +
            ggplot2::xlim(-xylim, xylim) + ggplot2::ylim(-xylim, xylim) + 
            ggplot2::labs(y = "A", x = "S", colour="L") +
            ggplot2::scale_colour_gradient2(low = "darkblue", mid = "yellow", high = "red") +
            ggplot2::labs(
            title=paste("Score: ",score_name,", Model: ",unique(plot_data$model),", Param: ",unique(plot_data$prm))) + coord_cartesian(clip="off")
            
            # ggplot2::labs(
            # title=paste("Score: ",score_name,", Model: ",unique(plot_data$model),", Param: ",unique(plot_data$prm)),
            # tag=paste(
            # "S median = ",sprintf("%.04f",medianS),
            # "\nA median = ",sprintf("%.04f",medianA),
            # "\nL median = ",sprintf("%.04f",medianL),
            # "\n ",
            # "\nS mean = ",sprintf("%.04f",meanS),
            # "\nA mean = ",sprintf("%.04f",meanA),
            # "\nL mean = ",sprintf("%.04f",meanL)
            # )

  gg
}