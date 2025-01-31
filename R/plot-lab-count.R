#' Plot LAB treatment counts
#' @description Creates a column plot of LAB treatment counts
#' @param dt data.table containing count data
#' @param fill_var Optional character name of variable to use for fill colors
#' @return ggplot object
#' @details Uses afcharts styling
#' @export
plot_lab_count <- function(dt, fill_var = NULL) {
  afcharts::use_afcharts()

  p <- dt |>
    ggplot2::ggplot(aes(x = year, y = phbudi_any)) +
    ggplot2::geom_col(aes(fill = if (!is.null(fill_var)) get(fill_var))) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme(
      legend.position = if (is.null(fill_var)) "none" else "right",
      plot.title.position = "plot"
    )

  return(p)
}
