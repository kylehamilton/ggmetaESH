#' Funnel Plot
#'
#' Generate a funnel plot
#'
#' @param model metafor rma model
#' @param xlabel x-axis label
#' @param ylabel y-axis label
#'
#' @import ggplot2
#' @import tibble
#' @import tidyr
#'
#'
#' @export ggfunnelESH

ggfunnelESH <- function(model, xlabel, ylabel) {
  CRIT_95 <- 1.96
  CRIT_99 <- 2.58

  d <- tibble(se = sqrt(model$vi), es = model$yi)
  center <- mean(d$es)
  xlabel <- xlabel
  ylabel <- ylabel

  lower_lim <- max(d$se) + .05 * max(d$se)
  funnel95 <- data.frame(
    x = c(center - lower_lim * CRIT_95, center,
          center + lower_lim * CRIT_95),
    y = c(-lower_lim, 0,-lower_lim)
  )

  left_lim99 <- ifelse(center - lower_lim * CRIT_99 < min(d$es),
                       center - lower_lim * CRIT_99,
                       min(d$es))
  right_lim99 <- ifelse(center + lower_lim * CRIT_99 > max(d$es),
                        center + lower_lim * CRIT_99,
                        max(d$es))
  funnel99 <- data.frame(
    x = c(center - lower_lim * CRIT_99, center,
          center + lower_lim * CRIT_99),
    y = c(-lower_lim, 0,-lower_lim)
  )


  ggplot(d, aes(x = es, y = -se)) +
    scale_x_continuous(limits = c(left_lim99, right_lim99)) +
    scale_y_continuous(
      labels = function(x) {
        abs(x)
      }
    ) +
    geom_polygon(aes(x = x, y = y),
                 data = funnel95,
                 alpha = .5,
                 fill = "white") +
    geom_polygon(aes(x = x, y = y),
                 data = funnel99,
                 alpha = .5,
                 fill = "white") +
    geom_vline(xintercept = center,
               linetype = "dotted",
               color = "black") +
    # geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
    geom_point() +
    xlab(xlabel) +
    ylab(ylabel) +
    geom_text(
      x = center + lower_lim * CRIT_95,
      y = -lower_lim + lower_lim / 60,
      label = "p < .05",
      vjust = "bottom",
      hjust = "center"
    ) +
    geom_text(
      x = center + lower_lim * CRIT_99,
      y = -lower_lim + lower_lim / 60,
      label = "p < .01",
      vjust = "bottom",
      hjust = "center"
    ) +
    theme(
      panel.background = element_rect(fill = "grey"),
      panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
      panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5)
    )
}
