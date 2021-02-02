#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_systematic <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family="sans",color = "black",size = 9),
      panel.spacing = unit(2, "lines"),
      # title
      plot.title = element_text(hjust = 0.5,size = 9),
      # axes
      axis.line=element_blank(),
      axis.ticks=element_blank(),
      # legend
      legend.position="bottom",
      legend.title = element_text(size = 9,vjust = +1.0),
      legend.key.size = unit(0.3, "cm"),
      legend.key.width = unit(0.7,"cm") ,
      # background colors
      panel.background=element_blank(),
      panel.border=element_rect(colour = "black",fill = "transparent"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      # keep edge black facet_wrap
      # strip.background = element_rect(fill="white"),
      strip.text =element_text(color = "black",size = 8)
    )
}
