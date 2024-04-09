#' Plotting EMU data at the level and rate of change.
#' @name plot_emu_data
#' @param emu_data A table of EMU data for one chosen type
#' @return A plot of EMU data
#' @import ggplot2
#' @import patchwork
#' @export
plot_emu_data <- function(emu_data){

  country_name <- emu_data %>% pull(name) %>% unique()
  data_type <- emu_data %>% pull(ss_type) %>% unique()

  # plotting EMU data
  emu_plot <- ggplot() +
    geom_point(emu_data, mapping = aes(year, emu)) +
    geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu)) +
    theme_bw() +
    labs(x = "Year", y = "EMU", colour = "Data type", linetype = " ") + ggtitle("EMU over time") + theme(legend.position = "bottom")

  # plotting delta EMU data
  delta_emu_plot <- ggplot() +
    geom_point(emu_data, mapping = aes(year, emu_roc)) +
    geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc)) +
    theme_bw() + labs(x = "Year", colour = "Data type", y = Delta~"EMU", linetype = " ", shape = " ")  + ggtitle("Annual changes in EMU over time")+ theme(legend.position = "bottom")+ guides(colour = "none")

  # combining plots horizontally
  combined_plots <- emu_plot + delta_emu_plot + plot_annotation(title = paste0(country_name, " (", data_type, ")"))


}
