#' Plotting EMU data at the level and rate of change.
#' @name plot_emu_data
#' @param emu_data A table of EMU data for one chosen type
#' @param mcpr_data A table of mCPR run data
#' @return A plot of EMU data
#' @import ggplot2
#' @import patchwork
#' @export
plot_emu_data <- function(emu_data, mcpr_data){

  mcpr_data <- mcpr_data %>% filter(mcpr > 0)

  country_name <- emu_data %>% pull(name) %>% unique()
  data_type <- emu_data %>% pull(ss_type) %>% unique()
  region_name <- emu_data %>% pull(Region) %>% unique()

  emu_years <- emu_data %>% pull(year)

  if(length(region_name) == 0 | is.na(region_name)){
    plot_title <- paste0(country_name, " (", data_type, ")")
  }
  else{
    plot_title <- paste0(region_name, " (", data_type, ")")
  }


  if(nrow(mcpr_data) == 0){
    # plotting EMU data
    emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu)) +
      #geom_line(mcpr_plot_data, mapping = aes(year, mcpr, linetype = "FPET2 mCPR")) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu)) +
      theme_bw() +
      labs(x = "Year", y = "EMU", colour = "Data type", linetype = " ") + ggtitle("EMU over time") + theme(legend.position = "bottom")

    # plotting delta EMU data
    delta_emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu_roc)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc)) +
      #geom_line(mcpr_plot_data, mapping = aes(year, mcpr_roc, linetype = "annual change in mCPR")) +
      theme_bw() + labs(x = "Year", colour = "Data type", y = Delta~"EMU", linetype = " ", shape = " ")  + ggtitle("Annual changes in EMU over time")+ theme(legend.position = "bottom")+ guides(colour = "none")

  }

  else {
    mcpr_plot_data <- mcpr_data %>% mutate(mcpr_roc = mcpr - lag(mcpr)) %>% filter(year %in% emu_years)

    # plotting EMU data
    emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu)) +
      geom_line(mcpr_plot_data, mapping = aes(year, mcpr, linetype = "FPET2 mCPR"), colour = "blue") +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu)) +
      theme_bw() +
      labs(x = "Year", y = "EMU", colour = "Data type", linetype = " ") + ggtitle("EMU over time") + theme(legend.position = "bottom")

    # plotting delta EMU data
    delta_emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu_roc)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc)) +
      geom_line(mcpr_plot_data, mapping = aes(year, mcpr_roc, linetype = "annual change in mCPR"), colour = "blue") +
      theme_bw() + labs(x = "Year", colour = "Data type", y = Delta~"EMU", linetype = " ", shape = " ")  + ggtitle("Annual changes in EMU over time")+ theme(legend.position = "bottom")+ guides(colour = "none")
  }
  # combining plots horizontally
  combined_plots <- emu_plot + delta_emu_plot + plot_annotation(title = plot_title)


  return(combined_plots)
}
