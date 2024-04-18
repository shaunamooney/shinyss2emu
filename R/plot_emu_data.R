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
      geom_point(emu_data, mapping = aes(year, emu, colour = "EMU with standard deviation bars")) +
      #geom_line(mcpr_plot_data, mapping = aes(year, mcpr, linetype = "FPET2 mCPR")) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu, colour = "EMU with standard deviation bars")) +
      theme_bw() +
      labs(x = "Year", y = "EMU", colour = "Data type", linetype = " ", caption = , caption = "Left visual shows both EMU with uncertainty (shown using standard deviation error bars). \n The uncertainty associated with EMU is due to the uncertainty \nassociated with the private sector adjustment factor in the SS-to-EMU calculation process.") + ggtitle("EMU over time") + theme(legend.position = "bottom")

    # plotting delta EMU data
    delta_emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu_roc)) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc)) +
      #geom_line(mcpr_plot_data, mapping = aes(year, mcpr_roc, linetype = "annual change in mCPR")) +
      theme_bw() + labs(x = "Year", colour = "Data type", y = Delta~"EMU", linetype = " ", shape = " ", caption = "Right visual shows rates of change in EMU with uncertainty (shown using standard deviation error bars).\n Rate of change refers to the annual difference between observations. \nFor example, the EMU rate of change in 2020 is the difference between the 2020 EMU and 2019 EMU.")  + ggtitle("Annual changes in EMU over time")#+ theme(legend.position = "bottom")

  }

  else {
    mcpr_plot_data <- mcpr_data %>% filter(year %in% emu_years) %>% mutate(mcpr_roc = mcpr - lag(mcpr))

    # plotting EMU data
    emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu, colour = "EMU with \nstandard deviation bars")) +
      geom_line(mcpr_plot_data, mapping = aes(year, mcpr, linetype = "mCPR (FPET2)"), colour = "blue") +
      geom_point(mcpr_plot_data, mapping = aes(year, mcpr), colour = "blue") +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu - sd_emu, ymax = emu + sd_emu, colour = "EMU with \nstandard deviation bars"), width = 0.5) +
      theme_bw() +
      labs(x = "Year", y = "EMU", colour = " ", linetype = " ", caption = "Left visual shows EMU with uncertainty and FPET2 mCPR over time. EMU uncertainty is due to the\nuncertainty associated with the private sector adjustment in the SS-to-EMU calculation process.") +
      ggtitle("EMU and mCPR over time") + theme(legend.position = "bottom")

    # plotting delta EMU data
    delta_emu_plot <- ggplot() +
      geom_point(emu_data, mapping = aes(year, emu_roc, colour = "Rates of change in EMU \n(with standard deviation bars)")) +
      geom_errorbar(emu_data, mapping = aes(x = year, ymin = emu_roc - sd_emu_roc, ymax = emu_roc + sd_emu_roc, colour = "Rates of change in EMU \n(with standard deviation bars)"), width = 0.5) +
      geom_line(mcpr_plot_data, mapping = aes(year, mcpr_roc, linetype = "Rates of change in mCPR (FPET2)"), colour = "blue") +
      geom_point(mcpr_plot_data, mapping = aes(year, mcpr_roc), colour = "blue") +
      theme_bw() + labs(x = "Year", colour = " ", y = Delta~"EMU", linetype = " ", shape = " ", caption = "Right visual shows rates of change in EMU with uncertainty and rates of change in FPET2 mCPR\nover time. Rate of change refers to the annual difference between observations. For example, the\nEMU rate of change in 2020 is the difference between the 2020 EMU and 2019 EMU.")  + ggtitle("Rates of change in EMU and mCPR over time")+ theme(legend.position = "bottom")
  }
  # combining plots horizontally
  combined_plots <- emu_plot + delta_emu_plot + plot_annotation(title = plot_title)


  return(combined_plots)
}
