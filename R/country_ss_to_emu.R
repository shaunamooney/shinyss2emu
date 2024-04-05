#' Calculating EMU from excel spreadsheet using modelled supply-share estimates, and saving output in specified files
#' @name country_ss_to_emu
#' @param country_tools_info Country tools info (get_tools_info() output)
#' @param incl_condoms include condoms in EMU output or not
#' @import tidyverse
#' @import stringr
#' @export

country_ss_to_emu <- function(country_tools_info, method_summary = FALSE, shiny_input_type = NULL){

  ss_tools_info <- country_tools_info
  country_emu_df <- list()
  fixed_country_emu_df <- list()

  if(is.null(shiny_input_type)){
    ss_data_types <- c("Contraceptive commodities distributed to clients",
                       "Contraceptive commodities distributed to facilities",
                       "FP visits",
                       "FP users")
  }

  else{

    ss_data_types <- ifelse(shiny_input_type == "Clients", "Contraceptive commodities distributed to clients",
                       ifelse(shiny_input_type == "Facilities", "Contraceptive commodities distributed to facilities",
                              ifelse(shiny_input_type == "Users", "FP users",
                                     ifelse(shiny_input_type == "Visits", "FP visits", "Unknown"))))

  }

  for(s in ss_data_types) {

    ss_quantity_data <- ss_tools_info$ss_quantity_data %>% filter(ss_type == s)
    pop_dataset <- ss_tools_info$pop_dataset
    setup_data <- ss_tools_info$setup_data
    recode_sectors_reporting <- ss_tools_info$recode_sectors_reporting
    recode_scaleup_table <- ss_tools_info$recode_scaleup_table
    ss_info <- ss_tools_info$ss_info %>% filter(ss_type == s)
    cyp_table <- ss_tools_info$cyp_table %>% filter(ss_type == s)
    reporting_rates <- ss_tools_info$reporting_rates_table %>% filter(ss_type == s)
    long_term_rates <- ss_tools_info$method_continuation_data %>%
      mutate_at(vars(-(1:2)), as.numeric)

    user_input_adjustment_table <- ss_tools_info$user_input_adjustment_table
    include_condoms_df <- ss_tools_info$include_condoms_df %>% filter(ss_type == s)

    Country <- setup_data$Country

    cyp_table_clean <- cyp_table %>%
      mutate(method_type = ifelse(method_type == "long", "Long", "Short")) %>%
      select(-ss_type, -relative_sd)

    if(nrow(ss_quantity_data) == 0){
      next
    }

    ss_data <- left_join(ss_quantity_data, cyp_table_clean, by = c("method_detail"))

    ss_data <- ss_data %>%
      select(method_type, method_overview, method_detail, cyp_factor, cyp_factor_adjusted, units, everything(), -ss_type) %>%
      select_if(~any(!is.na(.)))

    # Baseline users
    baseline_users <- baseline_users(ss_data, s, recode_scaleup_table, long_term_rates)

    uncertainty_adjust <- adjust_users_uncertainty(recode_sectors_reporting,
                                                   setup_data,
                                                   reporting_rates,
                                                   baseline_users,
                                                   ss_data,
                                                   long_term_rates,
                                                   user_input_adjustment_table)


    adjust_users_priv_sector <- uncertainty_adjust$users_incl_private

    fixed_adjust_users_priv_sector <- uncertainty_adjust$user_incl_private_fixed

    total_adj_users <- total_adjusted_users(adjust_users_priv_sector, fixed_adjust_users_priv_sector, include_condoms_df, method_summary)

    total_users <- total_adj_users$total_emu_df
    total_fixed_users <- total_adj_users$fixed_total_emu_df
    rr_data <- reporting_rates %>% rename(year = Year, ss_type = ss_type, name = Country)

    emu_data <- calculate_emu_from_users(pop_dataset, total_users, total_fixed_users, s, rr_data, method_summary)

    ss_type_number <- match(s, ss_data_types)

    if(Country == "DR Congo"){
      country_name <- "Democratic Republic of the Congo"
    }

    else if(Country == "Tanzania"){
      country_name <- "United Republic of Tanzania"
    }
    else{
      country_name <- Country
    }

    country_code <- country_code_data %>% filter(Country == country_name) %>% pull(division_numeric_code)

    if(length(country_code) == 0){
      country_code <- NA
    }
    country_emu_df[[ss_type_number]] <- emu_data$emu_samps %>% mutate(division_numeric_code = country_code)
    fixed_country_emu_df[[ss_type_number]] <- emu_data$fixed_emu %>% mutate(division_numeric_code = country_code)


  }

  emu_samps <- data.table::rbindlist(country_emu_df) %>% as_tibble()
  fixed_emu <- data.table::rbindlist(fixed_country_emu_df) %>% as_tibble()
  all_emu_out <- emu_samps %>% group_by(sample_id, name, ss_type) %>% mutate(delta_emu = emu - lag(emu))

  overall_emu <- all_emu_out %>%
    group_by(division_numeric_code, name, pop_type, ss_type, year) %>%
    summarise(median_emu = median(emu),
              emu_roc = median(delta_emu),
              sd_emu = sd(emu),
              sd_emu_roc = sd(delta_emu, na.rm = TRUE)) %>%
    arrange(ss_type) %>% rename(emu = median_emu) %>% filter(emu <= 1)

  return(overall_emu)
}
