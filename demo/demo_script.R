# script to check functions for calculating country-level emus using the excel tool
devtools::load_all()
new_tool_filepath <- "~/Documents/GitHub/emu_output_analysis/data/SS to EMU Tool - Simple.xlsx"

shiny_input_type <- "clients" # this would be an object in the shiny environment - choice from dropdown menu

test_new_tools <- get_shiny_tools_info(new_tool_filepath, shiny_input_type)

test_emu_dataset <- country_ss_to_emu(test_new_tools, shiny_input_type)
