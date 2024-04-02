# script to check functions for calculating country-level emus using the excel tool

new_tool_filepath <- "~/Documents/GitHub/emu_output_analysis/data/SS to EMU Tool - Simple.xlsx"

test_new_tools <- get_tools_info(new_tool_filepath)

test_emu_dataset <- country_ss_to_emu(test_new_tools)
