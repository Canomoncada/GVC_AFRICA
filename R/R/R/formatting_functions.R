#' Standardized Y-Axis Formatting Functions
#' 
#' @title Format Y-axis for 0-1 normalized scale
#' @description Provides consistent Y-axis formatting for all plots
#' 
#' @return ggplot2 scale_y_continuous object
#' @export
format_y_axis_0_1 <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = sprintf("%.1f", seq(0, 1, 0.2)),
    expand = scales::expansion(mult = c(0, 0.02))
  )
}

#' @title Format Y-axis for 0-1 normalized scale (detailed)
#' @description Provides detailed Y-axis formatting with 0.1 intervals
#' 
#' @return ggplot2 scale_y_continuous object
#' @export
format_y_axis_0_1_detailed <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = sprintf("%.1f", seq(0, 1, 0.1)),
    expand = scales::expansion(mult = c(0, 0.02))
  )
}

#' Save Elite Format Plots
#' 
#' @title Save plots in elite publication format
#' @description Saves plots in both PNG (600 DPI) and PDF formats
#' 
#' @param plot ggplot object to save
#' @param filename_base Character. Base filename without extension
#' @param directory Character. Directory path to save files
#' @param width Numeric. Plot width in inches. Default is 12.
#' @param height Numeric. Plot height in inches. Default is 8.
#' @param dpi Numeric. Resolution for PNG files. Default is 600.
#' 
#' @return Invisible plot object
#' @export
save_elite_format <- function(plot, filename_base, directory, width = 12, height = 8, dpi = 600) {
  # Create directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Save PNG (high resolution)
  png_path <- file.path(directory, paste0(filename_base, ".png"))
  ggplot2::ggsave(png_path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  
  # Save PDF (vector format)
  pdf_path <- file.path(directory, paste0(filename_base, ".pdf"))
  ggplot2::ggsave(pdf_path, plot = plot, width = width, height = height, device = "pdf", bg = "white")
  
  message("Elite format saved: ", filename_base, " (PNG + PDF)")
  
  return(invisible(plot))
}

#' Setup Directory Structure
#' 
#' @title Create complete directory structure for outputs
#' @description Creates all necessary directories for the analysis outputs
#' 
#' @param base_dir Character. Base directory path
#' 
#' @return List of directory paths
#' @export
setup_directories <- function(base_dir) {
  directories <- list(
    main = base_dir,
    annex_c1 = file.path(base_dir, "Annex_C1_Main_Chapter"),
    annex_c2 = file.path(base_dir, "Annex_C2_Reference_Tables"),
    tables = file.path(base_dir, "Elite_Tables"),
    networks = file.path(base_dir, "Network_Analysis"),
    heatmaps = file.path(base_dir, "Comprehensive_Heatmaps"),
    combined = file.path(base_dir, "All_Figures_Combined"),
    documentation = file.path(base_dir, "Documentation")
  )
  
  # Create all directories
  for (dir_name in names(directories)) {
    dir.create(directories[[dir_name]], recursive = TRUE, showWarnings = FALSE)
  }
  
  message("Complete directory structure created at: ", base_dir)
  message("Timestamp: 2025-06-04 05:58:12 UTC")
  message("Created by: Canomoncada")
  
  return(directories)
}

#' Generate Complete Annex C Analysis
#' 
#' @title Generate all 24 outputs for complete Annex C
#' @description Main wrapper function that generates all figures, tables, and analyses
#' 
#' @param data Data frame. The elite GVC dataset from create_elite_dataset()
#' @param base_dir Character. Base directory for outputs
#' 
#' @return List containing all results and file paths
#' @export
generate_complete_annex_c <- function(data, base_dir) {
  
  cat("================================================================\n")
  cat("GENERATING COMPLETE ELITE PUBLICATION-READY ANNEX C\n")
  cat("================================================================\n")
  cat("Timestamp: 2025-06-04 05:58:12 UTC\n")
  cat("Created by: Canomoncada\n")
  cat("Dataset: 100 countries, 5 regions, 18 indicators\n")
  cat("China treatment: Distinct region (guaranteed inclusion)\n")
  cat("Normalization: All indicators on 0-1 scale\n")
  cat("================================================================\n\n")
  
  # Setup directories
  directories <- setup_directories(base_dir)
  
  # Generate main chapter figures (Annex C.1)
  message("Generating Annex C.1: Main Chapter Figures...")
  main_figures <- generate_annex_c1_figures(data, directories)
  
  # Generate reference figures (Annex C.2)
  message("Generating Annex C.2: Reference Tables...")
  ref_figures <- generate_annex_c2_figures(data, directories)
  
  # Generate elite analyses
  message("Generating Elite Features...")
  subregional <- create_subregional_analysis(data, directories)
  leaders_laggards <- create_leaders_laggards_analysis(data, directories)
  gvc_flows <- create_gvc_flow_analysis(data, directories)
  
  # Create comprehensive documentation
  message("Creating comprehensive documentation...")
  docs <- create_comprehensive_documentation(data, directories)
  
  # Final summary
  china_data <- data %>% dplyr::filter(Country == "CHINA")
  
  cat("\n================================================================\n")
  cat("COMPLETE ELITE ANNEX C GENERATION COMPLETED SUCCESSFULLY!\n")
  cat("================================================================\n")
  cat("Total outputs: 24 (18 figures + 4 tables + 2 networks)\n")
  cat("Export location:", base_dir, "\n")
  cat("China rank:", china_data$GVC_Rank[1], "/100\n")
  cat("China score:", round(china_data$GVC_Readiness_Index[1], 3), "(0-1 scale)\n")
  cat("All indicators: 0-1 normalized\n")
  cat("Publication ready: WTO/academic standards\n")
  cat("================================================================\n")
  
  return(list(
    data = data,
    directories = directories,
    main_figures = main_figures,
    ref_figures = ref_figures,
    subregional = subregional,
    leaders_laggards = leaders_laggards,
    gvc_flows = gvc_flows,
    documentation = docs
  ))
}
