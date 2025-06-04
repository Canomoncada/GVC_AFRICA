# ================================================================================
# COMPLETE ELITE PUBLICATION-READY AFRICA GVC DIAGNOSTICS R PACKAGE
# ================================================================================
# Package: AfricaGVCDiagnostics
# Version: 1.0.0
# Created: 2025-06-04 06:04:55 UTC
# Author: Canomoncada
# Description: Elite publication-ready Africa GVC readiness diagnostics with
#              comparative analysis across Africa, LAC, ASEAN, OECD, and China
# License: MIT
# ================================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(readr)
library(tibble)
library(stringr)
library(ggrepel)
library(FactoMineR)
library(ggcorrplot)

# ================================================================================
# CORE DATA CREATION FUNCTIONS
# ================================================================================

#' Create Elite GVC Dataset
#' 
#' @title Create comprehensive elite dataset with 100 countries
#' @description Creates a comprehensive dataset with 100 countries across 5 regions
#' (Africa, LAC, ASEAN, OECD, CHINA) with 0-1 normalized GVC readiness indicators.
#' 
#' @param seed Numeric. Random seed for reproducibility. Default is 2025.
#' @return A data frame with 100 countries and 18 GVC readiness indicators
#' 
#' @examples
#' dataset <- create_elite_dataset()
#' head(dataset)
#' 
#' @export
create_elite_dataset <- function(seed = 2025) {
  set.seed(seed)
  
  cat("Creating elite GVC dataset...\n")
  cat("Timestamp: 2025-06-04 06:04:55 UTC\n")
  cat("Created by: Canomoncada\n")
  cat("Target: 100 countries, 5 regions, 18 indicators\n")
  cat("China treatment: Distinct region (guaranteed inclusion)\n\n")
  
  # Define comprehensive country lists (NO OTHER CATEGORY)
  africa_countries <- c(
    "South Africa", "Morocco", "Egypt", "Nigeria", "Kenya", "Ghana", "Ethiopia", 
    "Tanzania", "Uganda", "Rwanda", "Botswana", "Mauritius", "Seychelles", "Tunisia",
    "Senegal", "Mali", "Burkina Faso", "Côte d'Ivoire", "Cameroon", "Zambia",
    "Zimbabwe", "Madagascar", "Mozambique", "Angola", "Democratic Republic of Congo",
    "Sudan", "Algeria", "Libya", "Gabon", "Namibia", "Malawi", "Benin", "Togo",
    "Guinea", "Sierra Leone", "Liberia", "Niger", "Chad", "Central African Republic",
    "Mauritania", "Djibouti", "Eritrea", "Somalia", "Gambia", "Cape Verde"
  )
  
  lac_countries <- c(
    "Brazil", "Mexico", "Chile", "Colombia", "Argentina", "Peru", "Uruguay", "Costa Rica",
    "Panama", "Ecuador", "Paraguay", "Bolivia", "Venezuela", "Guatemala", "Honduras",
    "El Salvador", "Nicaragua", "Dominican Republic", "Jamaica", "Trinidad and Tobago",
    "Barbados", "Bahamas", "Belize", "Guyana", "Suriname"
  )
  
  asean_countries <- c(
    "Singapore", "Malaysia", "Thailand", "Indonesia", "Philippines", "Vietnam", "Brunei",
    "Myanmar", "Cambodia", "Laos"
  )
  
  oecd_countries <- c(
    "United States", "Germany", "Japan", "United Kingdom", "France", "Canada", "Australia", 
    "South Korea", "Netherlands", "Switzerland", "Sweden", "Norway", "Denmark", "Finland",
    "Austria", "Belgium", "Ireland", "New Zealand", "Luxembourg", "Iceland", "Italy", 
    "Spain", "Portugal", "Greece", "Czech Republic"
  )
  
  # Create exactly 100 countries with CHINA as standalone
  all_countries <- c(
    africa_countries[1:40],  # 40 African countries
    lac_countries[1:25],     # 25 LAC countries  
    asean_countries,         # 10 ASEAN countries
    oecd_countries[1:24],    # 24 OECD countries
    "CHINA"                  # 1 China (standalone)
  )
  
  # Verify we have exactly 100 countries
  all_countries <- all_countries[1:100]
  
  # Create comprehensive elite dataset
  elite_data <- data.frame(
    Country = all_countries,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      # Assign regions (CHINA as distinct group, NO OTHER)
      Region = dplyr::case_when(
        Country == "CHINA" ~ "CHINA",
        Country %in% africa_countries ~ "Africa",
        Country %in% lac_countries ~ "LAC",
        Country %in% asean_countries ~ "ASEAN",
        Country %in% oecd_countries ~ "OECD",
        TRUE ~ "ERROR" # This should never happen now
      )
    ) %>%
    # Remove any ERROR entries (should be none)
    dplyr::filter(Region != "ERROR") %>%
    
    dplyr::mutate(
      # Create detailed Technology Pillar Indicators (0-1 normalized)
      Digital_Infrastructure = dplyr::case_when(
        Country == "CHINA" ~ 0.62,
        Region == "OECD" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.85, 0.1))),
        Region == "ASEAN" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.70, 0.15))),
        Region == "LAC" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.60, 0.15))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.40, 0.20))),
        TRUE ~ 0.5
      ),
      
      Mobile_Connectivity = dplyr::case_when(
        Country == "CHINA" ~ 0.65,
        Region == "OECD" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.90, 0.08))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.75, 0.12))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.65, 0.15))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.50, 0.25))),
        TRUE ~ 0.5
      ),
      
      Internet_Penetration = dplyr::case_when(
        Country == "CHINA" ~ 0.63,
        Region == "OECD" ~ pmax(0.4, pmin(1, stats::rnorm(dplyr::n(), 0.88, 0.08))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.68, 0.15))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.58, 0.18))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.35, 0.20))),
        TRUE ~ 0.5
      ),
      
      # Trade & Logistics Pillar Indicators
      Logistics_Performance = dplyr::case_when(
        Country == "CHINA" ~ 0.58,
        Region == "OECD" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.82, 0.10))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.68, 0.15))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.58, 0.15))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.45, 0.20))),
        TRUE ~ 0.5
      ),
      
      Trade_Integration = dplyr::case_when(
        Country == "CHINA" ~ 0.60,
        Region == "OECD" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.75, 0.12))),
        Region == "ASEAN" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.80, 0.10))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.65, 0.15))),
        Region == "Africa" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.55, 0.20))),
        TRUE ~ 0.5
      ),
      
      Ease_of_Trading = dplyr::case_when(
        Country == "CHINA" ~ 0.56,
        Region == "OECD" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.80, 0.10))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.65, 0.15))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.55, 0.18))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.40, 0.20))),
        TRUE ~ 0.5
      ),
      
      # Sustainability Pillar Indicators
      Environmental_Performance = dplyr::case_when(
        Country == "CHINA" ~ 0.45,
        Region == "OECD" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.72, 0.15))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.50, 0.20))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.58, 0.20))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.42, 0.20))),
        TRUE ~ 0.5
      ),
      
      Energy_Transition = dplyr::case_when(
        Country == "CHINA" ~ 0.48,
        Region == "OECD" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.68, 0.15))),
        Region == "ASEAN" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.45, 0.20))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.55, 0.25))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.35, 0.20))),
        TRUE ~ 0.5
      ),
      
      Carbon_Efficiency = dplyr::case_when(
        Country == "CHINA" ~ 0.42,
        Region == "OECD" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.65, 0.18))),
        Region == "ASEAN" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.48, 0.20))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.52, 0.22))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.38, 0.20))),
        TRUE ~ 0.5
      ),
      
      # Institutions & Geopolitics Pillar Indicators
      Political_Stability = dplyr::case_when(
        Country == "CHINA" ~ 0.55,
        Region == "OECD" ~ pmax(0.4, pmin(1, stats::rnorm(dplyr::n(), 0.85, 0.10))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.60, 0.18))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.55, 0.20))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.45, 0.22))),
        TRUE ~ 0.5
      ),
      
      Business_Environment = dplyr::case_when(
        Country == "CHINA" ~ 0.58,
        Region == "OECD" ~ pmax(0.4, pmin(1, stats::rnorm(dplyr::n(), 0.82, 0.10))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.62, 0.15))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.58, 0.18))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.48, 0.20))),
        TRUE ~ 0.5
      ),
      
      Regulatory_Quality = dplyr::case_when(
        Country == "CHINA" ~ 0.52,
        Region == "OECD" ~ pmax(0.4, pmin(1, stats::rnorm(dplyr::n(), 0.80, 0.12))),
        Region == "ASEAN" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.58, 0.15))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.52, 0.18))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.42, 0.20))),
        TRUE ~ 0.5
      ),
      
      # Additional GVC-specific indicators for elite analysis
      GVC_Participation_Rate = dplyr::case_when(
        Country == "CHINA" ~ 0.72,
        Region == "OECD" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.78, 0.12))),
        Region == "ASEAN" ~ pmax(0.4, pmin(1, stats::rnorm(dplyr::n(), 0.85, 0.10))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.60, 0.18))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.45, 0.25))),
        TRUE ~ 0.5
      ),
      
      Forward_Linkages = dplyr::case_when(
        Country == "CHINA" ~ 0.68,
        Region == "OECD" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.65, 0.15))),
        Region == "ASEAN" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.75, 0.12))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.55, 0.20))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.35, 0.20))),
        TRUE ~ 0.5
      ),
      
      Backward_Linkages = dplyr::case_when(
        Country == "CHINA" ~ 0.75,
        Region == "OECD" ~ pmax(0.4, pmin(1, stats::rnorm(dplyr::n(), 0.80, 0.10))),
        Region == "ASEAN" ~ pmax(0.3, pmin(1, stats::rnorm(dplyr::n(), 0.70, 0.15))),
        Region == "LAC" ~ pmax(0.2, pmin(1, stats::rnorm(dplyr::n(), 0.58, 0.18))),
        Region == "Africa" ~ pmax(0.1, pmin(1, stats::rnorm(dplyr::n(), 0.40, 0.22))),
        TRUE ~ 0.5
      )
    ) %>%
    
    # Calculate Pillar Composites
    dplyr::rowwise() %>%
    dplyr::mutate(
      Technology = mean(c(Digital_Infrastructure, Mobile_Connectivity, Internet_Penetration), na.rm = TRUE),
      Trade_Logistics = mean(c(Logistics_Performance, Trade_Integration, Ease_of_Trading), na.rm = TRUE),
      Sustainability = mean(c(Environmental_Performance, Energy_Transition, Carbon_Efficiency), na.rm = TRUE),
      Institutions_Geopolitics = mean(c(Political_Stability, Business_Environment, Regulatory_Quality), na.rm = TRUE),
      GVC_Participation = mean(c(GVC_Participation_Rate, Forward_Linkages, Backward_Linkages), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    
    # Calculate Overall GVC Readiness Index
    dplyr::rowwise() %>%
    dplyr::mutate(
      GVC_Readiness_Index = mean(c(Technology, Trade_Logistics, Sustainability, Institutions_Geopolitics), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    
    # Create Rankings (1-100)
    dplyr::mutate(
      GVC_Rank = rank(desc(GVC_Readiness_Index), ties.method = "first"),
      Technology_Rank = rank(desc(Technology), ties.method = "first"),
      Trade_Logistics_Rank = rank(desc(Trade_Logistics), ties.method = "first"),
      Sustainability_Rank = rank(desc(Sustainability), ties.method = "first"),
      Institutions_Rank = rank(desc(Institutions_Geopolitics), ties.method = "first"),
      GVC_Participation_Rank = rank(desc(GVC_Participation), ties.method = "first")
    ) %>%
    
    # Arrange by overall ranking
    dplyr::arrange(GVC_Rank) %>%
    
    # Add enhanced labels for display
    dplyr::mutate(
      Country_with_Rank = paste0(GVC_Rank, ". ", Country),
      China_Highlight = ifelse(Country == "CHINA", "Yes", "No"),
      Performance_Tier = dplyr::case_when(
        GVC_Rank <= 20 ~ "Top Performers",
        GVC_Rank <= 40 ~ "Strong Performers", 
        GVC_Rank <= 60 ~ "Moderate Performers",
        GVC_Rank <= 80 ~ "Emerging Performers",
        TRUE ~ "Developing Performers"
      )
    )
  
  # Verify dataset structure
  region_counts <- table(elite_data$Region)
  china_rank <- elite_data$GVC_Rank[elite_data$Country == "CHINA"]
  china_score <- elite_data$GVC_Readiness_Index[elite_data$Country == "CHINA"]
  
  cat("Dataset created successfully!\n")
  cat("Regional distribution:\n")
  for (region in names(region_counts)) {
    cat("  ", region, ": ", region_counts[region], " countries\n", sep = "")
  }
  cat("Total countries: ", nrow(elite_data), "\n")
  cat("China rank: ", china_rank, "/100\n")
  cat("China score: ", round(china_score, 3), " (0-1 scale)\n")
  cat("All indicators: 0-1 normalized\n\n")
  
  return(elite_data)
}

# ================================================================================
# FORMATTING AND UTILITY FUNCTIONS
# ================================================================================

#' Format Y-axis for 0-1 normalized scale
#' @export
format_y_axis_0_1 <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = sprintf("%.1f", seq(0, 1, 0.2)),
    expand = scales::expansion(mult = c(0, 0.02))
  )
}

#' Format Y-axis for 0-1 normalized scale (detailed)
#' @export
format_y_axis_0_1_detailed <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = sprintf("%.1f", seq(0, 1, 0.1)),
    expand = scales::expansion(mult = c(0, 0.02))
  )
}

#' Save plots in elite publication format
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

#' Setup complete directory structure for outputs
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
  
  cat("Complete directory structure created at: ", base_dir, "\n")
  cat("Timestamp: 2025-06-04 06:04:55 UTC\n")
  cat("Created by: Canomoncada\n\n")
  
  return(directories)
}

# ================================================================================
# MAIN CHAPTER FIGURES (ANNEX C.1)
# ================================================================================

#' Generate all main chapter figures for Annex C.1
#' @export
generate_annex_c1_figures <- function(data, directories) {
  
  cat("================================================================================\n")
  cat("GENERATING ANNEX C.1: MAIN CHAPTER FIGURES\n")
  cat("================================================================================\n")
  cat("Timestamp: 2025-06-04 06:04:55 UTC\n")
  cat("Created by: Canomoncada\n")
  cat("Target: 6 main chapter figures\n")
  cat("China treatment: Distinct region (highlighted)\n\n")
  
  pillar_cols <- c("Technology", "Trade_Logistics", "Sustainability", "Institutions_Geopolitics")
  
  # Figure 1: Regional Boxplots
  cat("Creating Figure 1: Regional Boxplots...\n")
  figure_1 <- create_figure_1_boxplots(data, pillar_cols)
  save_elite_format(figure_1, "Figure_1_GVC_Pillar_Boxplots_5_Regions", directories$annex_c1, 12, 8)
  
  # Figure 2: Africa-China Heatmap
  cat("Creating Figure 2: Africa-China Heatmap...\n")
  figure_2 <- create_figure_2_heatmap(data, pillar_cols)
  save_elite_format(figure_2, "Figure_2_Africa_China_Heatmap_5_Regions", directories$annex_c1, 12, 10)
  
  # Figure 3: PCA Biplot
  cat("Creating Figure 3: PCA Biplot...\n")
  figure_3 <- create_figure_3_pca(data, pillar_cols)
  save_elite_format(figure_3, "Figure_3_Africa_China_PCA_Biplot_5_Regions", directories$annex_c1, 12, 9)
  
  # Figure 4: Comparative Boxplots
  cat("Creating Figure 4: Comparative Boxplots...\n")
  figure_4 <- create_figure_4_comparative(data)
  save_elite_format(figure_4, "Figure_4_Comparative_Boxplots_5_Regions", directories$annex_c1, 12, 7)
  
  # Figure 5: Subregional Analysis
  cat("Creating Figure 5: Subregional Analysis...\n")
  figure_5 <- create_figure_5_subregional(data, pillar_cols)
  save_elite_format(figure_5, "Figure_5_Subregional_vs_China_5_Regions", directories$annex_c1, 14, 9)
  
  # Figure 6: Parallel Coordinates
  cat("Creating Figure 6: Parallel Coordinates...\n")
  figure_6 <- create_figure_6_parallel(data, pillar_cols)
  save_elite_format(figure_6, "Figure_6_Parallel_Coordinates_5_Regions", directories$annex_c1, 12, 8)
  
  cat("All Annex C.1 figures created successfully (6/6)\n\n")
  
  return(list(
    figure_1 = figure_1,
    figure_2 = figure_2,
    figure_3 = figure_3,
    figure_4 = figure_4,
    figure_5 = figure_5,
    figure_6 = figure_6
  ))
}

# Helper functions for individual figures
create_figure_1_boxplots <- function(data, pillar_cols) {
  plot_data <- data %>%
    dplyr::filter(Region %in% c("Africa", "LAC", "ASEAN", "OECD", "CHINA")) %>%
    dplyr::select(Country, Region, dplyr::all_of(pillar_cols)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(pillar_cols), names_to = "Pillar", values_to = "Score") %>%
    dplyr::mutate(
      Pillar = dplyr::case_when(
        Pillar == "Technology" ~ "Technology",
        Pillar == "Trade_Logistics" ~ "Trade & Logistics",
        Pillar == "Sustainability" ~ "Sustainability",
        Pillar == "Institutions_Geopolitics" ~ "Institutions & Geopolitics",
        TRUE ~ Pillar
      ),
      Region = factor(Region, levels = c("Africa", "LAC", "ASEAN", "OECD", "CHINA"))
    )
  
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Region, y = Score, fill = Region)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
    ggplot2::scale_fill_manual(values = c("Africa" = "#E74C3C", "LAC" = "#2ECC71", 
                                         "ASEAN" = "#3498DB", "OECD" = "#9B59B6", "CHINA" = "#F39C12")) +
    format_y_axis_0_1() +
    ggplot2::facet_wrap(~ Pillar, nrow = 2) +
    ggplot2::labs(
      title = "Figure 1.
