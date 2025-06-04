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
#' \dontrun{
#' dataset <- create_elite_dataset()
#' head(dataset)
#' }
#' 
#' @export
create_elite_dataset <- function(seed = 2025) {
  set.seed(seed)
  
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
  
  return(elite_data)
}
