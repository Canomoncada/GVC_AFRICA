# Africa-China GVC Readiness Analysis Replication

This repository contains the code and documentation to fully replicate the Africa-China GVC Readiness project.

---

## Project Overview

Analyze and visualize readiness metrics for African economies in global value chains (GVCs), with a focus on China integration.

- **Author:** Anthony S. Cano Moncada
- **Contact:** ac4479a@american.edu
- **Last Updated:** June 2025

---

## Directory Structure

```
GVC_AFRICA/
├── Data/                  # Raw input data files (NOT included in repo)
├── outputs/               # Generated analysis results
│   ├── figures/           # Plots/figures (not tracked)
│   └── clean/             # Cleaned/processed data (not tracked)
├── logs/                  # Optional logs (not tracked)
├── scripts/               # Optional modular scripts
├── africa_china_gvc_master_replication.R
├── replication_driver.R
├── README.md
├── LICENSE
└── .gitignore
```

---

## Replication Instructions

### 1. Prepare Input Data

Place these files in the `Data/` directory:

| File                                      | Description/Source                            |
|--------------------------------------------|-----------------------------------------------|
| Business-Ready.xlsx                        | World Bank Doing Business indicators         |
| Co2toGDP_Data.csv                          | World Bank CO2 emissions data                |
| countries of the world.csv                 | General country statistics                    |
| GSMA_Data_2024.csv                         | GSMA Mobile Connectivity Index 2024          |
| Individuals-using-the-internet.csv         | ITU internet usage statistics                |
| International_LPI_from_2007_to_2023.xlsx   | World Bank Logistics Performance Index       |
| Political Stability.dta                    | World Bank Governance Indicators             |
| Share of modern renewables database.xlsx   | IRENA renewable energy statistics            |
| Trade (_ of GDP).csv                       | World Bank trade as % of GDP                 |

**Important:**  
Raw data is **not included** in this repository due to size and licensing. Obtain data from the sources above and place in the `Data/` directory.

---

### 2. Run Replication

1. **Open R or RStudio**
2. **Set working directory** to the project root (`GVC_AFRICA/`)
3. **Install required packages** (if needed):
   ```r
   install.packages(c("readr", "readxl", "haven", "ggplot2", "dplyr", 
                      "tidyr", "stringr", "corrplot", "factoextra"))
   ```
4. **Run the master replication script**:
   ```r
   source("replication_driver.R")
   ```

The script will sequentially execute all workflow parts (0–10):
- **Part 0:** Setup and initialization
- **Part 1:** Directory creation
- **Part 2:** Package loading
- **Part 3:** Utility functions
- **Part 4:** Region mapping
- **Part 5:** Data loading
- **Part 6:** Data cleaning and standardization
- **Part 7:** Analysis and clustering
- **Part 8:** Visualization
- **Part 9:** Validation and diagnostics
- **Part 10:** Final outputs and documentation

### Output Directory

All figures, tables, and cleaned data are written to a single export root
directory. By default this path is:

```
/Volumes/VALEN/Africa:LAC/Insert/READY TO PUBLISH
```

You can modify the `export_root` variable at the top of each script if you wish
to save the outputs elsewhere.

---

### 3. Expected Outputs

After successful replication:

- **Cleaned Data:**
  Saved in `outputs/clean/` (e.g., `Business_Ready_clean.csv`, `master_dataset.csv`)

- **Figures/Plots:**
  Saved in `outputs/figures/` (e.g., `pca_analysis.png`, `africa_readiness_map.png`)

- **Analysis Results:**  
  Summary statistics, cluster assignments, and policy recommendations

---

### 4. System Requirements

- **R:** Version 4.0.0 or higher
- **Operating System:** Windows, macOS, or Linux
- **Memory:** At least 4GB RAM recommended
- **Storage:** ~500MB for outputs

**Required R Packages:**
```r
c("readr", "readxl", "haven", "ggplot2", "dplyr", "tidyr", 
  "stringr", "corrplot", "factoextra", "cluster", "RColorBrewer")
```

---

### 5. Troubleshooting

**Common Issues:**

1. **Missing data files:** Ensure all 9 data files are in the `Data/` directory
2. **Package errors:** Run `install.packages()` for missing packages
3. **Path issues:** Verify your working directory is set to project root
4. **Memory errors:** Close other applications if running low on RAM

**Getting Help:**
- Open a GitHub issue in this repository
- Contact maintainer at ac4479a@american.edu

---

### 6. Citation

If you use this code or analysis, please cite:

```
Cano Moncada, Anthony S. (2025). Africa-China GVC Readiness Analysis. 
GitHub repository: https://github.com/Canomoncada/GVC_AFRICA
```

---

### 7. License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

### 8. Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Commit your changes (`git commit -am 'Add improvement'`)
4. Push to the branch (`git push origin feature/improvement`)
5. Create a Pull Request

---

### 9. Acknowledgments

- World Bank for data access
- GSMA for mobile connectivity data
- IRENA for renewable energy statistics
- ITU for internet usage data

---

**Last Updated:** June 3, 2025
