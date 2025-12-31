# Population Density Visualization

A project for visualizing population density using R, `rayshader`, and spatial data.

## ⚠️ Note on Data Files
The `/Data` folder containing large geographic dataset files (`.gpkg`) is **excluded** from this repository via `.gitignore` due to GitHub's file size limits.

## 📁 Key Data Sources
The analysis and visualization in this project rely on the following key datasets:
*   The primary global population dataset (`kontur_population_20231101.gpkg`) can be downloaded from the **Humanitarian Data Exchange (HDX)**: [Kontur Population Dataset 2023](https://data.humdata.org/dataset/kontur-population-dataset).
*   Other processed boundary files are stored locally in the `/Data` folder.

## 🚀 Usage Instructions
To replicate the visualization, follow these steps in order:

1.  **Data Preparation**: First, run the R script with the `_cut` suffix (e.g., `cut_data.R`). This script clips the global population dataset to your specific area of interest (e.g., a city or region) and saves the subset locally in the `/Data` folder.
2.  **Visualization**: Then, run the R script with the `_draw` suffix (e.g., `draw_plot.R`). This script reads the clipped data from `/Data` and generates the 3D population density map using `rayshader`.

> **Note**: Ensure the required data files are in the `/Data` folder before running the scripts.

## 🙏 Attribution
This project was inspired by and draws methodological reference from the excellent work of **Niloy Biswas**: [Population-Density-Map](https://github.com/niloy-biswas/Population-Density-Map). Major credits and thanks go to the original author.

## 🛠️ Tools
*   **R** with `sf`, `rayshader`, `tidyverse`, etc.
*   **Git** for version control.