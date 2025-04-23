# 786Annualised-plot
# PlotCraft: Interactive Visualization of Grouped Data

[![Lifecycle: Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
**PlotCraft** is an R package containing a Shiny web application designed for creating highly customizable plots comparing grouped data. It simplifies the process of visualizing both summary statistics (like means ± SE) and the underlying distribution of individual data points, facilitating clearer communication and exploration of research findings.

This tool is particularly useful for researchers, students, and analysts who need to generate publication-quality figures comparing two groups across multiple outcomes without extensive coding.

**Try PlotCraft Online:** [Link to Hosted App if deployed, e.g., on shinyapps.io]

**Associated Publication:** This software is described in detail in our SoftwareX paper:
> [Your Name(s), et al.]. (Year). PlotCraft: An R Shiny Application for Customizable Visualization of Grouped Summary and Point Data. *SoftwareX*, Volume(Issue), [ArticleID]. [DOI when available]

---

## Features

* **Multiple Plot Types:** Generate Bar+Scatter, Boxplot+Scatter, Violin+Scatter, Scatter Only, Histograms, Density Plots, and ECDF plots.
* **Combined Visualization:** Easily overlay individual data points onto summary representations (bars, boxplots, violins).
* **Interactive Customization:** Modify plot titles, labels, colors, themes, points, error bars, fonts, and more via an intuitive graphical user interface (GUI).
* **Two-Group Comparison:** Specifically designed for comparing two groups (e.g., Treatment vs. Control) across various outcomes.
* **Easy Data Input:** Upload data via a simple, structured CSV format. Sample data provided.
* **Export Outputs:** Download generated plots as PNG files and aggregated summary statistics as a CSV file.
* **No Coding Required:** Built for users with varying levels of programming expertise.

---

## Installation

You can install the development version of PlotCraft from GitHub using the `devtools` package. If you don't have `devtools` installed, first run: `install.packages("devtools")`.

```R
# Install the package from GitHub
devtools::install_github("[YourUsername]/PlotCraft") # Replace with your GitHub username/repo

# Load the package
library(PlotCraft)
