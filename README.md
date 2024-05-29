# Quantitative Assessment of G7's Collaboration in Sustainable Development Goals

**Authors**: Kai Liu ([kailiu@upei.ca](mailto:kailiu@upei.ca))[1], Ali Raisolsadat ([arraisolsadat@uwaterloo.ca](mailto:arraisolsadat@uwaterloo.ca)) [2], Xander Wang ([xxwang@upei.ca](mailto:xxwang@upei.ca))[3,4], and Van Quan Dau ([vdau@upei.ca](mailto:vdau@upei.ca)) [3,4]

**Institutions**:  
1. School of Mathematical and Computational Sciences, University of Prince Edward Island, Charlottetown, Prince Edward Island, Canada C1A 4P3
2. Faculty of Mathematics, University of Waterloo, Waterloo, Ontario, Canada N2L 3G1
3. Canadian Centre for Climate Change and Adaptation, University of Prince Edward Island, St. Peter's Bay, Prince Edward Island, Canada C0A 2A0
4. School of Climate Change and Adaptation, University of Prince Edward Island, Charlottetown, Prince Edward Island, Canada C1A 4P3

**Corresponding Author**: Dr. Xander Wang  
**Contact Information**: [xxwang@upei.ca](mailto:xxwang@upei.ca)

---

### Repository Contents

This repository contains the code and data for the project titled "Quantitative Assessment of G7's Collaboration in Sustainable Development Goals".

### Information about the Folders

- **`sdg_raw_data`**: Contains the raw Sustainable Development Goals indicator data from the "Our World in Data" database.
- **`sdg_grouped_raw_data`**: Contains the raw SDG indicator data, but grouped for each goal (1-15).
- **`results_datasets`**: Contains the main results for Domestic Changes, Foreign Changes, and Synergy data in `.CSV` and `.RData` formats.
- **`main_manuscript_figures`**: Contains the 5 main figures used in the manuscript text.
- **`s1_s12_supplementary_figures`**: Contains the 12 figures from the supplementary material of the manuscript.
- **`partial_true_direction_un.csv`**: Contains the indicator directions from Table 1 of the manuscript.
- **`SDG_Data.xlsx`**: An Excel file which contains all the data used in the manuscript results in multiple sheets, including SDG raw data and results datasets.

### Prerequisites 

- **R**: Please ensure that you have installed the latest version of the R software for your device. You can download it from [CRAN](https://cran.r-project.org/).
- **RStudio**: It is recommended to use RStudio for running the R scripts. You can download it from [RStudio's official website](https://rstudio.com/products/rstudio/download/).

### How to Run
1. **Before running the main code, set the directory to the same folder as the project.**
2. **Run Main Code**:
    - Open and run the `gross_synergy_markdown.Rmd` file. This is the main code for our manuscript.
    - The resulting datasets will be saved in the `results_datasets` folder.

3. **Generate Figure 1**:
    - Open and run `figure_1.R`.
    - The resulting figure will be saved in the `main_manuscript_figures` folder.

4. **Generate Figure 2**:
    - Open and run `figure_2.R`.
    - The resulting figure will be saved in the `main_manuscript_figures` and `s1_s12_supplementary_figures` folders, respectively.

5. **Generate Figure 3**:
    - Open and run `figure_3.R`.
    - The resulting figure will be saved in the `main_manuscript_figures` folder.

6. **Generate Figure 4**:
    - Open and run `figure_4.R`.
    - The resulting figure will be saved in the `main_manuscript_figures` and `s1_s12_supplementary_figures` folders, respectively.

7. **Generate Figure 5**:
    - Open and run `figure_5.R`.
    - The resulting figure will be saved in the `main_manuscript_figures` folder.
