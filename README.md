### Quantitative Assessment of G7's Collaboration in Sustainable Development Goals

Authors: Kai Liu, Ali Raisolsadat, Van Quan Dau, Xander Wang

School of Climate Change and Adaptation, University of Prince Edward Island, Charlottetown, PE, C1A 4P3, Canada

Faculty of Mathematics, University of Waterloo, 200 University Ave W, Waterloo, ON, N2L 3G1, Canada

Corresponding author: Dr. Xander Wang

Contact Information: xxwang@upei.ca

This repository contains the code for this project:

- main_code_and_results: This folder contains all figures for the manuscript and supplementary materials.
  - SDG_Data.xs summarizing all results from this work.

- results_and_figures_used_in_manuscript: This folder contains the R script and data used in this study.
  - **indicators (raw data)**: This folder includes raw data downloaded from the <a href="https://sdg-tracker.org" target="_blank">SDG Tracker</a> database.</li> Each Sustainable Development Goals indicator data is group based on the United Nations' categories.
  - **sdg_data**: This folder includes the cleaned SDG indicators data from 1999-2020.
  - **partial_true_direction_un.csv**: The .csv file indicates the desirable directions used in the synergy analysis.
  - **gross_synergy_markdown.Rmd**: This R markdown file is the main code for this project. The implementation of the methodlogy is within this file. Researchers can use this file to replicate or create new synergy values.
  - **results**: Outputs of the study derived from the R script (gross_synergy_markdown.Rmd).
