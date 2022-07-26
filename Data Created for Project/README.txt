
*** Data accompanying the research on ...

Authors: 
School of Climate Change and Adaptation, 
University of Prince Edward Island, 
Charlottetown, PE, C1A 4P3,
Canada

Corresponding author:
Contact Information: 

 
***General Introduction*** 
The "Datasets" folder containts the data created for this project. Each folder is in .csv format for ease of readability through statistical software. Please note that SDG stands for Sustainable Development Goals. 


***Description of the data in this data set***  

1. impact_average_economic_indicators.csv
This dataset is the average of the domestic change (DC) across every year for each of the economic SDGs listed in table 1. The domestic averages are for ever country in the group seven (G7). 

2. impact_average_environment_indicators.csv
This dataset is the average of the domestic change (DC) across every year for each of the environmental SDGs listed in table 1. The domestic averages are for ever country in the group seven (G7). 

3. economic_gross_synergy_g7.csv
This dataset contains time series Gross Synergy values (equation 10) for the economic SDGs of the G7 countries. 

4. environment_gross_synergy_g7.csv
This dataset contains time series Gross Synergy values (equation 10) for the environmental SDGs of the G7 countries. 

5. country[j]_contribution_of_synergy.csv
The country[j] represents country jth country of the G7 countries, namely Canda, France, Germany, Italy, Japan, the United Kingdom, and the United States of America. Therefore, there are 7 datasets with this name. These datasets containts time series contribution values of each country to the overall gross synergy synergy. These datasets were used to generate the heatmaps, by simply using each SDG indicator as the key and countries as values. The datasets were normalized between -100% to 100% for the heatmaps. 

6. economic_contribuitons_per_indicator_g7.csv 
7. environment_contribuitons_per_indicator_g7.csv 
These two datasets contain positive proportion of contributions to the gross synergy of economic and environmental SDG indicators by G7 countries. Each column represents the G7 country, and each observation represents the positive proportion of a single country to an SDG indicator. These proportions were found by counting the number of positive contributions for all years for a single SDG indicator. 
The contributions datasets (point 6 and 7 above) and the formula below was used to get this dataset. 

Positive Proportion of Contribution = number of positive contributions throughout the year / total number of contributions throughout the years

8. yearly_economic_contributions_by_g7.csv
9. yearly_environment_contributions_by_g7.csv
These two datasets contain yearly positive proportion of contributions to the gross synergy of economic and environmental SDG indicators by G7 countries. These proportions were found by counting the number of contributions for every single year for all economic and environmental indicators.
The contributions datasets (point 8 and 9 above) and the formula below was used to get this dataset. 

Yearly Positive Proportion of Contribution = number of positive contributions of one year throughout all SDG indicators / total number of SDG indicators



