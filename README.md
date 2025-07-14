# Statistics in Medicine – Final Project

This repository contains materials from a statistical analysis project on COVID-19 data, completed in collaboration with a colleague. The project was conducted as part of the *Statistics in Medicine* course in the fifth year of the Data Analysis program at Nicolaus Copernicus University in Toruń, Poland.

The project involved statistical analysis of anonymized clinical trial data to assess treatment efficacy. Due to confidentiality agreements, the dataset cannot be shared. However, the repository includes:

- **R script:** showing the analysis workflow.
- **Project report (PDF):** prepared in Polish.
- **English summary:** provided below for context.

---
## 📄 English Summary

The trial was focused on measuring antibodies level (BAU/ml) in men. The levels of antibodies were measured three times. Unfortunately only a bit more than half of participants (53.05%) actually had three measurements taken.
We applied descriptive statistics methods - checking missing values, calculating mean and median, creating box plots and histograms and checking for outliers using different criteria (Tukey, Chauvenet) to see what we can work with. After that, we've had free rein when it came to hypothesis, so we've made these:
- Age and COVID-19 morbidity are independent (Fisher's exact test)
- Age and COVID-19 symptoms are independent (Fisher's exact test)
- Blood type and COVID-19 morbidity are independent (Fisher's exact test)
- RH factor and COVID-19 morbidity are independent (Proportion's test)
- Antibody levels are identically distributed for men who showed symptoms vs. those who did not (three hypotheses corresponding to the three measurements, tested with the Mann-Whitney U test).
- COVID-19 morbidity and co-occuring diseases morbidity are independent (co-occurring diseases were categorized into respiratory, cardiovascular, and other diseases; tested with Fisher's exact test).
  
Due to the nature of the dataset, we primarily relied on nonparametric methods. All tests returned p-values greater than 0.05, except for the tests related to the second and third antibody measurements, where significant differences were observed.

---
**Note:** The data used in this project is confidential and cannot be included in this repository.
