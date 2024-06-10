# Guardian Corpus Analysis

This repository contains scripts and data for analyzing the Guardian news corpus using various text mining and topic modeling techniques. The primary goal of this project is to preprocess the text data, perform topic modeling, and visualize the results.

## Requirements

The following R packages are required for running the script:

- `here`
- `devtools`
- `quanteda`
- `quanteda.corpora`
- `stringr`
- `tidyverse`
- `tidytext`
- `stm`
- `geometry`
- `Rtsne`
- `rsvd`
- `SnowballC`
- `plotrix`

Ensure that you have these packages installed. If any package is missing, you can install it using the `install.packages()` function or `devtools::install_github()` for packages hosted on GitHub (for instance, `quanteda.corpora`).

## Files

- `STM.R`: The main R script that performs the analysis.
- `stopwords.txt`: A text file containing custom stopwords (*n*=736).
- `stm_outputs/saved_objects.RData`: Saved RData objects from the STM preprocessing step.
- `stm_outputs/PreliminaryModel.RData`: Preliminary STM model.
- `stm_outputs/prep_mimno_tsur.RData`: Preprocessed data for effect estimation.
- `stm_outputs/semantic_coherence_1.csv`: Semantic coherence results for the initial range of *K* (from 10 to 100).
- `stm_outputs/semantic_coherence_2.csv`: Semantic coherence results for the second range of *K* (from 15 to 35).
- `stm_outputs/semantic_coherence_3.csv`: Semantic coherence results for the final range of *K* (from 20 to 30).
- `stm_outputs/modelPrevFit_27.RData`: Final STM model with 27 topics.
- `stm_outputs/STM_27.RData`: Effect estimation model for 27 topics.
- `Figures/Figure1.jpeg`: Plot of semantic coherence vs. exclusivity for the initial range of *K*.
- `Figures/Figure2.jpeg`: Plot of semantic coherence vs. exclusivity for the second range of *K*.
- `Figures/Figure3.jpeg`: Plot of semantic coherence vs. exclusivity for the final range of *K*.
- `Figures/Vis_27.jpeg`: Summary plot of the final STM model with 27 topics.
- `Figures/VisTopics.jpeg`: Barplot of average topic proportions (*K*=27).

