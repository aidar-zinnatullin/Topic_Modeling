# Guardian Corpus Analysis

This repository contains scripts for analyzing the Guardian news corpus using various topic modeling techniques. The primary goal of this project is to preprocess the text data, perform topic modeling (STM and KeyATM), and visualize the results.

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
- `keyATM`
- `rlang`

Ensure that you have these packages installed. If any package is missing, you can install it using the `install.packages()` function or `devtools::install_github()` for packages hosted on GitHub (for instance, `quanteda.corpora`).

## Files

- `STM.R`: The main R script that performs the analysis.
- `Validation_oolong.R`: The validation scripts based on the Oolong R package for the test of topic and word intrusion (Chan \& Sältzer, 2020, <https://www.theoj.org/joss-papers/joss.02461/10.21105.joss.02461.pdf>).
- `Preprocessing.R`: Experiments with preprocessing based on the preText R package (Denny \& Spirling, 2018, <https://www.cambridge.org/core/journals/political-analysis/article/abs/text-preprocessing-for-unsupervised-learning-why-it-matters-when-it-misleads-and-what-to-do-about-it/AA7D4DE0AA6AB208502515AE3EC6989E>, and the link to the online tutorial  <http://www.mjdenny.com/getting_started_with_preText.html>.
- `KATM.R`: The R script for the dynamic keyword-assisted topic modeling (Eshima, Imai, \& Sasaki, 2024 <https://onlinelibrary.wiley.com/doi/10.1111/ajps.12779>, and the link to the online tutorial <https://keyatm.github.io/keyATM/index.html>)
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
- `dynamicTopicModel.qs`: Keyword-assisted topic model.

