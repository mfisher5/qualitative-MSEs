# Translating quantitative MSEs into qualitative network models

How do qualitative network models compare to a single-species MSE?

### Abstract

<<<.....>>>
We see a need for a more systematic and inclusive approach to “triaging” management strategies for an MSE, to identify which are likely to be viable candidates for selection, without developing complex population models and associated software.



### Directory structure

- `R` scripts: are numbered in the order they should be run. the `fxns` folder contains custom functions for R.
- `doc`: contains paper drafts, and figures and tables for the supplement
- `data`: currently contains the QNMs (as dia objects and matrices in CSV files), QPress perturbation output, sensitivity analysis output, and summary data for results

Where can I find....
- the CSV files with matrices for every QNM: `data/qnm_models`, CSV files are labeled by operating model scenario (QNM_XX) and management strategy (_hcrY)
- the output from the full quantitative MSEs, for the four performance metrics: `data/results/raw/mse_Table4_Species12_20240206.csv`
- the code for running boosted regression trees, as sensitivity analysis: `R/04_qnm_sensitivity.Rmd` -- calls `impact.ss.R`, and uses an input data frame that is saved into data/qnm_out/sensitivity/gbmIn....
