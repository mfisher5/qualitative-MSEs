# Can triage with qualitative models make MSE more efficient?

Mary C Fisher, Chris J Harvey, Tessa B Francis, Kristin N Marshall, Laura K Nelson, Corey Ridings, André E Punt

[![DOI](https://zenodo.org/badge/835397072.svg)](https://doi.org/10.5281/zenodo.24144070)

### Abstract

Management strategy evaluation (MSE) is a crucial tool for exploring alternative management strategies. However, expanding an MSE to account for complexities like spatial structure and climate change can further extend a time-consuming process. This is at odds with the need for more responsive fisheries management in the face of climate change. We explored the potential for qualitative network models (QNMs) to shorten the timeline for MSE by “triaging” management strategies. Ideally, triage with QNMs would pre-emptively eliminate strategies that are unlikely to satisfy management objectives, allowing the full MSE to be run for a smaller number of strategies or operating model scenarios. To test QNM performance as a triage tool, we translated a single-species MSE with five operating model scenarios and five management strategies into QNMs. Three of the five operating model scenarios included climate change impacts to the fished species. We then compared directional change in four performance metrics, as predicted by the QNMs versus the quantitative MSE. We considered two aspects of QNM performance as a triage tool: (1) the time to develop and run the QNMs, and (2) the degree to which the QNM predictions matched those of the quantitative MSE. Agreement between the two model types varied widely by performance metric, matching >70% of the time for spawning biomass and <45% of the time for total fishery catch. The QNMs performed well in identifying lose-lose catch-biomass outcomes from status quo or ‘hands-off’ management strategies under scenarios with negative impacts to biomass. However, the QNMs were overly optimistic in predicting increases for fishery catch when the quantitative MSE predicted a decline. As a result, triage would have retained a number of potentially non-viable management strategies, limiting the time saved. We therefore find that, despite their faster runtimes (< 5 seconds per QNM), QNMs are not a generally applicable tool for triage for MSE, and will likely be useful only under limited circumstances. We see greater value in QNMs as a complementary, participatory exercise to support the actual and perceived legitimacy of MSE, and to supplement quantitative model predictions.



### Directory structure

- `R` scripts: are numbered in the order they should be run. the `fxns` folder contains custom functions for R.
- `doc`: contains paper drafts, and figures and tables for the supplement
- `data`: currently contains the QNMs (as dia objects and matrices in CSV files), QPress perturbation output, sensitivity analysis output, and summary data for results

Where can I find....

- the DIA objects and/or matrices for every QNM: `data/qnm_models`
- the output from the quantitative MSE: `data/mse_out`
- the code to re-produce the figures from the paper: in the `R` folder, R scripts are named according to the figure produced. *These will require you to re-run scripts 3 and 3b, to produce the intermediate files needed for graphing.*


### Acknowledgements
Funding for this project was provided by the David & Lucile Packard Foundation through the [Ocean Modeling Forum](https://oceanmodelingforum.org/).
