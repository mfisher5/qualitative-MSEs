### R Scripts 

To replicate the analysis in the paper, run these R scripts in the order they are numbered. 

01: "Run" (perturb) the qualitative network models. 

02: Re-format QNM output from script 1 to match the quantitative MSE output tables for the paper.

03: Compare predictions from the QNMs v. quantitative MSE.

(03b: Compare predictions from the QNMs v. the quantitative MSE run using the Beverton-Holt stock-recruit relationship)

04: Evaluate the influence of each link in the QNM on a given response variable, with boosted regression trees. 

(04b: Explore output of boosted regression trees.  Can we connect certain patterns in link-specific influence to mismatches between the qualitative / quantitative models? )

05: Mostly for supplemental material. Compare the % total matrices that were stable across operating model scenarios (OMs) x management strategies (HCRs).

06: Mostly for supplemental material. A re-write of script 01. In these QNMs, the *Total Catch* and *Fishery 1 Catch* performance metrics are split by fish size (small / large fish). 

07: Mostly for supplemental material. A re-write of script 03. In these QNMs, the **Total Catch** and **Fishery 1 Catch** performance metrics are split by fish size (small / large fish). 

To re-create the figures in the paper, use the scripts titled "figX." Figures will be saved to the `doc` folder.

Scripts with `bh` or `BH` in the title will compare QNM output with the qMSE run using the Beverton-Holt stock-recruit model (rather than the Ricker model).

The `fxns` folder contains custom R functions called in the numbered scripts.