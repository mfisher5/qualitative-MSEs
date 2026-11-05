## Model run results

This folder will contain results produced in scripts 3 - 7. Key files that are provided are: 

`table1_mse0-4_species1-2_Ricker_20260819` : qMSE results for all runs, aggregated and formatted to make table 1 (Ricker stock-recruit model)

`table1_mse0-4_species1-2_BH_20260819` : qMSE results from all runs, aggregated and formatted to make table 1 (BH stock-recruit model)

`table2_qnm0-5_hcra-e_qualitative_20240923`: QNM results as qualitative categories, aggregated to make table 2

`table2_qnm0-5_hcra-e_pSims_20240923`: QNM results as the proportion of stable matrices with the same directional output, aggregated to make table 2

`mseVqnm_prob-psim_species1-2_allHCRallOM_20240923_2026review`: matches the qMSE output (as probability of increase) to the QNM output (as proportion stable matrices) for every case (qMSE using Ricker model). This master file is used to produce the rest of the output files in script 3.

`mseVqnm_bh_prob-psim_species1-2_allHCRallOM_20240923_2026review`: matches the qMSE output (as probability of increase) to the QNM output (as proportion stable matrices) for every case (qMSE using BH model). This master file is used to produce the rest of the output files in script 3b.

The subfolder **split_tc** contains results for which the catch metrics were split into large/mature and small/immature fish. 