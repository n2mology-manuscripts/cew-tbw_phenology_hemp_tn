# Collection records and analysis notes

The two observation files contain 2,468 species-level records from the final curated dissertation datasets. Each observed plant visit has one record per pest species. Source workbook, sheet and row identify the original observation. `data_dictionary.csv` defines every input column. Source workbook bytes and earlier extraction records are retained separately by the author.

The 2022 variety experiment assigned plants within five rows. In 2023, varieties occupied two-plant groups within rows; the two plants were subsamples and repeated visits were retained. ETREC-PS row 1 was excluded after fire-ant damage, leaving four rows there and five elsewhere. Ten 2023 T2 labels were corrected to T1 by the scripts, with original labels retained.

NETREC 2022 used three blocks, five nitrogen rates and two variety subplots per whole plot: 15 whole plots and 30 subplots. Plot 310 has no observed counts. The 150 lb N/acre contrast therefore uses two complete blocks; other contrasts use three. The five ranges are sampling positions. The pooled harvested-plant Shake collection is excluded. The data retain 256 unavailable species records, including 12 from two dead plants in ETREC-PS variety sampling and 244 in the nitrogen experiment. Missing counts are never converted to zero.

Seasonal abundance is the species total divided by observed plants on each collection date. Species composition is H. zea larvae divided by larvae of both pest species. Variety rates pool larvae over observed plant visits. Marginal percentile intervals use 4,999 whole-row resamples (seed 20260918). Entirely zero responses have no estimable bootstrap upper bound; their limits remain blank. Rotating rows and few clusters limit date-level and variety inference.

Nitrogen contrasts compare equally weighted variety-subplot rates within complete whole plots, calculate differences from zero N within blocks, and use t intervals across blocks. Separate nitrogen variety summaries pool the available treatments and use 4,999 whole-block resamples (seed 20260929); they are not adjusted for nitrogen or missingness. Seasonal connectors are constrained Hermite interpolation through observed rates, without extrapolation or inferential confidence bands.

Weather, floral stage and larval size groups are not analyzed. Historical grouped larval counts disagreed with the final collection totals. All plotted dates refer to host collection. Site-year patterns are descriptive; an interval spanning zero does not establish equivalence.
