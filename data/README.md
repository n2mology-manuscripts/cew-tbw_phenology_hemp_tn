# Larval collection data and analysis methods

The two observation files contain 2,468 species-level records from larval collections in Tennessee hemp during 2022 and 2023. Each plant visit has one record for *Helicoverpa zea* and one for *Chloridea virescens*. Blank larval counts denote unavailable observations; recorded zeros denote inspected plants with no larvae of that species. `data_dictionary.csv` defines the columns, units and missing values.

## Experimental units and sampling

In the 2022 variety plantings, individual plants were variety units within five replicate rows. In 2023, varieties occupied two-plant groups within rows; plants were subsamples. Repeated visits sampled the same physical units. ETREC-PS row 1 was excluded after fire-ant damage, leaving four rows there and five elsewhere in 2023.

NETREC 2022 was a separate nitrogen experiment with three blocks, five nitrogen rates and two variety subplots per whole plot: 15 whole plots and 30 subplots. `replicate_block` identifies the block and `whole_plot` identifies the whole plot within that block. `subplot` identifies the variety subplot; `plant` distinguishes its plants. `sampling_range` identifies a collection position, not an independent replicate. Nitrogen rates are nitrogen mass, not urea product mass.

For variety sampling, year, site, row and plant identify the 2022 units; year, site, row and group identify the 2023 units, with plant identifying the subsample. Adding collection date identifies a plant visit; adding species identifies a larval count. In the nitrogen experiment, year, site, subplot and plant identify a plant across repeated visits.

Three plants per nitrogen subplot were harvested on 21 September 2022. The pooled collection from harvested plants was used for colony rearing and excluded from statistical sampling. Subsequent availability also reflected plant deaths; individual harvest and death causes could not always be assigned. Subplot 310 has no observed counts. The data include 256 unavailable species-level observations: 12 from two dead plants in ETREC-PS variety sampling and 244 in the nitrogen experiment. Missing counts were not imputed.

Preliminary monitoring lacked complete quantitative counts and sampling effort. Summaries describe the recorded quantitative sampling periods and do not establish infestation onset or absence before those periods. Scheduled inspections included every available plant in the selected row, regardless of flowering status. Some field collections also support the companion parasitoid study; parasitoid results are reported separately.

## Estimates and uncertainty

Seasonal abundance is the species total divided by inspected plants with recorded counts on each collection date. Species composition is *H. zea* larvae divided by larvae of both pest species. Seasonal variety rates pool larvae over observed plant visits. Variety sampling yielded 2,381 larvae from 808 observed plant visits; the nitrogen experiment yielded 1,134 larvae from 298 visits.

Marginal 95% percentile intervals for variety rates and species composition use 4,999 whole-row resamples, retaining plants and repeated visits together. All-zero responses have no reported interval; blank interval bounds do not indicate zero uncertainty. Few rows, rotating sampling rows and differing availability limit inference. Intervals are exploratory and unadjusted for multiple comparisons; pairwise variety comparisons were not tested.

Nitrogen contrasts equally weight the two observed variety-subplot rates within each complete whole plot, calculate differences from zero N within blocks, and use 95% t intervals across complete blocks. Subplot 310 is unavailable, leaving two complete blocks for the 150 lb N/acre contrast and three for other contrasts, with one or two degrees of freedom. Separate nitrogen variety summaries pool available treatments over 145 plant visits for Cherry Wine and 153 for Sweetened. Their intervals use 4,999 whole-block resamples of the three blocks, retaining subplot assignments and repeated visits. These rates are not adjusted for nitrogen or unequal availability.

Seasonal curves use constrained piecewise cubic Hermite interpolation through observed date rates. They are visual summaries without extrapolation, fitted population models or inferential confidence bands. Random seeds for exact computational reproduction are set in `scripts/fit_models.r`.

Floral onset and maturity were informal observations. Weather, floral stage, injury and neighboring crops were not fitted as explanatory variables. Small larvae were recorded as instars 1-3 and large larvae as instars 4-6; individual instars were not recorded. Size-group totals did not agree with the larval collection totals used in this study, so size groups were excluded from quantitative analyses. Site-year patterns are descriptive, variety associations do not establish resistance, and nitrogen intervals spanning zero do not establish equivalence.
