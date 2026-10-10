# Seasonal abundance of corn earworm and tobacco budworm in Tennessee hemp

This repository contains collection records, R analyses, figures and supporting tables for seasonal abundance, species composition and variety associations of *Helicoverpa zea* and *Chloridea virescens* in Tennessee floral hemp during 2022 and 2023. The NETREC 2022 nitrogen experiment is analyzed separately.

## Data and analyses

`data/` contains the two collection datasets and a data dictionary. Blank counts denote unavailable observations. The scripts retain observed sampling effort, repeated plant visits and the confirmed field units. Species-specific date totals are divided by inspected plants. Seasonal variety estimates use whole-row resampling; nitrogen summaries retain the three-block design and the unavailable subplot.

[Data definitions and analysis notes](data/README.md) describe the design, corrections, exclusions and limits of interpretation. Some collections also support the companion parasitoid study; parasitoid results are reported separately.

## Reproduce the results

Install the packages recorded under [envs/](envs/README.md), then run from the repository root:

```sh
Rscript scripts/run_all.r
```

The command generates two figure sets in `results/figures/`, each as PNG, PDF and editable SVG, and seven supporting CSV tables in `results/tables/`. Intermediate tables and diagnostic renders use an external temporary directory. Set `MANUSCRIPT_WORK_DIR` to retain these files in a separate working directory.

- Figure 1, `figure_1`: seasonal abundance and composition alongside variety associations across five site-years (A-E).
- Figure 2, `figure_2`: NETREC 2022 seasonal abundance and composition (A), pooled variety rates (B), and within-block nitrogen contrasts (C).

[Figure and table captions](results/README.md) define denominators and intervals. SVG labels remain editable; the figures specify Arial. These descriptive estimates do not establish varietal resistance or equivalence among nitrogen treatments.
