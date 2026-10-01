# R dependencies

The analyses were run with R 4.6.1. `r_package_requirements.tsv` lists tested package versions; `renv.lock` records the same versions for restoration. The lockfile contains repository package records and no machine-specific installation paths.

To restore into an external R library, install renv and run from the repository root:

```r
install.packages("renv")
analysis_library <- Sys.getenv("R_LIBS_USER")
stopifnot(nzchar(analysis_library))
dir.create(analysis_library, recursive = TRUE, showWarnings = FALSE)
renv::restore(lockfile = "envs/renv.lock", library = analysis_library, prompt = FALSE)
```

Choose a single existing or new library directory for `R_LIBS_USER` before starting R. Full environments and caches remain outside the repository. Routine reproduction uses `Rscript scripts/run_all.r`. The figures specify Arial; that font or a compatible substitute is needed for the same SVG appearance.
