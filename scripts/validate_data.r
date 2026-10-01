# Validate observations needed by the analysis; field_units.r checks allocation,
# repeated visits, exclusions, species pairing and unavailable observations.
required <- c("source_workbook","source_sheet","source_row","record_id","sample_id",
              "year","site","date","row_label","range_label","block_label","plot_label",
              "plant_label","variety_original","species_original","nitrogen_original","experiment","larvae")
x <- rbind(read_table("data/observations_2022.csv"),read_table("data/observations_2023.csv"))
stopifnot(all(required %in% names(x)),nrow(x)==2468L,!anyDuplicated(x$record_id),
          all(x$year %in% c(2022,2023)),all(!is.na(as.Date(x$date))),
          all(is.na(x$larvae) | (x$larvae>=0 & x$larvae==floor(x$larvae))))
