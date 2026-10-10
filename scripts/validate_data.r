# Validate the analysis inputs; field_units.r checks allocation, repeated visits,
# species pairing, exclusions and unavailable observations.
required <- c("year","site","date","experiment","replicate_block","whole_plot",
              "row","group","subplot","sampling_range","plant","variety","species",
              "nitrogen_lb_acre","larvae")
x <- rbind(read_table("data/observations_2022.csv"),read_table("data/observations_2023.csv"))
stopifnot(identical(names(x),required),nrow(x)==2468L,
          all(x$year %in% c(2022,2023)),all(!is.na(as.Date(x$date))),
          all(x$site %in% c("ETREC-PS","HRREC","NETREC")),
          all(x$species %in% c("Helicoverpa zea","Chloridea virescens")),
          all(x$experiment %in% c("variety_sampling","nitrogen")),
          all(!is.na(x$variety)),all(!is.na(x$plant)),all(!is.na(x$replicate_block)),
          all(is.na(x$larvae) | (x$larvae>=0 & x$larvae==floor(x$larvae))))
stopifnot(all((x$experiment=="nitrogen") == (x$year==2022 & x$site=="NETREC")))
