source("scripts/field_units.r")
summary_tables <- list()
x <- field_units(rbind(read_table("data/observations_2022.csv"),read_table("data/observations_2023.csv")))
write_table(x,file.path(work_dir, "processed/pest_observations.csv"))
obs <- x[x$observed,]; obs$effort <- 1
summary_tables$seasonal_counts <- group_apply(obs,c("year","site","experiment","collection_date","species"),function(z)
  data.frame(plant_visits=nrow(z),units=length(unique(z$unit_id)),rows_or_blocks=length(unique(z$cluster_id)),larvae=sum(z$larvae),larvae_per_plant=sum(z$larvae)/nrow(z)))
summary_tables$sampling_effort <- group_apply(x,c("year","site","experiment"),function(z)
  data.frame(scheduled_plant_visits=length(unique(z$sample_id)),observed_plant_visits=sum(z$observed)/2,unavailable_plant_visits=sum(!z$observed)/2,physical_units=length(unique(z$unit_id)),observed_units=length(unique(z$unit_id[z$observed])),rows_or_blocks=length(unique(z$cluster_id)) ))
n <- group_apply(x[x$experiment=="nitrogen",],c("subplot","sampling_range","replicate_block","whole_plot","variety","nitrogen_lb_acre","nitrogen_kg_ha"),function(z)
  data.frame(observed_counts=if(any(z$observed)) "Available" else "Unavailable"))
summary_tables$nitrogen_design <- n
