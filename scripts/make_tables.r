# Exact numerical inputs remain in the external working directory. Public tables
# retain all rows and precision, with measurement names instead of generic ratios.
for(nm in names(summary_tables)) write_table(summary_tables[[nm]],file.path(work_dir,"tables",paste0(nm,".csv")))
public <- c(sampling_effort="table_s1_sampling_effort",seasonal_counts="table_s2_seasonal_counts",
            variety_estimates="table_s3_variety_estimates",species_composition="table_s4_species_composition",
            nitrogen_design="table_s5_nitrogen_design",nitrogen_contrasts="table_s6_nitrogen_contrasts",
            nitrogen_variety_estimates="table_s7_nitrogen_variety_estimates")
variety_labels <- c(BuddhaBelly="Buddha Belly",CherryBubble="Cherry Bubble",CherryBubblegum="Cherry Bubblegum",
                    CherryTrump="Cherry Trump",HillbillyHash="Hillbilly Hash",CherryWine="Cherry Wine",T1xCherryWine="T1 x Cherry Wine")
for(nm in names(public)) {
  tab <- summary_tables[[nm]]
  if("site" %in% names(tab)) tab$site[tab$site=="ETREC"] <- "ETREC-PS"
  if("experiment" %in% names(tab)) tab$experiment <- ifelse(tab$experiment=="nitrogen","Nitrogen experiment","Variety sampling")
  if("variety" %in% names(tab)) {
    hit <- tab$variety %in% names(variety_labels);tab$variety[hit] <- unname(variety_labels[tab$variety[hit]])
  }
  rename <- c(numerator="Larvae",denominator="Observed plant visits",estimate="Larvae per plant visit",
              clusters="Rows or blocks",lower_95="95% lower limit",upper_95="95% upper limit")
  if(nm=="species_composition") rename[c("numerator","denominator","estimate")] <- c("H. zea larvae","Both-species larvae","H. zea proportion")
  if(nm=="nitrogen_contrasts") rename["estimate"] <- "Rate difference from zero N"
  if(nm=="nitrogen_design") rename <- c(nitrogen_lb_acre="Nitrogen (lb N/acre)",nitrogen_kg_ha="Nitrogen (kg N/ha)")
  names(tab) <- vapply(names(tab),function(x)if(x %in% names(rename))rename[[x]] else {
    x <- gsub("_"," ",x);paste0(toupper(substr(x,1,1)),substring(x,2))
  },character(1))
  write_table(tab,file.path("results/tables",paste0(public[[nm]],".csv")))
}
