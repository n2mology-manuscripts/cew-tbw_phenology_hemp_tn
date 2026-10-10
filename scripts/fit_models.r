# Design-based estimation; see data/README.md. No independent-plant GLM.
set.seed(20260918)
v <- obs[obs$experiment=="variety_sampling",]
summary_tables$variety_estimates <- group_apply(v,c("year","site","species","variety"),function(z)cluster_ratio(z,"larvae","effort"))
summary_tables$species_composition <- group_apply(obs,c("year","site","experiment"),function(z) {
  z$hzea <- ifelse(z$species=="Helicoverpa zea",z$larvae,0)
  cluster_ratio(z,"hzea","larvae")
})
# Each whole plot averages its two variety subplot rates equally.
n <- obs[obs$experiment=="nitrogen",]
subplots <- group_apply(n,c("replicate_block","whole_plot_id","unit_id","variety","nitrogen_lb_acre","species"),function(z)
  data.frame(larvae=sum(z$larvae),plant_visits=nrow(z),rate=sum(z$larvae)/nrow(z)))
write_table(subplots,file.path(work_dir, "processed/nitrogen_subplot_rates.csv"))
whole <- group_apply(subplots,c("replicate_block","whole_plot_id","nitrogen_lb_acre","species"),function(z)data.frame(observed_subplots=nrow(z),rate=if(nrow(z)==2)mean(z$rate) else NA_real_))
write_table(whole,file.path(work_dir, "diagnostics/nitrogen_whole_plot_availability.csv"))
whole <- whole[!is.na(whole$rate),]
mean_ci <- function(v) {n<-length(v); se<-sd(v)/sqrt(n); data.frame(blocks=n,estimate=mean(v),lower_95=mean(v)-qt(.975,n-1)*se,upper_95=mean(v)+qt(.975,n-1)*se)}
summary_tables$nitrogen_estimates <- group_apply(whole,c("nitrogen_lb_acre","species"),function(z)mean_ci(z$rate))
diffs <- group_apply(whole,c("replicate_block","species"),function(z) {
  base <- z$rate[z$nitrogen_lb_acre==0];stopifnot(length(base)==1)
  data.frame(nitrogen_lb_acre=z$nitrogen_lb_acre[z$nitrogen_lb_acre!=0],difference=z$rate[z$nitrogen_lb_acre!=0]-base)
})
summary_tables$nitrogen_contrasts <- group_apply(diffs,c("species","nitrogen_lb_acre"),function(z)mean_ci(z$difference))
diag <- group_apply(obs,c("year","site","experiment","species"),function(z) {
  m<-mean(z$larvae);data.frame(records=nrow(z),clusters=length(unique(z$cluster_id)),mean=m,variance=var(z$larvae),variance_to_mean=if(m>0)var(z$larvae)/m else NA,zero_fraction=mean(z$larvae==0))
})
write_table(diag,file.path(work_dir, "diagnostics/count_dispersion.csv"))
# NETREC variety associations describe observed rates pooled across N treatments.
# Whole blocks retain the subplot allocation and repeated visits. No N-adjusted
# variety effect is claimed; plot 310 remains unavailable and is not imputed.
set.seed(20260929)
summary_tables$nitrogen_variety_estimates <- group_apply(n,c("species","variety"),function(z)cluster_ratio(z,"larvae","effort"))
nitrogen_variety_blocks <- group_apply(n,c("species","variety","replicate_block"),function(z)
  data.frame(larvae=sum(z$larvae),plant_visits=nrow(z),rate=sum(z$larvae)/nrow(z)))
write_table(nitrogen_variety_blocks,file.path(work_dir, "processed/nitrogen_variety_block_rates.csv"))
