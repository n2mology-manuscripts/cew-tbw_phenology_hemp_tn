group_apply <- function(d, keys, fun) {
  grouping <- lapply(d[keys],function(v)ifelse(is.na(v),"<missing>",as.character(v)))
  groups <- split(seq_len(nrow(d)), interaction(grouping, drop=TRUE, lex.order=TRUE))
  ans <- lapply(groups, function(i) {result<-fun(d[i,,drop=FALSE]); labels<-d[rep(i[1],nrow(result)),keys,drop=FALSE];rownames(labels)<-NULL;cbind(labels,result)})
  out <- do.call(rbind,ans); rownames(out) <- NULL; out
}
# Link repeated observations using the recorded scientific units.
field_units <- function(x) {
  x$collection_date <- as.Date(x$date)
  # Use the short site code internally; public inputs and tables use ETREC-PS.
  x$site[x$site=="ETREC-PS"] <- "ETREC"
  nitrogen <- x$experiment=="nitrogen"
  stopifnot(all(!is.na(x$subplot[nitrogen])),all(!is.na(x$whole_plot[nitrogen])),
            all(x$replicate_block[nitrogen] %in% 1:3),
            all(x$whole_plot[nitrogen] %in% 1:5),
            all(x$nitrogen_lb_acre[nitrogen] %in% c(0,50,100,150,200)),
            all(is.na(x$nitrogen_lb_acre[!nitrogen])),
            all(!is.na(x$row[!nitrogen])),
            all(x$replicate_block[!nitrogen]==x$row[!nitrogen]),
            all(!is.na(x$group[x$year==2023])))
  x$cluster_id <- paste(x$year,x$site,x$replicate_block,sep="_")
  x$whole_plot_id <- ifelse(nitrogen,paste(x$cluster_id,x$whole_plot,sep="_n"),NA)
  x$unit_id <- ifelse(nitrogen,paste(x$year,x$site,x$subplot,sep="_"),
    ifelse(x$year==2022,paste(x$cluster_id,x$plant,sep="_p"),paste(x$cluster_id,x$group,sep="_g")))
  x$plant_id <- ifelse(x$year==2022 & !nitrogen,x$unit_id,paste(x$unit_id,x$plant,sep="_p"))
  x$sample_id <- paste(x$plant_id,x$collection_date,sep="_")
  x$nitrogen_kg_ha <- x$nitrogen_lb_acre*1.120851156
  x$observed <- !is.na(x$larvae)
  x$availability <- ifelse(x$observed,"observed",ifelse(nitrogen,"unavailable_harvest_or_death","unavailable_dead_plant"))
  stopifnot(!anyDuplicated(x[c("plant_id","collection_date","species")]))
  stopifnot(all(table(x$sample_id)==2),!any(x$year==2023 & x$site=="ETREC" & x$row==1))
  pairs <- split(x,x$sample_id)
  stopifnot(all(vapply(pairs,function(z)length(unique(z$observed))==1,logical(1))))
  mapping <- unique(x[c("unit_id","variety","cluster_id","whole_plot_id","nitrogen_lb_acre")])
  stopifnot(!anyDuplicated(mapping$unit_id))
  n <- mapping[!is.na(mapping$whole_plot_id),]
  stopifnot(nrow(n)==30,length(unique(n$whole_plot_id))==15,length(unique(n$cluster_id))==3)
  stopifnot(all(vapply(split(n,n$whole_plot_id),function(z)nrow(z)==2 && length(unique(z$nitrogen_lb_acre))==1 && length(unique(z$variety))==2,logical(1))))
  missing <- x[!x$observed,]
  stopifnot(nrow(missing)==256,all(missing$year==2022))
  e <- missing[missing$site=="ETREC",]
  stopifnot(nrow(e)==12,all((e$row==5 & e$plant==6) | (e$row==4 & e$plant==10)))
  stopifnot(!any(x$collection_date==as.Date("2022-09-21")))
  write_table(mapping,file.path(work_dir,"processed/physical_units.csv"))
  write_table(missing,file.path(work_dir,"diagnostics/unavailable_records.csv"))
  x
}

# Ratio of observed totals, resampling entire rows/blocks and all their repeat visits.
# Marginal descriptive uncertainty; few clusters and schedule confounding limit inference.
cluster_ratio <- function(z, numerator, denominator, B=4999) {
  a <- aggregate(z[c(numerator,denominator)],list(cluster=z$cluster_id),sum)
  a <- a[a[[denominator]]>0,,drop=FALSE]
  n <- nrow(a); num <- sum(a[[numerator]]); den <- sum(a[[denominator]])
  est <- if(den>0)num/den else NA_real_
  ci <- c(NA_real_,NA_real_)
  if(n>=3) {
    draws <- replicate(B,{i<-sample.int(n,n,replace=TRUE);sum(a[[numerator]][i])/sum(a[[denominator]][i])})
    ci <- quantile(draws,c(.025,.975),names=FALSE)
    # An all-zero numerator has no empirically estimable upper uncertainty bound.
    if(all(a[[numerator]]==0)) ci <- c(NA_real_,NA_real_)
  }
  data.frame(numerator=num,denominator=den,clusters=n,estimate=est,lower_95=ci[1],upper_95=ci[2])
}
