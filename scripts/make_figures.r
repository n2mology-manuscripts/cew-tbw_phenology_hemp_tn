# Seasonal abundance, composition and variety associations; separate nitrogen experiment.
# Read estimates; interpolate observed rates for visual guidance.
options(warn = 2)
out <- file.path(work_dir, "diagnostics/figures")
figout <- "results/figures"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
inputs <- c(file.path(work_dir, "tables/seasonal_counts.csv"), file.path(work_dir, "tables/variety_estimates.csv"),
            file.path(work_dir, "tables/species_composition.csv"), file.path(work_dir, "tables/nitrogen_contrasts.csv"),
            file.path(work_dir, "diagnostics/nitrogen_whole_plot_availability.csv"),
            file.path(work_dir, "processed/pest_observations.csv"), file.path(work_dir, "tables/nitrogen_variety_estimates.csv"),
            file.path(work_dir, "processed/nitrogen_variety_block_rates.csv"))
hashes <- vapply(inputs, digest::digest, character(1), algo = "sha256", file = TRUE)
read <- function(path) read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
s <- read(inputs[1]); v <- read(inputs[2]); c <- read(inputs[3]); n <- read(inputs[4])
w <- read(inputs[5]); p <- read(inputs[6])
nv <- read(inputs[7]); nb <- read(inputs[8])
for(j in seq_len(nrow(nv))) {
  a <- nb[nb$species==nv$species[j] & nb$variety==nv$variety[j],]
  check_n <- nrow(a)==3 && sum(a$larvae)==nv$numerator[j] && sum(a$plant_visits)==nv$denominator[j]
  stopifnot(check_n,abs(sum(a$larvae)/sum(a$plant_visits)-nv$estimate[j])<1e-10)
}
all_seasonal <- s
s <- s[s$experiment == "variety_sampling", ]
all_composition <- c
c <- c[c$experiment == "variety_sampling", ]
p <- p[p$experiment == "variety_sampling" & p$observed, ]
sp <- c("Chloridea virescens", "Helicoverpa zea")
cols <- c("#B44A00", "#006A9E"); shapes <- c(16, 17)
sp_labels <- c("C. virescens", "H. zea")
keys <- c("2022 ETREC", "2023 ETREC", "2022 HRREC", "2023 HRREC", "2023 NETREC")
key <- function(z) paste(z$year, z$site)
site_label <- function(x) ifelse(grepl(" ETREC$", x), paste0(x, "-PS"), x)
same <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-10))
checks <- data.frame(check = character(), passed = logical())
check <- function(name, result) {
  checks[nrow(checks) + 1, ] <<- list(name, isTRUE(result))
  if (!isTRUE(result)) stop(name)
}
check("Five variety site-years; nitrogen excluded", setequal(unique(key(s)), keys) && nrow(c) == 5)
check("Species pairs have matching observed plant effort", all(vapply(split(s, paste(key(s), s$collection_date)), function(z) nrow(z) == 2 && length(unique(z$plant_visits)) == 1, logical(1))))
check("Seasonal totals equal observed species records", same(sum(s$larvae), sum(p$larvae)))
check("Composition denominator equals seasonal totals", same(sum(c$denominator), sum(s$larvae)))
check("Four nitrogen variety estimates reconstructed from three blocks",nrow(nv)==4 && all(nv$clusters==3) && sum(nv$numerator)==1134 && sum(nv$denominator)/2==298)
# Row summaries reveal replication; they are not independent plant observations.
row_rates <- do.call(rbind, lapply(split(p, paste(p$year, p$site, p$species, p$variety, p$cluster_id)), function(z) {
  data.frame(year = z$year[1], site = z$site[1], species = z$species[1], variety = z$variety[1],
             cluster_id = z$cluster_id[1], larvae = sum(z$larvae), plant_visits = nrow(z), rate = sum(z$larvae) / nrow(z))
}))
for (i in seq_len(nrow(v))) {
  a <- row_rates[key(row_rates) == key(v)[i] & row_rates$species == v$species[i] & row_rates$variety == v$variety[i], ]
  check(paste("Row reconstruction", key(v)[i], v$species[i], v$variety[i]),
        nrow(a) == v$clusters[i] && same(sum(a$larvae), v$numerator[i]) &&
        same(sum(a$plant_visits), v$denominator[i]) && same(sum(a$larvae) / sum(a$plant_visits), v$estimate[i]))
}
write.csv(row_rates, file.path(out, "variety_row_rates.csv"), row.names = FALSE, na = "")
library(grid)
composition <- c
composition$cvir_count <- composition$denominator - composition$numerator
composition$cvir_share <- 1 - composition$estimate
composition$cvir_lower_95 <- 1 - composition$upper_95
composition$cvir_upper_95 <- 1 - composition$lower_95
check("Both species composition sums to one", all(abs(composition$cvir_share + composition$estimate - 1) < 1e-12))
check("Complementary intervals remain ordered", all(composition$cvir_lower_95 <= composition$cvir_share & composition$cvir_share <= composition$cvir_upper_95))
write.csv(composition, file.path(out, "composition_both_species.csv"), row.names = FALSE)
# Darkened species colors support white text on the composition strips.
luminance <- function(color) {
  z <- as.numeric(col2rgb(color))/255
  z <- ifelse(z <= .04045, z/12.92, ((z+.055)/1.055)^2.4)
  sum(z*c(.2126,.7152,.0722))
}
contrast <- 1.05/(vapply(cols, luminance, numeric(1))+.05)
check("Species colors exceed 4.5:1 contrast against white", all(contrast > 4.5))
write.csv(data.frame(species=sp,color=cols,contrast_against_white=contrast), file.path(out,"color_contrast.csv"), row.names=FALSE)
txt <- function(label, x, y, size=8.5, bold=FALSE, col="#171717", just="centre", rot=0, italic=FALSE) {
  # Real text strings (including species) remain editable in SVG exports.
  grid.text(label, x=x, y=y, just=just, rot=rot, gp=gpar(fontfamily="sans",fontsize=size,fontface=if(italic)"italic" else if(bold)"bold" else "plain",col=col))
}
seg <- function(x0,y0,x1,y1,col="#343434",lwd=1) grid.segments(x0,y0,x1,y1,gp=gpar(col=col,lwd=lwd))
dot <- function(x,y,i,size=1.55,col=cols[i],open=FALSE) grid.points(x,y,default.units="npc",pch=if(open)if(i==1)21 else 24 else shapes[i],size=unit(size,"mm"),gp=gpar(col=col,fill=if(open)"white" else col,lwd=.85))
# Center the complete legend, including symbols and the rendered text widths.
species_legend <- function(y, size=9) {
  widths <- vapply(sp_labels, function(label) convertWidth(grobWidth(textGrob(label,
    gp=gpar(fontfamily="sans",fontsize=size,fontface="italic"))), "npc", valueOnly=TRUE), numeric(1))
  marker <- convertWidth(unit(4,"mm"),"npc",valueOnly=TRUE)
  gap <- convertWidth(unit(7,"mm"),"npc",valueOnly=TRUE)
  x <- (1-sum(widths)-2*marker-gap)/2
  for(i in 1:2) {
    dot(x+marker/3,y,i,1.7)
    txt(sp_labels[i],x+marker,y,size=size,just="left",italic=TRUE)
    x <- x+marker+widths[i]+gap
  }
}
# Interpolate only inside sampled windows. Verify every segment stays between
# its observed endpoints; no new peaks, negative rates, or fitted confidence bands.
curve_key <- function(z) paste(z$year,z$site,z$experiment,z$species)
curves <- lapply(split(all_seasonal,curve_key(all_seasonal)),function(z) {
  z <- z[order(z$collection_date),]; xx <- as.numeric(as.Date(z$collection_date)); yy <- z$larvae_per_plant
  # Weighted harmonic slopes are zero at sign changes and flat segments.
  # Zero endpoint slopes constrain the ends without extrapolation.
  h <- diff(xx);delta <- diff(yy)/h;m <- numeric(length(xx))
  for(k in seq.int(2,length(xx)-1)) {
    if(delta[k-1]*delta[k]>0) {
      w1<-2*h[k]+h[k-1];w2<-h[k]+2*h[k-1]
      m[k]<-(w1+w2)/(w1/delta[k-1]+w2/delta[k])
    }
  }
  f <- splinefunH(xx,yy,m)
  gridx <- sort(unique(c(xx,seq(min(xx),max(xx),length.out=1201))))
  gridy <- f(gridx); ii <- pmin(findInterval(gridx,xx),length(xx)-1L)
  lower <- pmin(yy[ii],yy[ii+1]); upper <- pmax(yy[ii],yy[ii+1])
  check(paste("Interpolation preserves observations and local bounds",curve_key(z)[1]),
        same(f(xx),yy) && all(gridy >= -1e-10) && all(gridy>=lower-1e-10 & gridy<=upper+1e-10))
  data.frame(year=z$year[1],site=z$site[1],experiment=z$experiment[1],species=z$species[1],date_numeric=gridx,rate=gridy)
})
write.csv(do.call(rbind,curves),file.path(out,"interpolated_curves.csv"),row.names=FALSE)
draw_combined <- function() {
  grid.newpage()
  seasonal_center <- mean(c(.112,.527))
  variety_center <- mean(c(.724,.970))
  txt("Seasonal abundance and composition",seasonal_center,.978,size=10,bold=TRUE)
  txt("Variety associations",variety_center,.978,size=10,bold=TRUE)
  txt("Larvae / inspected plant",seasonal_center,.957,size=8.4)
  txt("Larvae / inspected plant visit",variety_center,.957,size=8.4)
  # Site groups span both columns; site names denote locations, not regional replicates.
  tops <- .918 - (0:4)*.177
  bottoms <- tops - .166
  for(g in list(list(ii=1:2,label="ETREC-PS"),list(ii=3:4,label="HRREC"),list(ii=5,label="NETREC"))) {
    yt <- tops[min(g$ii)]+.006; yb <- bottoms[max(g$ii)]-.004
    grid.rect(x=.499,y=(yt+yb)/2,width=.972,height=yt-yb,gp=gpar(fill=NA,col="#626262",lwd=1.1))
    grid.rect(x=.036,y=(yt+yb)/2,width=.046,height=yt-yb,gp=gpar(fill="#EAEDEF",col=NA))
    txt(g$label,.036,(yt+yb)/2,size=9.3,bold=TRUE,rot=90)
    if(length(g$ii)>1) seg(.059,(bottoms[g$ii[1]]+tops[g$ii[2]])/2,.985,(bottoms[g$ii[1]]+tops[g$ii[2]])/2,col="#ACB2B5",lwd=.65)
  }
  for(j in seq_along(keys)) {
    top <- tops[j]; bottom <- bottoms[j]
    d <- s[key(s)==keys[j],]; comp <- composition[key(composition)==keys[j],]
    dates <- sort(unique(as.Date(d$collection_date)))
    # Actual days retain calendar spacing; separate panel windows avoid empty months.
    bounds <- as.numeric(range(dates)) + c(-2,2)
    xl <- .112; xr <- .527; yb <- bottom+.061; yt <- top-.022
    px <- function(date) xl+(as.numeric(date)-bounds[1])/diff(bounds)*(xr-xl)
    ticks <- c(0,5,10,15)
    ticks <- ticks[ticks>=0]; ymax <- max(ticks)
    if(ymax < max(d$larvae_per_plant)) {ticks <- c(ticks,ymax+diff(ticks)[1]);ymax <- max(ticks)}
    py <- function(y) yb+(y/ymax)*(yt-yb)
    txt(paste0(LETTERS[j],"  ",substr(keys[j],1,4)),.073,top-.007,size=9.2,bold=TRUE,just="left")
    txt(paste0("n = ",comp$denominator," larvae"),xr,top-.007,size=7.5,just="right")
    for(t in ticks) {
      seg(xl,py(t),xr,py(t),col=if(t==0)"#393939" else "#D8DDE0",lwd=if(t==0).85 else .5)
      txt(t,xl-.009,py(t),size=7.8,just="right")
    }
    seg(xl,yb,xl,yt,lwd=.85)
    for(i in 1:2) {
      a <- d[d$species==sp[i],];a <- a[order(a$collection_date),]
      smooth <- curves[[curve_key(a)[1]]]
      grid.lines(px(smooth$date_numeric),py(smooth$rate),gp=gpar(col=cols[i],lwd=1.8))
      dot(px(as.Date(a$collection_date)),py(a$larvae_per_plant),i,size=1.4)
    }
    for(dd in as.character(dates)) {
      xx <- px(as.Date(dd));seg(xx,yb,xx,yb-.003,lwd=.6)
      txt(as.integer(format(as.Date(dd),"%d")),xx,yb-.010,size=7.4)
    }
    months <- split(dates,format(dates,"%Y-%m"))
    for(mm in months) {
      ll<-px(min(mm))- .006;rr<-px(max(mm))+.006
      seg(ll,yb-.023,rr,yb-.023,lwd=.85)
      seg(ll,yb-.023,ll,yb-.020,lwd=.85);seg(rr,yb-.023,rr,yb-.020,lwd=.85)
      txt(format(mm[1],"%b"),(ll+rr)/2,yb-.031,size=8,bold=TRUE)
    }
    # Both species occupy a compact 100% strip. The whisker shows uncertainty
    # in their shared boundary; H. zea bounds are its exact complement.
    cy <- bottom+.010; h <- .014; splitx <- xl+comp$cvir_share*(xr-xl)
    grid.rect(x=(xl+splitx)/2,y=cy,width=splitx-xl,height=h,gp=gpar(fill=cols[1],col=NA))
    grid.rect(x=(splitx+xr)/2,y=cy,width=xr-splitx,height=h,gp=gpar(fill=cols[2],col=NA))
    txt(sprintf("%.1f%%",100*comp$cvir_share),(xl+splitx)/2,cy,size=7.8,bold=TRUE,col="white")
    txt(sprintf("%.1f%%",100*comp$estimate),(splitx+xr)/2,cy,size=7.8,bold=TRUE,col="white")
    lo<-xl+comp$cvir_lower_95*(xr-xl);hi<-xl+comp$cvir_upper_95*(xr-xl)
    seg(lo,cy+h/2+.003,hi,cy+h/2+.003,lwd=1.05,col="#111111")
    seg(lo,cy+h/2+.0015,lo,cy+h/2+.0045,lwd=1.05,col="#111111")
    seg(hi,cy+h/2+.0015,hi,cy+h/2+.0045,lwd=1.05,col="#111111")
    # Both species retain the approved grouped row points and intervals.
    vd <- v[key(v)==keys[j],];vv <- sort(unique(vd$variety))
    vl <- .724; vr <- .970; vb <- bottom+.021; vt <- top-.023
    local_rows <- row_rates[key(row_rates)==keys[j],]
    vticks <- seq(0,25,5)
    vticks <- vticks[vticks>=0]; vmax <- max(vticks)
    vx <- function(z) vl+z/vmax*(vr-vl)
    vy <- seq(vt,vb+.008,length.out=length(vv))
    for(t in vticks) {
      seg(vx(t),vb-.004,vx(t),vt+.005,col="#D8DDE0",lwd=.5)
      txt(t,vx(t),bottom+.007,size=7.4)
    }
    for(k in seq_along(vv)) {
      txt(vv[k],vl-.012,vy[k],size=7.8,just="right")
      for(i in 1:2) {
        a <- vd[vd$species==sp[i] & vd$variety==vv[k],]
        yy <- vy[k]+if(i==1).0038 else -.0038
        raw <- row_rates[key(row_rates)==keys[j] & row_rates$species==sp[i] & row_rates$variety==vv[k],]
        raw <- raw[order(raw$cluster_id),]
        dot(vx(raw$rate),yy+seq(-.0017,.0017,length.out=nrow(raw)),i,size=1.1,col=cols[i],open=TRUE)
        if(is.finite(a$upper_95)) seg(vx(a$lower_95),yy,vx(a$upper_95),yy,col=cols[i],lwd=1.65)
        dot(vx(a$estimate),yy,i,size=1.6)
        if(!is.finite(a$upper_95)) txt("CI n/a",vx(a$estimate)+.010,yy,size=6.5,just="left")
      }
    }
  }
}
draw_nitrogen <- function() {
  grid.newpage()
  txt("NETREC | 2022 nitrogen experiment",.5,.962,size=11,bold=TRUE)
  centers <- c(mean(c(.065,.333)),mean(c(.394,.632)),mean(c(.748,.971)))
  txt("Seasonal abundance\nand composition",centers[1],.901,size=9.3,bold=TRUE)
  txt("Variety\nassociations",centers[2],.901,size=9.3,bold=TRUE)
  txt("Nitrogen\ncontrasts",centers[3],.901,size=9.3,bold=TRUE)
  txt("Larvae / inspected plant",centers[1],.835,size=8.3)
  txt("Larvae / inspected plant visit",centers[2],.835,size=7.7)
  txt("Difference from 0 N",centers[3],.835,size=7.7)
  grid.rect(x=.5,y=.435,width=.967,height=.705,gp=gpar(fill=NA,col="#626262",lwd=1.1))
  for(i in 1:3) txt(LETTERS[i],c(.030,.369,.671)[i],.768,size=10,bold=TRUE,just="left")
  d <- all_seasonal[all_seasonal$experiment=="nitrogen",]
  comp <- all_composition[all_composition$experiment=="nitrogen",]
  dates <- sort(unique(as.Date(d$collection_date)));bounds <- as.numeric(range(dates))+c(-2,2)
  xl <- .065;xr <- .333;yb <- .302;yt <- .723
  px <- function(x)xl+(as.numeric(x)-bounds[1])/diff(bounds)*(xr-xl)
  py <- function(y)yb+y/15*(yt-yb)
  for(t in c(0,5,10,15)) {seg(xl,py(t),xr,py(t),col=if(t==0)"#393939" else "#D8DDE0",lwd=.6);txt(t,xl-.010,py(t),size=8,just="right")}
  seg(xl,yb,xl,yt,lwd=.85)
  for(i in 1:2) {
    a <- d[d$species==sp[i],];a<-a[order(a$collection_date),];sm<-curves[[curve_key(a)[1]]]
    grid.lines(px(sm$date_numeric),py(sm$rate),gp=gpar(col=cols[i],lwd=1.8))
    dot(px(as.Date(a$collection_date)),py(a$larvae_per_plant),i,size=1.6)
  }
  for(dd in as.character(dates)) {xx<-px(as.Date(dd));seg(xx,yb,xx,yb-.006,lwd=.6);txt(as.integer(format(as.Date(dd),"%d")),xx,yb-.024,size=8)}
  for(mm in split(dates,format(dates,"%Y-%m"))) {
    ll<-px(min(mm))-.008;rr<-px(max(mm))+.008
    seg(ll,yb-.050,rr,yb-.050,lwd=.85);seg(ll,yb-.050,ll,yb-.042,lwd=.85);seg(rr,yb-.050,rr,yb-.042,lwd=.85)
    txt(format(mm[1],"%b"),(ll+rr)/2,yb-.071,size=8,bold=TRUE)
  }
  txt(paste0("n = ",comp$denominator," larvae"),xr,.756,size=7.8,just="right")
  cy<-.144;h<-.027;splitx<-xl+(1-comp$estimate)*(xr-xl)
  grid.rect(x=(xl+splitx)/2,y=cy,width=splitx-xl,height=h,gp=gpar(fill=cols[1],col=NA))
  grid.rect(x=(splitx+xr)/2,y=cy,width=xr-splitx,height=h,gp=gpar(fill=cols[2],col=NA))
  txt(sprintf("%.1f%%",100*(1-comp$estimate)),(xl+splitx)/2,cy,size=8,bold=TRUE,col="white")
  txt(sprintf("%.1f%%",100*comp$estimate),(splitx+xr)/2,cy,size=8,bold=TRUE,col="white")
  lo<-xl+(1-comp$upper_95)*(xr-xl);hi<-xl+(1-comp$lower_95)*(xr-xl)
  seg(lo,cy+.022,hi,cy+.022,lwd=1.05,col="#111111");seg(lo,cy+.018,lo,cy+.026,lwd=1.05);seg(hi,cy+.018,hi,cy+.026,lwd=1.05)
  # Variety rates pool the observed N treatments; open marks are whole blocks.
  # This is a descriptive association, not an N-adjusted variety effect.
  seg(.356,.120,.356,.764,col="#ACB2B5",lwd=.65)
  seg(.658,.120,.658,.764,col="#ACB2B5",lwd=.65)
  vx_var <- function(z).394+z/25*(.632-.394)
  vv <- c("Cherry Wine","Sweetened");vy_var <- c(.592,.315)
  for(k in 1:2) {
    txt(if(k==1)"Cherry Wine" else "Sweetened",centers[2],vy_var[k]+.092,size=8.4,bold=TRUE)
    for(t in seq(0,25,5)) {
      seg(vx_var(t),vy_var[k]-.054,vx_var(t),vy_var[k]+.054,col="#D8DDE0",lwd=.5)
      txt(t,vx_var(t),vy_var[k]-.077,size=7.2)
    }
    for(i in 1:2) {
      a<-nv[nv$species==sp[i] & nv$variety==vv[k],]
      raw<-nb[nb$species==sp[i] & nb$variety==vv[k],];raw<-raw[order(raw$replicate_block),]
      yy<-vy_var[k]+if(i==1).021 else -.021
      dot(vx_var(raw$rate),yy+seq(-.008,.008,length.out=nrow(raw)),i,size=1.2,open=TRUE)
      seg(vx_var(a$lower_95),yy,vx_var(a$upper_95),yy,col=cols[i],lwd=1.65)
      dot(vx_var(a$estimate),yy,i,size=1.7)
    }
  }
  # Common contrast scale for both species; complete-block dots remain visible.
  vl<-.748;vr<-.971;vx<-function(z)vl+(z+20)/40*(vr-vl)
  for(i in 1:2) {
    d<-n[n$species==sp[i],];d<-d[order(d$nitrogen_lb_acre),]; ys<-if(i==1)seq(.685,.485,length.out=4) else seq(.360,.160,length.out=4)
    txt(sp_labels[i],centers[3],max(ys)+.051,size=8.8,col=cols[i],italic=TRUE)
    for(t in seq(-20,20,10))seg(vx(t),min(ys)-.023,vx(t),max(ys)+.025,col=if(t==0)"#686868" else "#D8DDE0",lwd=if(t==0).85 else .5)
    for(j in 1:4) {
      txt(paste0(d$nitrogen_lb_acre[j]," (",d$blocks[j],")"),vl-.015,ys[j],size=8,just="right")
      b<-diffs[diffs$species==sp[i] & diffs$nitrogen_lb_acre==d$nitrogen_lb_acre[j],]
      dot(vx(b$difference),ys[j]+seq(-.009,.009,length.out=nrow(b)),i,size=1.2,open=TRUE)
      seg(vx(d$lower_95[j]),ys[j],vx(d$upper_95[j]),ys[j],col=cols[i],lwd=1.7)
      dot(vx(d$estimate[j]),ys[j],i,size=1.7)
    }
    for(t in seq(-20,20,10))txt(t,vx(t),min(ys)-.049,size=7.4)
  }
  txt("lb N/acre (blocks)",.724,.736,size=7.3)
  species_legend(.034)
}
# Independently reconstruct the block contrasts used in Figure 2.
w <- w[is.finite(w$rate) & w$observed_subplots==2,]
diffs <- do.call(rbind,lapply(split(w,paste(w$species,w$replicate_block)),function(z) {
  base<-z$rate[z$nitrogen_lb_acre==0];stopifnot(length(base)==1)
  a<-z[z$nitrogen_lb_acre!=0,];a$difference<-a$rate-base;a
}))
for(j in seq_len(nrow(n))) {
  a<-diffs[diffs$species==n$species[j] & diffs$nitrogen_lb_acre==n$nitrogen_lb_acre[j],]$difference
  half<-qt(.975,length(a)-1)*sd(a)/sqrt(length(a))
  check(paste("Nitrogen contrast reconstruction",n$species[j],n$nitrogen_lb_acre[j]),length(a)==n$blocks[j] && same(mean(a),n$estimate[j]) && same(mean(a)-half,n$lower_95[j]) && same(mean(a)+half,n$upper_95[j]))
}
write.csv(diffs,file.path(out,"nitrogen_block_differences.csv"),row.names=FALSE)
check("Nitrogen seasonal total and effort",sum(all_seasonal$larvae[all_seasonal$experiment=="nitrogen"])==1134 && sum(all_seasonal$plant_visits[all_seasonal$experiment=="nitrogen"])/2==298)
check("Common axes include all plotted values",max(v$upper_95,row_rates$rate,nv$upper_95,nb$rate,na.rm=TRUE)<=25 && max(all_seasonal$larvae_per_plant)<=15 && max(abs(c(n$lower_95,n$upper_95,diffs$difference)))<=20)
export <- function(name,fun,height) {
  png(file.path(figout,paste0(name,".png")),width=7.2,height=height,units="in",res=360,pointsize=9)
  fun();dev.off()
  pdf(file.path(figout,paste0(name,".pdf")),width=7.2,height=height,pointsize=9,useDingbats=FALSE)
  fun();dev.off()
  svglite::svglite(file.path(figout,paste0(name,".svg")),width=7.2,height=height,pointsize=9,
                  standalone=TRUE,fix_text_size=TRUE,system_fonts=list(sans="Arial"))
  fun();dev.off()
}
export("figure_1",function(){draw_combined();species_legend(.018)},9.5)
export("figure_2",draw_nitrogen,4.6)
check("Analytical inputs unchanged",identical(hashes,vapply(inputs,digest::digest,character(1),algo="sha256",file=TRUE)))
write.csv(data.frame(path=inputs,sha256=unname(hashes)),file.path(out,"input_manifest.csv"),row.names=FALSE)
write.csv(checks,file.path(out,"validation.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"session_info.txt"))
cat(nrow(checks),"checks passed; two manuscript data figures generated.\n")

# Render the native SVGs with a second graphics engine for export review.
for (name in c("figure_1", "figure_2")) {
  rsvg::rsvg_png(file.path(figout, paste0(name, ".svg")),
                 file.path(out, paste0(name, "_svg_render.png")), width=1440)
}
