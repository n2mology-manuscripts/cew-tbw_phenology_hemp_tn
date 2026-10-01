# Reproduce the manuscript figures and supporting tables from included records.
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
if(length(script)) setwd(dirname(dirname(normalizePath(script))))
for(pkg in c("digest", "svglite", "rsvg")) {
  if(!requireNamespace(pkg, quietly=TRUE)) stop("Missing package ",pkg,"; see envs/README.md")
}
work_dir <- Sys.getenv("MANUSCRIPT_WORK_DIR", unset=tempfile("phenology_"))
dir.create(work_dir,recursive=TRUE,showWarnings=FALSE)
repo_path <- paste0(normalizePath(".",winslash="/"),"/")
work_path <- paste0(normalizePath(work_dir,winslash="/"),"/")
if(startsWith(tolower(work_path),tolower(repo_path))) stop("MANUSCRIPT_WORK_DIR must be outside the repository")
for(d in c("processed","tables","diagnostics","diagnostics/figures")) dir.create(file.path(work_dir,d),recursive=TRUE,showWarnings=FALSE)
for(d in c("results/figures","results/tables")) dir.create(d,recursive=TRUE,showWarnings=FALSE)
read_table <- function(path) read.csv(path,stringsAsFactors=FALSE,check.names=FALSE,na.strings="")
write_table <- function(x,path) {dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);write.csv(x,path,row.names=FALSE,na="")}
source("scripts/validate_data.r")
source("scripts/prepare_data.r")
source("scripts/fit_models.r")
source("scripts/make_tables.r")
source("scripts/make_figures.r")
cat("Generated two figure sets and seven supplementary CSV tables.\n")
