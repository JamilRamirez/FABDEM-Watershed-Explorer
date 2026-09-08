options(warn = 1)
source_env <- new.env(parent = globalenv())
source('app.R', local = source_env, echo = FALSE, print.eval = FALSE, encoding = 'UTF-8')
delim_env <- environment(source_env$delimitacion$ui)
find_ambiguity_options <- get('find_ambiguity_options', envir = delim_env, inherits = FALSE)

cases <- data.frame(
  CASE_ID=c('Socsi','Huaycoloro','Chira','SantaClotilde','NapoDHN','NapoBellavista'),
  LON=c(-76.194500,-76.882010,-80.691111,-73.630000,-73.091694,-73.073000),
  LAT=c(-13.028300,-11.949380,-4.891389,-2.520000,-3.496528,-3.482000),
  stringsAsFactors=FALSE
)

cache <- new.env(parent=emptyenv())
cache$block_id <- NULL; cache$grid_template <- NULL; cache$reverse_cache <- NULL
cache$stream_cache <- NULL; cache$stripe_rows <- NULL; cache$n_stripes <- NULL
cache$stream_threshold_cells <- NULL; cache$stream_threshold_km2 <- NULL

out <- list()
for (i in seq_len(nrow(cases))) {
  x <- cases[i,]
  started <- Sys.time()
  ans <- tryCatch({
    b <- source_env$find_block_for_click(x$LON,x$LAT)
    source_env$load_block_if_needed(as.character(b[['BLOCK_ID']][1]), cache)
    a <- find_ambiguity_options(
      lon=x$LON, lat=x$LAT,
      radius_m=source_env$DEFAULT_SNAP_RADIUS_M,
      grid_template=cache$grid_template,
      stream_cache=cache$stream_cache,
      stripe_rows=cache$stripe_rows,
      n_stripes=cache$n_stripes
    )
    data.frame(CASE_ID=x$CASE_ID,STATUS=a$status,REASON=a$reason,N_OPTIONS=length(a$options),SECONDS=as.numeric(difftime(Sys.time(),started,units='secs')),ERROR='',stringsAsFactors=FALSE)
  }, error=function(e) data.frame(CASE_ID=x$CASE_ID,STATUS='ERROR',REASON='',N_OPTIONS=NA_integer_,SECONDS=as.numeric(difftime(Sys.time(),started,units='secs')),ERROR=conditionMessage(e),stringsAsFactors=FALSE))
  print(ans)
  out[[i]] <- ans
}
res <- do.call(rbind,out)
dir.create('qa_results/ambiguity-detect',recursive=TRUE,showWarnings=FALSE)
write.csv(res,'qa_results/ambiguity-detect/results.csv',row.names=FALSE)
print(res)
