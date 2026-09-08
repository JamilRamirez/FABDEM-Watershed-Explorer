# Radiografia de candidatos del snap alrededor de la estacion Mazan DHN.
# No cambia logica productiva.

options(warn=1)
root <- normalizePath(".",winslash="/",mustWork=TRUE); setwd(root)
app_env <- new.env(parent=globalenv())
source("app.R",local=app_env,echo=FALSE,print.eval=FALSE,encoding="UTF-8")

lon <- -73.091694; lat <- -3.496528
out_dir <- "qa_results/napo_candidates"; dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

cache <- new.env(parent=emptyenv())
cache$block_id<-NULL;cache$grid_template<-NULL;cache$reverse_cache<-NULL;cache$stream_cache<-NULL
cache$stripe_rows<-NULL;cache$n_stripes<-NULL;cache$stream_threshold_cells<-NULL;cache$stream_threshold_km2<-NULL
block <- app_env$find_block_for_click(lon,lat); bid <- as.character(block$BLOCK_ID[1])
app_env$load_block_if_needed(bid,cache)

cand <- app_env$snap_collect_stream_candidates(
  lon=lon,lat=lat,radius_m=1500,
  grid_template=cache$grid_template,
  stream_cache=cache$stream_cache,
  stripe_rows=cache$stripe_rows,
  n_stripes=cache$n_stripes
)
if(nrow(cand)<1) stop("Sin candidatos a 1500 m")

comp <- app_env$snap_stream_component_ids(cand$cell,terra::ncol(cache$grid_template))
cand$component <- comp

# Conteo inverso capado: suficiente para distinguir una rama local de un gran tronco.
count_capped <- function(reverse_cache,outlet_cell,cap=120000000){
  nc<-as.double(reverse_cache$metadata$ncols); nr<-as.double(reverse_cache$metadata$nrows)
  frontier<-as.double(outlet_cell); n<-1; level<-0L
  repeat{
    if(n>=cap) return(list(n_cells=as.double(cap),capped=TRUE,levels=level))
    if(length(frontier)==0) return(list(n_cells=as.double(n),capped=FALSE,levels=level))
    level<-level+1L; if(level>app_env$MAX_TRACE_LEVELS) return(list(n_cells=as.double(n),capped=TRUE,levels=level))
    vr<-app_env$get_reverse_values(reverse_cache,frontier)
    rows<-floor((frontier-1)/nc)+1; cols<-((frontier-1)%%nc)+1
    parents<-numeric(0)
    use<-bitwAnd(vr,1L)!=0L & rows>1 & cols>1; if(any(use)) parents<-c(parents,frontier[use]-nc-1)
    use<-bitwAnd(vr,2L)!=0L & rows>1; if(any(use)) parents<-c(parents,frontier[use]-nc)
    use<-bitwAnd(vr,4L)!=0L & rows>1 & cols<nc; if(any(use)) parents<-c(parents,frontier[use]-nc+1)
    use<-bitwAnd(vr,8L)!=0L & cols>1; if(any(use)) parents<-c(parents,frontier[use]-1)
    use<-bitwAnd(vr,16L)!=0L & cols<nc; if(any(use)) parents<-c(parents,frontier[use]+1)
    use<-bitwAnd(vr,32L)!=0L & rows<nr & cols>1; if(any(use)) parents<-c(parents,frontier[use]+nc-1)
    use<-bitwAnd(vr,64L)!=0L & rows<nr; if(any(use)) parents<-c(parents,frontier[use]+nc)
    use<-bitwAnd(vr,128L)!=0L & rows<nr & cols<nc; if(any(use)) parents<-c(parents,frontier[use]+nc+1)
    if(length(parents)==0) return(list(n_cells=as.double(n),capped=FALSE,levels=level))
    parents<-unique(parents); n<-n+length(parents); frontier<-parents
  }
}

components <- sort(unique(cand$component))
summary_rows <- vector("list",length(components))
for(i in seq_along(components)){
  cc<-components[i]; idx<-which(cand$component==cc)
  best<-idx[which.min(cand$distance_m[idx])]
  cell<-cand$cell[best]
  xy<-terra::xyFromCell(cache$grid_template,cell)
  p<-sf::st_sfc(sf::st_point(c(xy[1,1],xy[1,2])),crs=sf::st_crs(terra::crs(cache$grid_template)))
  pw<-sf::st_transform(p,4326); ll<-sf::st_coordinates(pw)[1,]
  t0<-Sys.time(); cnt<-count_capped(cache$reverse_cache,cell); secs<-as.numeric(difftime(Sys.time(),t0,units="secs"))
  summary_rows[[i]]<-data.frame(
    COMPONENT=cc,
    N_STREAM_CELLS_IN_1500M=length(idx),
    NEAREST_DISTANCE_M=cand$distance_m[best],
    OUTLET_CELL=cell,
    OUTLET_LON=ll[1],OUTLET_LAT=ll[2],
    UPSTREAM_CELLS_CAPPED=cnt$n_cells,
    HIT_120M_CAP=cnt$capped,
    COUNT_TIME_S=secs,
    stringsAsFactors=FALSE
  )
  print(summary_rows[[i]])
}
summary_df<-do.call(rbind,summary_rows)
summary_df<-summary_df[order(summary_df$NEAREST_DISTANCE_M),]
write.csv(summary_df,file.path(out_dir,"napo_components_1500m.csv"),row.names=FALSE)

# Tambien deja la decision actual por radios para compararla.
sel<-app_env$snap_select_progressive_candidate(cand,app_env$snap_progressive_radii(1500),terra::ncol(cache$grid_template))
writeLines(capture.output(str(sel)),file.path(out_dir,"current_selection.txt"))
print(summary_df)
print(sel)
