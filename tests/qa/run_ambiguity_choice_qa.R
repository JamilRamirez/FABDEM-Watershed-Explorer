# QA real del selector de cuencas ambiguas
# Ejecuta deteccion sobre Runtime real y rastrea cada opcion propuesta.

options(warn = 1)
root <- normalizePath('.', winslash = '/', mustWork = TRUE)
setwd(root)

app_env <- new.env(parent = globalenv())
source('app.R', local = app_env, echo = FALSE, print.eval = FALSE, encoding = 'UTF-8')

delim_env <- environment(app_env$delimitacion$ui)
find_ambiguity_options <- get('find_ambiguity_options', envir = delim_env, inherits = FALSE)

cases <- data.frame(
  CASE_ID = c('AQA-001','AQA-002','AQA-003','AQA-004','AQA-005','AQA-006'),
  FEATURE = c(
    'Socsi - Rio Canete',
    'Quebrada Huaycoloro',
    'Rio Chira - Sullana',
    'Rio Napo - Santa Clotilde',
    'Rio Napo - Mazan DHN',
    'Rio Napo - Bellavista Mazan'
  ),
  LON = c(-76.194500, -76.882010, -80.691111, -73.630000, -73.091694, -73.073000),
  LAT = c(-13.028300, -11.949380, -4.891389, -2.520000, -3.496528, -3.482000),
  EXPECT = c('single','single','single','ambiguous','ambiguous','ambiguous_or_single'),
  stringsAsFactors = FALSE
)

out_dir <- file.path('qa_results', 'ambiguity-choice')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

new_block_cache <- function() {
  e <- new.env(parent = emptyenv())
  e$block_id <- NULL
  e$grid_template <- NULL
  e$reverse_cache <- NULL
  e$stream_cache <- NULL
  e$stripe_rows <- NULL
  e$n_stripes <- NULL
  e$stream_threshold_cells <- NULL
  e$stream_threshold_km2 <- NULL
  e
}

block_cache <- new_block_cache()
rows_out <- list()
option_rows <- list()

for (ii in seq_len(nrow(cases))) {
  case <- cases[ii, , drop = FALSE]
  cat('\n===== ', case$CASE_ID, ' | ', case$FEATURE, ' =====\n', sep = '')
  started <- Sys.time()

  row <- data.frame(
    CASE_ID = case$CASE_ID,
    FEATURE = case$FEATURE,
    EXPECT = case$EXPECT,
    STATUS = NA_character_,
    REASON = NA_character_,
    N_OPTIONS = NA_integer_,
    TOTAL_S = NA_real_,
    ERROR = NA_character_,
    stringsAsFactors = FALSE
  )

  tryCatch({
    block <- app_env$find_block_for_click(case$LON, case$LAT)
    if (is.null(block) || nrow(block) < 1L) stop('Punto fuera del dominio.')
    block_id <- as.character(block[['BLOCK_ID']][1])
    app_env$load_block_if_needed(block_id, block_cache)

    amb <- find_ambiguity_options(
      lon = case$LON,
      lat = case$LAT,
      radius_m = app_env$DEFAULT_SNAP_RADIUS_M,
      grid_template = block_cache$grid_template,
      stream_cache = block_cache$stream_cache,
      stripe_rows = block_cache$stripe_rows,
      n_stripes = block_cache$n_stripes
    )

    row$STATUS <- as.character(amb$status)
    row$REASON <- as.character(amb$reason)
    row$N_OPTIONS <- length(amb$options)

    cat('STATUS=', row$STATUS, ' REASON=', row$REASON, ' OPTIONS=', row$N_OPTIONS, '\n', sep='')

    for (jj in seq_along(amb$options)) {
      opt <- amb$options[[jj]]
      trace_started <- Sys.time()
      trace <- app_env$trace_upstream(
        cache = block_cache$reverse_cache,
        outlet_cell = opt$outlet_cell,
        progress_fun = function(...) invisible(NULL)
      )
      trace_s <- as.numeric(difftime(Sys.time(), trace_started, units = 'secs'))

      option_rows[[length(option_rows) + 1L]] <- data.frame(
        CASE_ID = case$CASE_ID,
        OPTION = jj,
        ROLE = if (!is.null(opt$ambiguity_role)) as.character(opt$ambiguity_role) else 'single',
        SNAP_MODE = as.character(opt$snap_mode),
        SNAP_DISTANCE_M = as.numeric(opt$snap_distance_m),
        OUTLET_LON = as.numeric(opt$outlet_lon),
        OUTLET_LAT = as.numeric(opt$outlet_lat),
        TRACE_CELLS = as.numeric(trace$n_cells),
        TRACE_ENGINE = as.character(trace$trace_engine),
        TRACE_S = trace_s,
        stringsAsFactors = FALSE
      )

      cat(
        '  option ', jj,
        ' role=', option_rows[[length(option_rows)]]$ROLE,
        ' dist=', round(opt$snap_distance_m), 'm',
        ' cells=', format(trace$n_cells, big.mark=','),
        '\n', sep=''
      )
      rm(trace)
      gc()
    }

  }, error = function(e) {
    row$STATUS <<- 'ERROR'
    row$ERROR <<- conditionMessage(e)
    cat('ERROR: ', conditionMessage(e), '\n', sep='')
  })

  row$TOTAL_S <- as.numeric(difftime(Sys.time(), started, units = 'secs'))
  rows_out[[length(rows_out) + 1L]] <- row
  gc()
}

results <- do.call(rbind, rows_out)
write.csv(results, file.path(out_dir, 'ambiguity_cases.csv'), row.names = FALSE)

if (length(option_rows) > 0L) {
  options <- do.call(rbind, option_rows)
} else {
  options <- data.frame()
}
write.csv(options, file.path(out_dir, 'ambiguity_options.csv'), row.names = FALSE)

print(results)
if (nrow(options) > 0L) print(options)

# Gating estricto para los tres casos donde conocemos la conducta deseada.
lookup <- setNames(results$STATUS, results$CASE_ID)
if (!identical(lookup[['AQA-001']], 'single')) stop('Socsi no debe entrar en seleccion ambigua.')
if (!identical(lookup[['AQA-002']], 'single')) stop('Huaycoloro no debe entrar en seleccion ambigua.')
if (!identical(lookup[['AQA-003']], 'single')) stop('Chira en Sullana no debe entrar en seleccion ambigua.')
if (!identical(lookup[['AQA-004']], 'ambiguous')) stop('Santa Clotilde debe ofrecer alternativas.')
if (!identical(lookup[['AQA-005']], 'ambiguous')) stop('Mazan DHN debe ofrecer alternativas y no fijar automaticamente la rama de 7540 km2.')

cat('\nAMBIGUITY REAL QA: PASS\n')
