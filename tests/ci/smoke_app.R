# FABDEM Watershed Explorer - smoke test de carga completa

options(
  shiny.launch.browser = FALSE,
  warn = 1
)

root <- normalizePath(
  file.path(dirname(sys.frame(1)$ofile %||% "tests/ci/smoke_app.R"), "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(root)

required_packages <- c(
  "shiny",
  "leaflet",
  "sf",
  "terra",
  "DT",
  "readxl"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Faltan paquetes para smoke test: ",
      paste(missing_packages, collapse = ", ")
    )
  )
}

app_env <- new.env(parent = globalenv())

result <- source(
  "app.R",
  local = app_env,
  echo = FALSE,
  print.eval = FALSE,
  encoding = "UTF-8",
  chdir = FALSE
)

if (!exists("ui", envir = app_env, inherits = FALSE)) {
  stop("app.R no creó el objeto ui.")
}

if (!exists("server", envir = app_env, inherits = FALSE)) {
  stop("app.R no creó el objeto server.")
}

if (!is.function(get("server", envir = app_env, inherits = FALSE))) {
  stop("El objeto server no es una función.")
}

if (!inherits(result$value, "shiny.appobj")) {
  stop("app.R no terminó construyendo un objeto shiny.appobj.")
}

modules <- c(
  "delimitacion",
  "morfometria",
  "geologia",
  "geomorfologia",
  "suelos",
  "hidrogeologia",
  "medio_fisico",
  "clima",
  "cobertura",
  "cum",
  "vida",
  "clima_superficie",
  "cuencas",
  "distritos",
  "poblados",
  "contexto_territorial"
)

for (module_name in modules) {
  if (!exists(module_name, envir = app_env, inherits = FALSE)) {
    stop("No se cargó el módulo: ", module_name)
  }

  module <- get(module_name, envir = app_env, inherits = FALSE)

  if (!is.list(module)) {
    stop("El módulo no es una lista: ", module_name)
  }

  if (!is.function(module$ui)) {
    stop("El módulo no expone ui(): ", module_name)
  }

  if (!is.function(module$server)) {
    stop("El módulo no expone server(): ", module_name)
  }
}

required_runtime_objects <- c(
  "RUNTIME_BASE_URL",
  "RUNTIME_ROOT",
  "CORE_DIR",
  "DEM_DIR",
  "ASSET_MANIFEST_CSV",
  "CATALOG_READY_FILE"
)

for (object_name in required_runtime_objects) {
  if (!exists(object_name, envir = app_env, inherits = FALSE)) {
    stop("config.R no dejó disponible: ", object_name)
  }
}

if (!file.exists(app_env$ASSET_MANIFEST_CSV)) {
  stop("ASSET_MANIFEST_CSV no existe después de cargar la app.")
}

if (!file.exists(app_env$CATALOG_READY_FILE)) {
  stop("CATALOG_READY_FILE no existe después de cargar la app.")
}

cat(
  "FABDEM Shiny smoke test: PASS\n",
  "Modules loaded: ",
  length(modules),
  "\n",
  sep = ""
)
