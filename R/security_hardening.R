# ============================================================
# SECURITY HARDENING — archivos espaciales cargados por el usuario
#
# Endurecimiento localizado, sin cambiar la arquitectura ni imponer
# límites de tamaño, número de geometrías o extensión de cuenca.
# ============================================================

security_sanitize_display_text <- function(x) {
  if (is.null(x)) return(x)

  x <- as.character(x)
  x <- gsub("[\r\n\t]", " ", x, perl = TRUE)
  x <- gsub("<", "‹", x, fixed = TRUE)
  x <- gsub(">", "›", x, fixed = TRUE)
  x
}

security_validate_archive_member_names <- function(member_names, archive_label) {
  if (!length(member_names)) return(invisible(TRUE))

  member_names <- as.character(member_names)
  normalized <- gsub("\\\\", "/", member_names)

  has_control <- grepl("[\r\n\t]", normalized, perl = TRUE)
  is_absolute <- startsWith(normalized, "/") |
    startsWith(normalized, "//") |
    grepl("^[A-Za-z]:($|/)", normalized, perl = TRUE)

  has_parent <- vapply(
    strsplit(normalized, "/", fixed = TRUE),
    function(parts) any(parts == ".."),
    logical(1)
  )

  bad <- is.na(normalized) | !nzchar(normalized) | has_control | is_absolute | has_parent

  if (any(bad)) {
    examples <- paste(utils::head(member_names[bad], 3L), collapse = ", ")
    stop(
      "Archivo comprimido rechazado por contener rutas internas inseguras (",
      archive_label,
      "): ",
      examples
    )
  }

  invisible(TRUE)
}

security_validate_archive <- function(path, archive_label = basename(path), depth = 0L) {
  # Protege únicamente contra anidamiento patológico de contenedores.
  # No limita el volumen ni la extensión del análisis geoespacial.
  if (depth > 12L) {
    stop("Archivo comprimido rechazado: anidamiento ZIP/KMZ excesivo en ", archive_label)
  }

  listing <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) {
      stop(
        "No se pudo inspeccionar el archivo comprimido ",
        archive_label,
        ": ",
        conditionMessage(e)
      )
    }
  )

  if (is.null(listing) || !nrow(listing) || !"Name" %in% names(listing)) {
    stop("El archivo comprimido está vacío o no tiene una estructura ZIP válida: ", archive_label)
  }

  member_names <- as.character(listing$Name)
  security_validate_archive_member_names(member_names, archive_label)

  nested_idx <- tolower(tools::file_ext(member_names)) %in% c("zip", "kmz")
  if (!any(nested_idx)) return(invisible(TRUE))

  inspect_dir <- tempfile("archive_security_")
  dir.create(inspect_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(inspect_dir, recursive = TRUE, force = TRUE), add = TRUE)

  nested_names <- member_names[nested_idx]

  tryCatch(
    utils::unzip(path, files = nested_names, exdir = inspect_dir),
    error = function(e) {
      stop(
        "No se pudo inspeccionar un archivo comprimido anidado en ",
        archive_label,
        ": ",
        conditionMessage(e)
      )
    }
  )

  for (nested_name in nested_names) {
    nested_path <- file.path(inspect_dir, gsub("/", .Platform$file.sep, nested_name, fixed = TRUE))
    if (!file.exists(nested_path)) {
      stop("No se pudo materializar el archivo comprimido anidado: ", nested_name)
    }

    security_validate_archive(
      nested_path,
      archive_label = paste0(archive_label, " > ", nested_name),
      depth = depth + 1L
    )
  }

  invisible(TRUE)
}

security_validate_uploaded_archives <- function(upload_df) {
  if (is.null(upload_df) || !nrow(upload_df)) return(invisible(TRUE))

  for (i in seq_len(nrow(upload_df))) {
    original_name <- as.character(upload_df$name[i])
    ext <- tolower(tools::file_ext(original_name))

    if (ext %in% c("zip", "kmz")) {
      security_validate_archive(
        path = upload_df$datapath[i],
        archive_label = basename(original_name)
      )
    }
  }

  invisible(TRUE)
}

install_security_hardening <- function(target_env = parent.frame()) {
  if (!exists("read_uploaded_basin_candidates", envir = target_env, inherits = FALSE)) {
    stop("No se encontró read_uploaded_basin_candidates() para aplicar el endurecimiento de seguridad.")
  }

  current <- get("read_uploaded_basin_candidates", envir = target_env, inherits = FALSE)
  if (isTRUE(attr(current, "security_hardened"))) return(invisible(TRUE))

  original <- current

  hardened <- function(upload_df, work_dir) {
    security_validate_uploaded_archives(upload_df)
    candidates <- original(upload_df = upload_df, work_dir = work_dir)

    if (length(candidates)) {
      candidates <- lapply(
        candidates,
        function(candidate) {
          for (field in intersect(c("label", "source_name", "layer_name"), names(candidate))) {
            candidate[[field]] <- security_sanitize_display_text(candidate[[field]])
          }
          candidate
        }
      )
    }

    candidates
  }

  attr(hardened, "security_hardened") <- TRUE
  assign("read_uploaded_basin_candidates", hardened, envir = target_env)
  invisible(TRUE)
}
