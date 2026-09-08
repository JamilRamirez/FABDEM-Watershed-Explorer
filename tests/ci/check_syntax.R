# FABDEM Watershed Explorer - validación de sintaxis R

files <- c(
  "app.R",
  sort(list.files("R", pattern = "\\.R$", full.names = TRUE)),
  sort(list.files("tests/ci", pattern = "\\.R$", full.names = TRUE))
)

if (length(files) < 2L) {
  stop("No se encontraron archivos R para validar.")
}

for (file in files) {
  tryCatch(
    {
      parse(file = file, encoding = "UTF-8")
      cat("OK parse: ", file, "\n", sep = "")
    },
    error = function(e) {
      stop(
        paste0(
          "Error de sintaxis en ",
          file,
          ": ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

cat("FABDEM R syntax CI: PASS\n")
