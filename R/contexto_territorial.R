# ============================================================
# R/contexto_territorial.R
#
# FABDEM Watershed Explorer
# MODULO 05: CONTEXTO TERRITORIAL
# v3: Cuencas + División distrital + Centros poblados
# ============================================================

contexto_territorial <- local({

  ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".ct-wrap{padding:10px 12px 24px 12px;max-width:1550px;margin:auto;}",
            ".ct-pending{padding:24px;color:#666;background:#fafafa;border:1px solid #e2e2e2;border-radius:7px;}"
          )
        )
      ),

      shiny::div(
        class = "ct-wrap",

        shiny::tabsetPanel(
          id = ns("submodulo"),
          type = "tabs",

          shiny::tabPanel(
            title = "Cuencas",
            value = "cuencas",
            cuencas$ui(
              ns("cuencas")
            )
          ),

          shiny::tabPanel(
            title = "División distrital",
            value = "distrital",
            distritos$ui(
              ns("distritos")
            )
          ),

          shiny::tabPanel(
            title = "Centros poblados",
            value = "poblados",
            poblados$ui(
              ns("poblados")
            )
          )
        )
      )
    )
  }


  server <- function(
      id,
      basin,
      basin_source,
      basin_label
  ) {
    shiny::moduleServer(
      id,
      function(input, output, session) {
        cuencas_result <- cuencas$server(
          "cuencas",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        distritos_result <- distritos$server(
          "distritos",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        poblados_result <- poblados$server(
          "poblados",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        list(
          cuencas = cuencas_result,
          distritos = distritos_result,
          poblados = poblados_result
        )
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})
