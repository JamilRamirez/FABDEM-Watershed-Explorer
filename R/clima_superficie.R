# ============================================================
# R/clima_superficie.R
#
# FABDEM Watershed Explorer
# MODULO 04: CLIMA Y SUPERFICIE
# v1: Clima + Cobertura/uso + CUM + Zonas de vida
# ============================================================

clima_superficie <- local({

  ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".cs-wrap{padding:10px 12px 24px 12px;max-width:1550px;margin:auto;}"
          )
        )
      ),

      shiny::div(
        class = "cs-wrap",

        shiny::tabsetPanel(
          id = ns("submodulo"),
          type = "tabs",

          shiny::tabPanel(
            title = "Clima",
            value = "clima",
            clima$ui(
              ns("clima")
            )
          ),

          shiny::tabPanel(
            title = "Cobertura/uso",
            value = "cobertura",
            cobertura$ui(
              ns("cobertura")
            )
          ),

          shiny::tabPanel(
            title = "Capacidad de uso mayor",
            value = "cum",
            cum$ui(
              ns("cum")
            )
          ),

          shiny::tabPanel(
            title = "Zonas de vida",
            value = "vida",
            vida$ui(
              ns("vida")
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
        clima_result <- clima$server(
          "clima",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        cobertura_result <- cobertura$server(
          "cobertura",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        cum_result <- cum$server(
          "cum",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        vida_result <- vida$server(
          "vida",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        list(
          clima = clima_result,
          cobertura = cobertura_result,
          cum = cum_result,
          vida = vida_result
        )
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})
