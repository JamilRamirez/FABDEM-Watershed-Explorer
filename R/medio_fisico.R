# ============================================================
# R/medio_fisico.R
#
# FABDEM Watershed Explorer
# MODULO 03: MEDIO FISICO
# v4: Geologia + Geomorfologia + Suelos + Hidrogeologia
# ============================================================

medio_fisico <- local({

  ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            ".mf-wrap{padding:10px 12px 24px 12px;max-width:1550px;margin:auto;}",
            ".mf-pending{padding:24px;color:#666;background:#fafafa;border:1px solid #e2e2e2;border-radius:7px;}"
          )
        )
      ),

      shiny::div(
        class = "mf-wrap",

        shiny::tabsetPanel(
          id = ns("submodulo"),
          type = "tabs",

          shiny::tabPanel(
            title = "Geología",
            value = "geologia",
            geologia$ui(
              ns("geologia")
            )
          ),

          shiny::tabPanel(
            title = "Geomorfología",
            value = "geomorfologia",
            geomorfologia$ui(
              ns("geomorfologia")
            )
          ),

          shiny::tabPanel(
            title = "Suelos",
            value = "suelos",
            suelos$ui(
              ns("suelos")
            )
          ),

          shiny::tabPanel(
            title = "Hidrogeología",
            value = "hidrogeologia",
            hidrogeologia$ui(
              ns("hidrogeologia")
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
        geologia_result <- geologia$server(
          "geologia",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        geomorfologia_result <- geomorfologia$server(
          "geomorfologia",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        suelos_result <- suelos$server(
          "suelos",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        hidrogeologia_result <- hidrogeologia$server(
          "hidrogeologia",
          basin = basin,
          basin_source = basin_source,
          basin_label = basin_label
        )

        list(
          geologia = geologia_result,
          geomorfologia = geomorfologia_result,
          suelos = suelos_result,
          hidrogeologia = hidrogeologia_result
        )
      }
    )
  }


  list(
    ui = ui,
    server = server
  )
})
