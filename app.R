# ============================================================
# app.R
#
# FABDEM Watershed Explorer
# Arquitectura modular local
# v26: Contexto territorial completo: Cuencas + Distritos + Centros poblados
# ============================================================


pkgs <- c(
  "shiny",
  "leaflet",
  "sf",
  "terra",
  "DT",
  "readxl"
)


faltan <- pkgs[
  !vapply(
    pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(faltan) > 0L) {
  stop(
    paste0(
      "Faltan paquetes: ",
      paste(
        faltan,
        collapse = ", "
      )
    )
  )
}


library(shiny)
library(leaflet)
library(sf)
library(terra)
library(DT)
library(readxl)


sf::sf_use_s2(
  FALSE
)


options(
  shiny.launch.browser = TRUE,
  shiny.maxRequestSize = 100 * 1024^2
)


# ============================================================
# CARGA CENTRAL EXPLICITA
# ============================================================
#
# app.R controla directamente el orden de composicion.
# Los source() se ejecutan EN EL ENTORNO DE app.R.
#
# No se encapsulan dentro de una funcion auxiliar, porque
# source(..., local = TRUE) dentro de una funcion carga los
# objetos en el frame temporal de esa funcion.
# ============================================================


# 1. Configuracion siempre primero
source(
  file.path("R", "config.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)


# 2. Soporte general
source(
  file.path("R", "helpers.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)


# 3. Modulos base
source(
  file.path("R", "delimitacion.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "morfometria.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)


# 4. Medio fisico
source(
  file.path("R", "geologia.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "geomorfologia.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "suelos.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "hidrogeologia.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "medio_fisico.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)


# 5. Clima y superficie
source(
  file.path("R", "clima.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "cobertura.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "cum.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "vida.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "clima_superficie.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)


# 6. Contexto territorial
source(
  file.path("R", "cuencas.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "distritos.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "poblados.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)

source(
  file.path("R", "contexto_territorial.R"),
  local = TRUE,
  encoding = "UTF-8",
  chdir = FALSE
)


# Confirmacion directa, sin validadores indirectos.
# Si config.R no definio LAYERS_DIR, R fallara aqui con el error real.
message(
  "Configuracion cargada | LAYERS_DIR: ",
  LAYERS_DIR
)


# ============================================================
# UI
# ============================================================

ui <- tagList(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "fabdem.css"
    )
  ),

  navbarPage(
  title = div(
    class = "fabdem-brand",
    tags$img(
      src = "fabdem_logo.png",
      class = "fabdem-brand-logo",
      alt = ""
    ),
    span(
      class = "fabdem-brand-copy",
      span(
        class = "fabdem-brand-line",
        span(
          class = "fabdem-brand-title",
          "FABDEM Watershed Explorer"
        ),
        span(
          class = "fabdem-brand-author",
          "· by Jamil Ramirez"
        )
      ),
      tags$small(
        "Delimitación y caracterización de cuencas"
      )
    )
  ),
  id = "modulo_activo",
  windowTitle = "FABDEM Watershed Explorer",
  inverse = TRUE,
  collapsible = TRUE,


  tabPanel(
    title = "Delimitación",
    value = "delimitacion",
    delimitacion$ui(
      "delimitacion"
    )
  ),


  tabPanel(
    title = "Morfometría",
    value = "morfometria",
    morfometria$ui(
      "morfometria"
    )
  ),


  tabPanel(
    title = "Medio físico",
    value = "medio_fisico",
    medio_fisico$ui(
      "medio_fisico"
    )
  ),


  tabPanel(
    title = "Clima y superficie",
    value = "clima_superficie",
    clima_superficie$ui(
      "clima_superficie"
    )
  ),


  tabPanel(
    title = "Contexto territorial",
    value = "contexto_territorial",
    contexto_territorial$ui(
      "contexto_territorial"
    )
  )
  ),

  tags$button(
    type = "button",
    class = "btn fabdem-about-trigger",
    `data-toggle` = "modal",
    `data-target` = "#fabdem-about-modal",
    `aria-label` = "Acerca de FABDEM Watershed Explorer",
    tags$span(
      class = "glyphicon glyphicon-info-sign",
      `aria-hidden` = "true"
    ),
    tags$span(
      class = "fabdem-about-trigger-label",
      "Acerca de"
    )
  ),

  tags$div(
    id = "fabdem-about-modal",
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    `aria-labelledby` = "fabdem-about-title",
    tags$div(
      class = "modal-dialog fabdem-about-dialog",
      role = "document",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header fabdem-about-header",
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Cerrar",
            tags$span(
              `aria-hidden` = "true",
              HTML("&times;")
            )
          ),
          tags$div(
            class = "fabdem-about-kicker",
            "ACERCA DE"
          ),
          tags$h3(
            id = "fabdem-about-title",
            class = "modal-title",
            "FABDEM Watershed Explorer"
          ),
          tags$p(
            "Delimitación y caracterización integral de cuencas hidrográficas"
          )
        ),
        tags$div(
          class = "modal-body fabdem-about-body",
          tags$p(
            class = "fabdem-about-intro",
            paste0(
              "Esta aplicación permite delimitar una cuenca desde un punto de salida ",
              "o cargar una cuenca propia, y organiza su caracterización en cinco ",
              "módulos conectados."
            )
          ),
          tags$div(
            class = "fabdem-about-grid",
            tags$section(
              tags$h4("Delimitación"),
              tags$p(
                "Ubica el punto por mapa, coordenadas geográficas, UTM o grados-minutos-segundos; delimita la cuenca con el contexto D8 de FABDEM."
              )
            ),
            tags$section(
              tags$h4("Morfometría"),
              tags$p(
                "Calcula geometría, forma, relieve, recorrido hidráulico, cauce principal, red de drenaje, orden Strahler y tiempos de concentración."
              )
            ),
            tags$section(
              tags$h4("Medio físico"),
              tags$p(
                "Resume geología, geomorfología, suelos e hidrogeología dentro de la cuenca activa."
              )
            ),
            tags$section(
              tags$h4("Clima y superficie"),
              tags$p(
                "Integra clima, cobertura y uso del suelo, capacidad de uso mayor y zonas de vida."
              )
            ),
            tags$section(
              tags$h4("Contexto territorial"),
              tags$p(
                "Relaciona la cuenca con unidades hidrográficas, red de ríos, división distrital y centros poblados."
              )
            ),
            tags$section(
              tags$h4("Resultados"),
              tags$p(
                "Genera mapas, tablas, archivos geoespaciales y descargas gráficas para documentar el análisis."
              )
            )
          ),
          tags$div(
            class = "fabdem-about-note",
            tags$strong("Uso recomendado. "),
            "Herramienta de apoyo para análisis y caracterización; los resultados deben contrastarse con fuentes oficiales y verificación de campo cuando corresponda."
          ),
          layer_source_ui(
            title = "Fuentes de datos de la aplicación",
            show_module = TRUE
          )
        ),
        tags$div(
          class = "modal-footer fabdem-about-footer",
          tags$span("Desarrollado por Jamil Ramirez"),
          tags$button(
            type = "button",
            class = "btn btn-primary",
            `data-dismiss` = "modal",
            "Cerrar"
          )
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {

  delimitacion_result <- delimitacion$server(
    "delimitacion"
  )


  morfometria_result <- morfometria$server(
    "morfometria",
    basin = delimitacion_result$basin,
    outlet = delimitacion_result$outlet,
    folder = delimitacion_result$folder,
    block_id = delimitacion_result$block_id,
    hydro_context = delimitacion_result$hydro_context,
    basin_source = delimitacion_result$basin_source,
    basin_label = delimitacion_result$basin_label
  )


  medio_fisico_result <- medio_fisico$server(
    "medio_fisico",
    basin = delimitacion_result$basin,
    basin_source = delimitacion_result$basin_source,
    basin_label = delimitacion_result$basin_label
  )


  clima_superficie_result <- clima_superficie$server(
    "clima_superficie",
    basin = delimitacion_result$basin,
    basin_source = delimitacion_result$basin_source,
    basin_label = delimitacion_result$basin_label
  )


  contexto_territorial_result <- contexto_territorial$server(
    "contexto_territorial",
    basin = delimitacion_result$basin,
    basin_source = delimitacion_result$basin_source,
    basin_label = delimitacion_result$basin_label
  )
}


shinyApp(
  ui = ui,
  server = server
)
