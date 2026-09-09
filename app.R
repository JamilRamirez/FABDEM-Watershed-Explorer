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

source(
  file.path("R", "snap_topologico.R"),
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
    ),
    tags$script(
      src = "morfometria_layout.js"
    ),
    tags$style(
      HTML(
        ".shiny-notification{pointer-events:auto!important;}\n.shiny-notification-close{pointer-events:auto!important;cursor:pointer!important;z-index:2;}\n"
      )
    ),
    tags$script(
      id = "fabdem-notification-autodismiss",
      HTML(
        "
        document.addEventListener('DOMContentLoaded', function () {
          const TTL = 10000;
          const timers = new WeakMap();

          const isActiveProgress = function (el) {
            const hasProgress = el.classList.contains('shiny-notification-progress') ||
              !!el.querySelector('.progress, .progress-bar');
            if (!hasProgress) return false;
            const txt = (el.textContent || '').replace(/\\s+/g, ' ');
            return !(/100%|Listo/i.test(txt));
          };

          const fadeAndRemove = function (el) {
            if (!document.body.contains(el)) return;
            el.style.transition = 'opacity 300ms ease, transform 300ms ease';
            el.style.opacity = '0';
            el.style.transform = 'translateX(12px)';
            window.setTimeout(function () {
              if (document.body.contains(el)) el.remove();
            }, 320);
          };

          const arm = function (el) {
            if (!(el instanceof HTMLElement) || !el.classList.contains('shiny-notification')) return;

            const oldTimer = timers.get(el);
            if (oldTimer) window.clearTimeout(oldTimer);

            if (isActiveProgress(el)) return;

            const timer = window.setTimeout(function () {
              fadeAndRemove(el);
            }, TTL);
            timers.set(el, timer);
          };

          const scan = function (node) {
            if (!(node instanceof HTMLElement)) return;
            if (node.classList.contains('shiny-notification')) arm(node);
            node.querySelectorAll('.shiny-notification').forEach(arm);
          };

          document.querySelectorAll('.shiny-notification').forEach(arm);

          const observer = new MutationObserver(function (mutations) {
            mutations.forEach(function (mutation) {
              if (mutation.type === 'childList') {
                mutation.addedNodes.forEach(scan);
                const parent = mutation.target instanceof HTMLElement
                  ? mutation.target.closest('.shiny-notification')
                  : null;
                if (parent) arm(parent);
              } else if (mutation.target instanceof HTMLElement) {
                const parent = mutation.target.closest('.shiny-notification');
                if (parent) arm(parent);
              }
            });
          });

          observer.observe(document.body, {
            childList: true,
            subtree: true,
            characterData: true
          });
        });
        "
      )
    ),
    tags$script(
      HTML(
        "
        document.addEventListener('DOMContentLoaded', function () {
          const ensureOsmAttribution = function () {
            document.querySelectorAll('.leaflet-control-attribution').forEach(function (el) {
              if (!/OpenStreetMap/i.test(el.textContent || '')) {
                const separator = el.textContent.trim() ? ' | ' : '';
                el.insertAdjacentHTML(
                  'beforeend',
                  separator + '<a href=\"https://www.openstreetmap.org/copyright\" target=\"_blank\" rel=\"noopener noreferrer\">© OpenStreetMap contributors</a>'
                );
              }
            });
          };

          ensureOsmAttribution();
          const observer = new MutationObserver(ensureOsmAttribution);
          observer.observe(document.body, { childList: true, subtree: true });
        });
        "
      )
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
          tags$div(
            class = "fabdem-about-note",
            tags$strong("FABDEM v1.2 y licencia. "),
            tags$span(
              "El modelo digital de elevación FABDEM v1.2 es publicado por la University of Bristol y está sujeto a la "
            ),
            tags$a(
              href = "https://data.bris.ac.uk/data/dataset/s5hqmjcdj8yo2ibzi9b4ew3sn",
              target = "_blank",
              rel = "noopener noreferrer",
              "Non-Commercial Government Licence for public sector information"
            ),
            tags$span(
              ". Los recortes, derivados y productos generados por esta aplicación no sustituyen ni modifican las condiciones de uso del dataset fuente."
            )
          ),
          tags$div(
            class = "fabdem-about-note",
            tags$strong("Privacidad de archivos cargados. "),
            "Las cuencas o archivos espaciales cargados por el usuario se utilizan para ejecutar el análisis de la sesión. La aplicación no requiere registro ni solicita datos personales. Evite cargar información confidencial o datos personales innecesarios."
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
