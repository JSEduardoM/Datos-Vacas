# ============================================================
#  SAUSALITO · Dashboard UNALM
#  Replica del dashboard DairyPlan con datos extraídos de PDFs
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(scales)

# ── Colores corporativos ──────────────────────────────────
COLOR_VERDE       <- "#2e7d32"
COLOR_VERDE_CLARO <- "#43a047"
COLOR_NARANJA     <- "#e65100"
COLOR_NARANJA_CL  <- "#fb8c00"
COLOR_ROJO        <- "#c62828"
COLOR_AZUL        <- "#1565c0"
COLOR_GRIS        <- "#546e7a"
COLOR_FONDO       <- "#f5f6fa"

# ── Carga de datos ────────────────────────────────────────
# Los CSVs deben estar en la misma carpeta que app.R
load_data <- function() {
  base <- "."

  safe_read <- function(f) {
    p <- file.path(base, f)
    if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
    else data.frame()
  }

  list(
    prod        = safe_read("produccion_leche.csv"),       # vaca, grupo, status, dias_lactacion, kg_ultimo_ordeno,
                                                            # produccion_actual_24h, produccion_promedio,
                                                            # variacion_diaria, produccion_maxima,
                                                            # dia_max_produccion, total_lactacion
    gestantes   = safe_read("vacas_gestantes.csv"),        # vaca, lote, lactacion, fecha_parto, dias_lactacion,
                                                            # estatus, dia_gestacion, nro_inseminaciones,
                                                            # dias_abierta, prod_promedio
    para_secar  = safe_read("vacas_para_secar.csv"),       # vaca, grupo, status, dias_lactacion,
                                                            # dias_al_secado, dias_al_parto, prod_actual,
                                                            # fecha_secado_recom, fecha_estimada_parto
    prox_parto  = safe_read("vacas_proximas_parto.csv"),   # vaca, grupo, nombre, lactacion, dim,
                                                            # inicio_lactancia, fecha_insem, toro,
                                                            # dias_gestacion, dias_seca, fecha_parto_est,
                                                            # dias_faltantes, status
    inseminadas = safe_read("vacas_inseminadas.csv"),      # vaca, lote, lactacion, fecha_parto, estatus,
                                                            # fecha_inseminacion, toro, cant_inseminaciones,
                                                            # dias_inseminacion, dias_abiertos, prod_promedio
    secas       = safe_read("vacas_secas.csv"),            # vaca, grupo, status, fecha_secado, dias_secada,
                                                            # lactacion_nro, dias_lactacion, fecha_insem,
                                                            # dias_insem, toro, fecha_parto_est, dias_vacia
    problema    = safe_read("vacas_problema.csv"),         # vaca, status, grupo, dim
    mastitis    = safe_read("mastitis.csv"),               # vaca, edad, lactacion, dias_lact, leche_24h,
                                                            # dias_relativo, fecha_evento, tipo, tratamiento
    test_prenez = safe_read("test_prenez.csv"),            # vaca, grupo_status, grupo_nro, lactacion,
                                                            # dias_lact, cant_inseminaciones, fecha_insem,
                                                            # dias_insem
    vaquillas   = safe_read("vaquillas.csv"),              # vaca, grupo, fecha_nacimiento, edad_meses,
                                                            # status, madre, padre, peso_corporal
    vaq_parto   = safe_read("vaquillas_proximas_parto.csv")
  )
}

datos <- load_data()

# ── KPIs calculados ───────────────────────────────────────
# Producción promedio: columna "produccion_promedio" en produccion_leche.csv
prod_prom  <- if (nrow(datos$prod) > 0)
                round(mean(datos$prod$produccion_promedio, na.rm = TRUE), 1) else 0

vacas_prod     <- nrow(datos$prod)
vacas_gest     <- nrow(datos$gestantes)
vacas_secar    <- nrow(datos$para_secar)
vacas_secas    <- nrow(datos$secas)
vacas_insem    <- nrow(datos$inseminadas)
vacas_problema <- nrow(datos$problema)

# ── Vacas con historial de mastitis (columna real: "vaca") ──
vacas_mastitis <- if (nrow(datos$mastitis) > 0)
                    nrow(unique(datos$mastitis["vaca"])) else 0

# IEP: no disponible directamente; usamos dias_insem del test_prenez
iep_prom <- if (nrow(datos$test_prenez) > 0)
              round(mean(datos$test_prenez$dias_insem, na.rm = TRUE)) else 0

# Porcentaje gestantes sobre producción
pct_gest    <- if (vacas_prod > 0) round(vacas_gest / vacas_prod * 100, 1) else 0

# Tasa de concepción aproximada
tasa_concep <- if ((vacas_gest + vacas_insem) > 0)
                 round(vacas_gest / (vacas_gest + vacas_insem) * 100) else 0

# ── Composición del hato ──────────────────────────────────
hato_completo <- data.frame(
  categoria = c("En Producción", "Secas / Preparto", "Vaquillas Reemplazo"),
  n = c(vacas_prod, vacas_secas, nrow(datos$vaquillas))
) %>% mutate(pct = round(n / sum(n) * 100, 1))

# ── Tendencia producción por bins de DIM ─────────────────
tend_prod <- datos$prod %>%
  mutate(dim_bin = floor(dias_lactacion / 30) * 30) %>%
  group_by(dim_bin) %>%
  summarise(
    prom_hato = mean(produccion_actual_24h, na.rm = TRUE),
    prom_prod = mean(produccion_promedio,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  arrange(dim_bin) %>%
  mutate(meta = 40)

# ── Producción por grupo (lote) ───────────────────────────
prod_grupo <- datos$prod %>%
  group_by(grupo) %>%
  summarise(
    prod_prom_g = round(mean(produccion_promedio, na.rm = TRUE), 1),
    n_vacas     = n(),
    .groups     = "drop"
  ) %>%
  arrange(desc(prod_prom_g)) %>%
  mutate(grupo_label = paste0("Grupo ", grupo, " (n=", n_vacas, ")"))

# ── Alertas: vacas a secar ────────────────────────────────
alertas <- datos$para_secar %>%
  select(vaca, status, dias_lactacion, dias_al_secado, prod_actual,
         fecha_secado_recom, fecha_estimada_parto) %>%
  rename(
    "Vaca N°"          = vaca,
    "Estado"           = status,
    "Días Lact."       = dias_lactacion,
    "Días a Secar"     = dias_al_secado,
    "Prod. Prom.(kg)"  = prod_actual,
    "Fecha Secado"     = fecha_secado_recom,
    "Parto Estimado"   = fecha_estimada_parto
  ) %>%
  arrange(`Días a Secar`)

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  skin = "green",

  dashboardHeader(
    title = tags$span(
      style = "font-weight:700; font-size:15px; letter-spacing:.5px;",
      "SAUSALITO · Dashboard UNALM"
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 240,
    sidebarMenu(
      menuItem("Dashboard Principal",  tabName = "dashboard",    icon = icon("gauge-high")),
      menuItem("Inventario del Hato",  tabName = "inventario",   icon = icon("cow")),
      menuItem("Producción Diaria",    tabName = "produccion",   icon = icon("droplet")),
      menuItem("Control Reproductivo", tabName = "reproduccion", icon = icon("calendar-check")),
      menuItem("Mastitis",             tabName = "mastitis",     icon = icon("bacteria")),
      menuItem("Vacas Problema",       tabName = "problema",     icon = icon("triangle-exclamation"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML(paste0("
      body,.content-wrapper{ background:", COLOR_FONDO,"; font-family:'Segoe UI',Arial,sans-serif; }
      .main-sidebar{ background:#1b5e20 !important; }
      .sidebar-menu>li>a{ color:#c8e6c9 !important; font-size:13px; }
      .sidebar-menu>li.active>a,.sidebar-menu>li>a:hover{
        background:#2e7d32 !important; color:#fff !important; }
      .skin-green .main-header .navbar{ background:#2e7d32 !important; }
      .skin-green .main-header .logo  { background:#1b5e20 !important; }

      .kpi-card{
        border-radius:12px; padding:16px 18px; color:#fff;
        margin-bottom:14px; box-shadow:0 4px 12px rgba(0,0,0,.15);
        display:flex; align-items:center; gap:12px; }
      .kpi-icon{ font-size:34px; opacity:.85; }
      .kpi-value{ font-size:28px; font-weight:700; line-height:1; }
      .kpi-label{ font-size:11px; opacity:.9; margin-top:3px;
                  text-transform:uppercase; letter-spacing:.6px; }
      .kpi-verde  { background:linear-gradient(135deg,#2e7d32,#43a047); }
      .kpi-naranja{ background:linear-gradient(135deg,#e65100,#fb8c00); }
      .kpi-azul   { background:linear-gradient(135deg,#1565c0,#1976d2); }
      .kpi-rojo   { background:linear-gradient(135deg,#c62828,#e53935); }
      .kpi-gris   { background:linear-gradient(135deg,#37474f,#546e7a); }

      .chart-box{
        background:#fff; border-radius:12px; padding:16px;
        box-shadow:0 2px 8px rgba(0,0,0,.08); margin-bottom:14px; }
      .chart-title{ font-size:13px; color:#555; font-weight:600; margin-bottom:8px; }
      .section-header{
        background:linear-gradient(90deg,#2e7d32,#43a047);
        color:#fff; border-radius:8px; padding:8px 16px;
        font-size:13px; font-weight:600; margin-bottom:12px; }
      table.dataTable thead th{ background:#2e7d32 !important; color:#fff !important; }
      table.dataTable tbody tr:hover{ background:#e8f5e9 !important; }
    ")))),

    tabItems(

      # ═══════════════════════════════════════════════════
      # TAB 1 — DASHBOARD PRINCIPAL
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "dashboard",

        fluidRow(
          column(2, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("weight-hanging")),
            div(div(class="kpi-value", paste0(prod_prom," kg")),
                div(class="kpi-label","Prod. Prom./Vaca/Día")))),
          column(2, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("cow")),
            div(div(class="kpi-value", vacas_prod),
                div(class="kpi-label","Vacas en Producción")))),
          column(2, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("baby")),
            div(div(class="kpi-value", paste0(vacas_gest," (",pct_gest,"%)")),
                div(class="kpi-label","Vacas Gestantes")))),
          column(2, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("calendar-days")),
            div(div(class="kpi-value", paste0(iep_prom," días")),
                div(class="kpi-label","Días Últ. Insem. Prom.")))),
          column(2, div(class="kpi-card kpi-rojo",
            div(class="kpi-icon", icon("bacteria")),
            div(div(class="kpi-value", vacas_mastitis),
                div(class="kpi-label","Vacas c/ Mastitis")))),
          column(2, div(class="kpi-card kpi-rojo",
            div(class="kpi-icon", icon("percent")),
            div(div(class="kpi-value", paste0(tasa_concep,"%")),
                div(class="kpi-label","Tasa Concepción"))))
        ),

        fluidRow(
          column(8, div(class="chart-box",
            div(class="chart-title","📈 Tendencia Producción por Período de Lactancia (DIM)"),
            plotlyOutput("plot_tendencia", height="300px")
          )),
          column(4, div(class="chart-box",
            div(class="chart-title","🐄 Composición del Hato"),
            plotlyOutput("plot_estado_hato", height="300px")
          ))
        ),

        fluidRow(
          column(7, div(class="chart-box",
            div(class="section-header","⚠️ Alertas Reproductivas — Vacas a Secar"),
            DTOutput("tabla_alertas")
          )),
          column(5, div(class="chart-box",
            div(class="chart-title","🥛 Producción Promedio por Grupo (kg/día)"),
            plotlyOutput("plot_grupos", height="280px")
          ))
        )
      ),

      # ═══════════════════════════════════════════════════
      # TAB 2 — INVENTARIO DEL HATO
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "inventario",

        fluidRow(
          column(3, div(class="kpi-card kpi-verde",
            div(class="kpi-icon", icon("cow")),
            div(div(class="kpi-value", vacas_prod),
                div(class="kpi-label","Vacas en Producción")))),
          column(3, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("moon")),
            div(div(class="kpi-value", vacas_secas),
                div(class="kpi-label","Vacas Secas")))),
          column(3, div(class="kpi-card kpi-azul",
            div(class="kpi-icon", icon("seedling")),
            div(div(class="kpi-value", nrow(datos$vaquillas)),
                div(class="kpi-label","Vaquillas Reemplazo")))),
          column(3, div(class="kpi-card kpi-gris",
            div(class="kpi-icon", icon("list")),
            div(div(class="kpi-value", vacas_prod + vacas_secas + nrow(datos$vaquillas)),
                div(class="kpi-label","Total Animales"))))
        ),

        fluidRow(
          column(6, div(class="chart-box",
            div(class="chart-title","Distribución completa del hato"),
            plotlyOutput("plot_hato_completo", height="350px")
          )),
          column(6, div(class="chart-box",
            div(class="chart-title","Estado de las vacas en producción"),
            plotlyOutput("plot_status_prod", height="350px")
          ))
        ),

        fluidRow(
          column(12, div(class="chart-box",
            div(class="section-header","📋 Listado Producción"),
            DTOutput("tabla_prod_completa")
          ))
        )
      ),

      # ═══════════════════════════════════════════════════
      # TAB 3 — PRODUCCIÓN DIARIA
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "produccion",

        fluidRow(
          column(3, div(class="kpi-card kpi-verde",
            div(class="kpi-icon", icon("chart-line")),
            div(div(class="kpi-value", paste0(prod_prom," kg")),
                div(class="kpi-label","Prom. Producción")))),
          column(3, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("arrow-trend-up")),
            div(div(class="kpi-value",
                    paste0(round(max(datos$prod$produccion_promedio, na.rm=TRUE),1)," kg")),
                div(class="kpi-label","Máxima Actual")))),
          column(3, div(class="kpi-card kpi-azul",
            div(class="kpi-icon", icon("arrow-trend-down")),
            div(div(class="kpi-value",
                    paste0(round(min(datos$prod$produccion_promedio, na.rm=TRUE),1)," kg")),
                div(class="kpi-label","Mínima Actual")))),
          column(3, div(class="kpi-card kpi-gris",
            div(class="kpi-icon", icon("droplet")),
            div(div(class="kpi-value",
                    paste0(round(sum(datos$prod$produccion_promedio, na.rm=TRUE)/1000,1)," t/día")),
                div(class="kpi-label","Producción Total Estimada"))))
        ),

        fluidRow(
          column(12, div(class="chart-box",
            div(class="chart-title","Distribución de producción promedio por vaca (kg/día)"),
            plotlyOutput("hist_produccion", height="300px")
          ))
        ),
        fluidRow(
          column(12, div(class="chart-box",
            div(class="section-header","📋 Detalle Producción por Vaca"),
            DTOutput("tabla_produccion_detalle")
          ))
        )
      ),

      # ═══════════════════════════════════════════════════
      # TAB 4 — CONTROL REPRODUCTIVO
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "reproduccion",

        fluidRow(
          column(3, div(class="kpi-card kpi-verde",
            div(class="kpi-icon", icon("syringe")),
            div(div(class="kpi-value", vacas_insem),
                div(class="kpi-label","Vacas Inseminadas")))),
          column(3, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("baby")),
            div(div(class="kpi-value", vacas_gest),
                div(class="kpi-label","Vacas Gestantes")))),
          column(3, div(class="kpi-card kpi-azul",
            div(class="kpi-icon", icon("calendar-days")),
            div(div(class="kpi-value", iep_prom),
                div(class="kpi-label","Días Insem. Prom.")))),
          column(3, div(class="kpi-card kpi-rojo",
            div(class="kpi-icon", icon("triangle-exclamation")),
            div(div(class="kpi-value", vacas_problema),
                div(class="kpi-label","Vacas Problema (≥3 Insem.)"))))
        ),

        fluidRow(
          column(6, div(class="chart-box",
            div(class="chart-title","Distribución de días abiertos (vacas inseminadas)"),
            plotlyOutput("hist_dias_abiertos", height="280px")
          )),
          column(6, div(class="chart-box",
            div(class="chart-title","Distribución de inseminaciones por vaca"),
            plotlyOutput("plot_cant_insem", height="280px")
          ))
        ),

        fluidRow(
          column(12, div(class="chart-box",
            div(class="section-header","📋 Próximas al Parto (siguientes 21 días)"),
            DTOutput("tabla_prox_parto")
          ))
        ),
        fluidRow(
          column(12, div(class="chart-box",
            div(class="section-header","⚠️ Vacas Problema (≥3 inseminaciones)"),
            DTOutput("tabla_problema")
          ))
        )
      ),

      # ═══════════════════════════════════════════════════
      # TAB 5 — MASTITIS
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "mastitis",

        fluidRow(
          column(4, div(class="kpi-card kpi-rojo",
            div(class="kpi-icon", icon("bacteria")),
            div(div(class="kpi-value", vacas_mastitis),
                div(class="kpi-label","Vacas con Historial de Mastitis")))),
          column(4, div(class="kpi-card kpi-naranja",
            div(class="kpi-icon", icon("file-medical")),
            div(div(class="kpi-value", nrow(datos$mastitis)),
                div(class="kpi-label","Total Eventos de Mastitis")))),
          column(4, div(class="kpi-card kpi-azul",
            div(class="kpi-icon", icon("percent")),
            div(div(class="kpi-value",
                    paste0(if(vacas_prod>0) round(vacas_mastitis/vacas_prod*100,1) else 0, "%")),
                div(class="kpi-label","% Hato con Mastitis"))))
        ),

        fluidRow(
          column(6, div(class="chart-box",
            div(class="chart-title","Eventos de mastitis por número de lactación"),
            plotlyOutput("plot_mastitis_lac", height="300px")
          )),
          column(6, div(class="chart-box",
            div(class="chart-title","Tipo de diagnóstico de mastitis"),
            plotlyOutput("plot_mastitis_tipo", height="300px")
          ))
        ),

        fluidRow(
          column(12, div(class="chart-box",
            div(class="section-header","🦠 Listado Completo — Animales con Mastitis"),
            DTOutput("tabla_mastitis_full")
          ))
        )
      ),

      # ═══════════════════════════════════════════════════
      # TAB 6 — VACAS PROBLEMA
      # ═══════════════════════════════════════════════════
      tabItem(tabName = "problema",

        fluidRow(
          column(12,
            div(class="kpi-card kpi-rojo",
                style="max-width:320px; margin-bottom:20px;",
              div(class="kpi-icon", icon("triangle-exclamation")),
              div(div(class="kpi-value", vacas_problema),
                  div(class="kpi-label","Vacas con 3 o más inseminaciones"))))
        ),

        fluidRow(
          column(6, div(class="chart-box",
            div(class="chart-title","Distribución DIM en vacas problema"),
            plotlyOutput("plot_problema_dim", height="280px")
          )),
          column(6, div(class="chart-box",
            div(class="chart-title","Estado de vacas problema"),
            plotlyOutput("plot_problema_status", height="280px")
          ))
        ),

        fluidRow(
          column(12, div(class="chart-box",
            div(class="section-header","⚠️ Listado completo — Vacas Problema"),
            DTOutput("tabla_problema_full")
          ))
        )
      )

    ) # fin tabItems
  ) # fin dashboardBody
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {

  VERDE  <- "#2e7d32"
  NARAN  <- "#e65100"
  AZUL   <- "#1565c0"
  ROJO   <- "#c62828"
  GRIS   <- "#546e7a"
  PALETA <- c("#2e7d32","#fb8c00","#1565c0","#c62828","#546e7a","#7b1fa2","#00838f")

  lay <- function(fig, xtitle="", ytitle="")
    layout(fig,
           xaxis = list(title=xtitle, gridcolor="#eee"),
           yaxis = list(title=ytitle, gridcolor="#eee"),
           plot_bgcolor="#fff", paper_bgcolor="#fff",
           margin = list(l=40,r=10,t=10,b=40))

  # ── Tendencia producción ─────────────────────────────
  output$plot_tendencia <- renderPlotly({
    df <- tend_prod
    plot_ly(df, x = ~dim_bin) %>%
      add_lines(y = ~prom_hato, name = "Prod. Actual 24h",
                line = list(color=VERDE, width=3)) %>%
      add_lines(y = ~prom_prod, name = "Prod. Promedio",
                line = list(color=NARAN, width=2, dash="dot")) %>%
      add_lines(y = ~meta, name = "Meta 40 kg",
                line = list(color=ROJO, width=1.5, dash="dash")) %>%
      lay("Días en Leche (DIM)", "kg / vaca / día") %>%
      layout(legend=list(orientation="h", y=-0.25), hovermode="x unified")
  })

  # ── Composición del hato (pie) ───────────────────────
  output$plot_estado_hato <- renderPlotly({
    plot_ly(hato_completo, labels=~categoria, values=~n, type="pie",
            marker=list(colors=c(VERDE, NARAN, AZUL)),
            textinfo="label+percent", hoverinfo="label+value+percent") %>%
      layout(paper_bgcolor="#fff", showlegend=TRUE,
             margin=list(l=5,r=5,t=10,b=5))
  })

  # ── Tabla alertas ─────────────────────────────────────
  output$tabla_alertas <- renderDT({
    datatable(alertas, rownames=FALSE,
              options=list(pageLength=8, scrollX=TRUE, dom="ftp",
                           language=list(search="Buscar:"))) %>%
      formatStyle("Días a Secar",
        backgroundColor = styleInterval(c(-90,-30), c("#ffebee","#fff3e0","#e8f5e9")),
        fontWeight="bold")
  })

  # ── Producción por grupo ──────────────────────────────
  output$plot_grupos <- renderPlotly({
    df <- prod_grupo %>% head(15)   # top 15 grupos
    plot_ly(df, y=~grupo_label, x=~prod_prom_g,
            type="bar", orientation="h",
            marker=list(color=VERDE, line=list(color="#1b5e20",width=1))) %>%
      lay("kg/vaca/día promedio", "") %>%
      layout(yaxis=list(autorange="reversed"))
  })

  # ── Hato completo ─────────────────────────────────────
  output$plot_hato_completo <- renderPlotly({
    plot_ly(hato_completo, labels=~categoria, values=~n, type="pie",
            marker=list(colors=c(VERDE, NARAN, AZUL)),
            textinfo="label+value+percent") %>%
      layout(paper_bgcolor="#fff", margin=list(l=5,r=5,t=10,b=5))
  })

  # ── Status vacas en producción ────────────────────────
  output$plot_status_prod <- renderPlotly({
    df <- datos$prod %>%
      count(status) %>%
      arrange(desc(n))
    plot_ly(df, x=~status, y=~n, type="bar",
            marker=list(color=PALETA[seq_len(nrow(df))])) %>%
      lay("Estado", "N° vacas")
  })

  # ── Tabla producción completa ─────────────────────────
  output$tabla_prod_completa <- renderDT({
    datos$prod %>%
      select(vaca, grupo, status, dias_lactacion,
             produccion_actual_24h, produccion_promedio,
             produccion_maxima, total_lactacion) %>%
      rename("Vaca"="vaca","Grupo"="grupo","Estado"="status",
             "DIM"="dias_lactacion","Prod.24h(kg)"="produccion_actual_24h",
             "Prom.(kg)"="produccion_promedio","Máx.(kg)"="produccion_maxima",
             "Total Lact."="total_lactacion") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })

  # ── Histograma producción ─────────────────────────────
  output$hist_produccion <- renderPlotly({
    plot_ly(datos$prod, x=~produccion_promedio, type="histogram",
            nbinsx=20,
            marker=list(color=VERDE, line=list(color="#1b5e20",width=1))) %>%
      lay("Producción promedio (kg/día)", "N° vacas")
  })

  # ── Tabla producción detalle ──────────────────────────
  output$tabla_produccion_detalle <- renderDT({
    datos$prod %>%
      select(vaca, grupo, status, dias_lactacion, kg_ultimo_ordeno,
             produccion_actual_24h, produccion_promedio,
             variacion_diaria, produccion_maxima) %>%
      rename("Vaca"="vaca","Gp"="grupo","Estado"="status","DIM"="dias_lactacion",
             "Últ.Ordeño(kg)"="kg_ultimo_ordeno","Prod.24h(kg)"="produccion_actual_24h",
             "Prom.(kg)"="produccion_promedio","Variación"="variacion_diaria",
             "Máx.(kg)"="produccion_maxima") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE)) %>%
      formatStyle("Variación",
        color=styleInterval(0, c("red","green")), fontWeight="bold")
  })

  # ── Histograma días abiertos ──────────────────────────
  output$hist_dias_abiertos <- renderPlotly({
    plot_ly(datos$inseminadas, x=~dias_abiertos, type="histogram",
            nbinsx=20, marker=list(color=NARAN)) %>%
      lay("Días abiertos", "N° vacas")
  })

  # ── Distribución N° inseminaciones ───────────────────
  output$plot_cant_insem <- renderPlotly({
    df <- datos$inseminadas %>% count(cant_inseminaciones) %>%
      mutate(cant_inseminaciones = factor(cant_inseminaciones))
    plot_ly(df, x=~cant_inseminaciones, y=~n, type="bar",
            marker=list(color=AZUL)) %>%
      lay("N° inseminaciones", "N° vacas")
  })

  # ── Tabla próximas al parto ───────────────────────────
  output$tabla_prox_parto <- renderDT({
    datos$prox_parto %>%
      select(vaca, grupo, nombre, lactacion, dim,
             toro, fecha_parto_est, dias_faltantes, status) %>%
      rename("Vaca"="vaca","Grupo"="grupo","Nombre"="nombre",
             "Lact."="lactacion","DIM"="dim","Toro"="toro",
             "Parto Estimado"="fecha_parto_est",
             "Días Faltan"="dias_faltantes","Estado"="status") %>%
      arrange(`Días Faltan`) %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             language=list(search="Buscar:"))) %>%
      formatStyle("Días Faltan",
        backgroundColor=styleInterval(c(-7,7),c("#ffcdd2","#fff9c4","#c8e6c9")),
        fontWeight="bold")
  })

  # ── Tabla problema (reproductivo) ─────────────────────
  output$tabla_problema <- renderDT({
    datos$problema %>%
      rename("Vaca"="vaca","Estado"="status","Grupo"="grupo","DIM"="dim") %>%
      datatable(rownames=FALSE,
                options=list(pageLength=10, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })

  # ── Mastitis por lactación ────────────────────────────
  output$plot_mastitis_lac <- renderPlotly({
    df <- datos$mastitis %>% count(lactacion) %>%
      mutate(lactacion = factor(lactacion))
    plot_ly(df, x=~lactacion, y=~n, type="bar",
            marker=list(color=ROJO)) %>%
      lay("Número de Lactación", "N° eventos")
  })

  # ── Mastitis por tipo ─────────────────────────────────
  output$plot_mastitis_tipo <- renderPlotly({
    df <- datos$mastitis %>%
      filter(tipo != "" & !is.na(tipo)) %>%
      count(tipo) %>% arrange(desc(n)) %>% head(10)
    plot_ly(df, y=~reorder(tipo, n), x=~n, type="bar", orientation="h",
            marker=list(color=NARAN)) %>%
      lay("N° eventos", "Tipo diagnóstico") %>%
      layout(yaxis=list(autorange="reversed"))
  })

  # ── Tabla mastitis completa ───────────────────────────
  output$tabla_mastitis_full <- renderDT({
    datos$mastitis %>%
      rename("Vaca"="vaca","Edad"="edad","Lactación"="lactacion",
             "DIM"="dias_lact","Leche 24h(kg)"="leche_24h",
             "Días Relativo"="dias_relativo","Fecha"="fecha_evento",
             "Tipo"="tipo","Tratamiento"="tratamiento") %>%
      datatable(rownames=FALSE, filter="top",
                options=list(pageLength=20, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })

  # ── Vacas problema — DIM ──────────────────────────────
  output$plot_problema_dim <- renderPlotly({
    plot_ly(datos$problema, x=~dim, type="histogram",
            nbinsx=10, marker=list(color=ROJO)) %>%
      lay("Días en Leche (DIM)", "N° vacas")
  })

  # ── Vacas problema — status ───────────────────────────
  output$plot_problema_status <- renderPlotly({
    df <- datos$problema %>% count(status) %>% arrange(desc(n))
    plot_ly(df, x=~status, y=~n, type="bar",
            marker=list(color=PALETA[seq_len(nrow(df))])) %>%
      lay("Estado", "N° vacas")
  })

  # ── Tabla problema completa ───────────────────────────
  output$tabla_problema_full <- renderDT({
    datos$problema %>%
      rename("Vaca"="vaca","Estado"="status","Grupo"="grupo","DIM"="dim") %>%
      datatable(rownames=FALSE,
                options=list(pageLength=20, scrollX=TRUE,
                             language=list(search="Buscar:")))
  })
}

# ── Lanzar la app ─────────────────────────────────────────
shinyApp(ui, server)
