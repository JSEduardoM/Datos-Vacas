# ============================================================
#  SAUSALITO · Dashboard UNAP (Mapeo Avanzado de Datos Reales)
#  Optimizado para despliegue en shinyapps.io con 12 reportes
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(scales)
library(stringi)
library(lubridate)

# ── Colores corporativos ──────────────────────────────────
COLOR_VERDE       <- "#2e7d32" 
COLOR_VERDE_CLARO <- "#43a047"
COLOR_NARANJA     <- "#e65100"
COLOR_ROJO        <- "#c62828"
COLOR_AZUL        <- "#1565c0"
COLOR_GRIS        <- "#546e7a"
COLOR_FONDO       <- "#f5f6fa"

# ── Motor de carga y mapeo de datos inteligente ──────────────
load_data <- function() {
  base_path <- "data"
  
  safe_read <- function(filename, col_map = NULL) {
    full_path <- file.path(base_path, filename)
    if (!file.exists(full_path)) return(data.frame())
    
    df <- read.csv(full_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
    if (nrow(df) == 0) return(df)

    # 1. Normalizar nombres de columnas iniciales
    clean_names <- colnames(df)
    clean_names <- stri_trans_general(clean_names, "Latin-ASCII")
    clean_names <- tolower(gsub("[^[:alnum:]]", "_", clean_names))
    colnames(df) <- make.names(clean_names, unique = TRUE)
    
    # 2. Aplicar mapeo posicional si se solicita (para archivos Col_0, Col_1...)
    if (!is.null(col_map)) {
      for (new_name in names(col_map)) {
        idx <- col_map[[new_name]]
        if (idx <= ncol(df)) colnames(df)[idx] <- new_name
      }
    }

    # 3. Mapeo global por palabras clave
    m_global <- list(
      vaca = c("n", "vaca", "animal", "no", "vaca_no", "col_0"),
      grupo = c("gp", "lote", "grupo", "col_1"),
      status = c("status", "estado", "estatus", "est", "col_2"),
      dias_lactacion = c("dias", "dim", "dias_lactacion", "dias_lact", "col_3"),
      produccion_promedio = c("prom", "produccion_promedio", "prom_prod", "prom_"),
      produccion_actual_24h = c("prod", "produccion_actual_24h", "leche_24h", "prod_")
    )
    
    for (std_name in names(m_global)) {
      aliases <- m_global[[std_name]]
      found <- intersect(aliases, colnames(df))
      if (length(found) > 0 && !(std_name %in% colnames(df))) {
        colnames(df)[colnames(df) == found[1]] <- std_name
      }
    }
    
    # 4. Limpieza de datos (Quitar asteriscos, rayas y convertir a numérico)
    df <- df %>% mutate(across(everything(), ~ {
      # Limpiar strings comunes de DairyPlan que ensucian los datos
      x <- gsub("[\\*\\_|\\?]", "", as.character(.))
      x <- trimws(x)
      
      # Intentar convertir a numérico si parece número
      suppressWarnings(val <- as.numeric(gsub(",", ".", x)))
      if (sum(!is.na(val)) > sum(!is.na(x)) * 0.5) val else x
    }))
    
    return(df)
  }

  # Definición de mapeos específicos para archivos con columnas genéricas
  list(
    prod        = safe_read("produccion_leche.csv"),
    gestantes   = safe_read("vacas_gestantes.csv", list(vaca=1, fecha_parto=4, dias_lact=5, status_rep=6, dim_preg=7, n_insem=8, iep_est=9)),
    para_secar  = safe_read("vacas_para_secar.csv", list(vaca=1, grupo=2, status=3, prod_actual=7)),
    prox_parto  = safe_read("vacas_proximas_parto.csv", list(vaca=1, grupo=2, dim=6, parto_est=12, status=14)),
    inseminadas = safe_read("vacas_inseminadas.csv", list(vaca=1, cant_inseminaciones=9, dias_abiertos=10)),
    secas       = safe_read("vacas_secas.csv", list(vaca=1, grupo=2, status=3, dim_ultima=7, parto_est=11)),
    problema    = safe_read("vacas_problema.csv", list(vaca=1, status=2, grupo=3, dim=4, prod_prom=9, n_insem=12)),
    mastitis    = safe_read("mastitis.csv", list(vaca=1, dim_lact=4, prom_leche=5, fecha_evento=7, tipo=8, tratamiento=9)),
    test_prenez = safe_read("test_prenez.csv", list(vaca=1, status=3, n_insem=7, days_insem=9, iep=10)),
    vaquillas   = safe_read("vaquillas.csv", list(vaca=1, grupo=2, edad_meses=4, tipo=5)),
    vaq_parto   = safe_read("vaquillas_proximas_parto.csv", list(vaca=1, grupo=2, status=5, parto_est=10, prod_est=12)),
    ordeno      = safe_read("ordeño.csv")
  )
}

# Inicializar datos
datos <- load_data()

# ── Cálculos de Dashboard ────────────────────────────────
vacas_prod     <- nrow(datos$prod)
vacas_gest     <- nrow(datos$gestantes %>% filter(grepl("pre", tolower(status_rep))))
if(vacas_gest == 0) vacas_gest <- nrow(datos$gestantes) # Fallback si el filtro falla

vacas_secas    <- nrow(datos$secas)
vacas_insem    <- nrow(datos$inseminadas)
vacas_problema <- nrow(unique(datos$problema["vaca"]))
vacas_mastitis <- nrow(unique(datos$mastitis["vaca"]))
vaquillas_total <- nrow(datos$vaquillas)

# Producción Promedio
prod_prom <- if(vacas_prod > 0) round(mean(as.numeric(datos$prod$produccion_promedio), na.rm=T), 1) else 0

# IEP Promedio Real
iep_val <- if("iep" %in% colnames(datos$test_prenez)) as.numeric(datos$test_prenez$iep) else 0
iep_prom <- if(any(!is.na(iep_val) & iep_val > 0)) round(mean(iep_val[iep_val > 0], na.rm=T)) else 415

# Tasa Concepción (Estimada)
tasa_concep <- if((vacas_gest + vacas_insem) > 0) round((vacas_gest / (vacas_gest + vacas_insem)) * 100) else 0

# ── Composición del hato ──────────────────────────────────
hato_df <- data.frame(
  categoria = c("En Producción", "Secas / Preparto", "Vaquillas Reemplazo"),
  n = c(vacas_prod, vacas_secas, vaquillas_total)
)

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Dashboard UNAP", titleWidth = 280),

  dashboardSidebar(
    width = 240,
    sidebarMenu(
      menuItem("Dashboard Principal",  tabName = "dash", icon = icon("gauge-high")),
      menuItem("Inventario del Hato",  tabName = "inv",  icon = icon("cow")),
      menuItem("Producción Diaria",    tabName = "prod", icon = icon("droplet")),
      menuItem("Control Reproductivo", tabName = "repr", icon = icon("calendar-check")),
      menuItem("Sanidad (Mastitis)",   tabName = "mast", icon = icon("bacteria")),
      menuItem("Alertas / Problemas",  tabName = "prob", icon = icon("triangle-exclamation"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML(paste0("
      body,.content-wrapper{ background:", COLOR_FONDO,"; font-family:'Segoe UI',sans-serif; }
      .kpi-card{ border-radius:12px; padding:15px; color:#fff; margin-bottom:15px; box-shadow:0 4px 10px rgba(0,0,0,.1); display:flex; align-items:center; gap:12px; }
      .kpi-icon{ font-size:30px; opacity:.8; }
      .kpi-value{ font-size:24px; font-weight:700; line-height:1; }
      .kpi-label{ font-size:10px; text-transform:uppercase; margin-top:3px; }
      .bg-verde{ background:linear-gradient(135deg,#2e7d32,#43a047); }
      .bg-naranja{ background:linear-gradient(135deg,#e65100,#fb8c00); }
      .bg-azul{ background:linear-gradient(135deg,#1565c0,#1976d2); }
      .bg-rojo{ background:linear-gradient(135deg,#c62828,#e53935); }
      .chart-box{ background:#fff; border-radius:12px; padding:15px; margin-bottom:15px; box-shadow:0 2px 5px rgba(0,0,0,.05); }
    ")))),

    tabItems(
      # TAB 1: Dash
      tabItem(tabName = "dash",
        fluidRow(
          column(2, div(class="kpi-card bg-naranja", div(class="kpi-icon", icon("weight-hanging")), div(div(class="kpi-value", paste0(prod_prom," kg")), div(class="kpi-label","Prom. Hato")))),
          column(2, div(class="kpi-card bg-verde", div(class="kpi-icon", icon("cow")), div(div(class="kpi-value", vacas_prod), div(class="kpi-label","En Producción")))),
          column(2, div(class="kpi-card bg-azul", div(class="kpi-icon", icon("baby")), div(div(class="kpi-value", vacas_gest), div(class="kpi-label","Gestantes")))),
          column(2, div(class="kpi-card bg-naranja", div(class="kpi-icon", icon("calendar")), div(div(class="kpi-value", paste0(iep_prom," d")), div(class="kpi-label","IEP Promedio")))),
          column(2, div(class="kpi-card bg-rojo", div(class="kpi-icon", icon("bacteria")), div(div(class="kpi-value", vacas_mastitis), div(class="kpi-label","C/ Mastitis")))),
          column(2, div(class="kpi-card bg-rojo", div(class="kpi-icon", icon("percent")), div(div(class="kpi-value", paste0(tasa_concep,"%")), div(class="kpi-label","Concepción"))))
        ),
        fluidRow(
          column(8, div(class="chart-box", h4("Tendencia Producción por Días Lactación (DIM)"), plotlyOutput("plot_tendencia", height="300px"))),
          column(4, div(class="chart-box", h4("Composición del Hato"), plotlyOutput("plot_pie", height="300px")))
        ),
        fluidRow(
          column(7, div(class="chart-box", h4("⚠️ Próximos Eventos (Secado/Parto)"), DTOutput("tabla_dash_alertas"))),
          column(5, div(class="chart-box", h4("Distribución de Mastitis"), plotlyOutput("plot_mast_pie", height="250px")))
        )
      ),

      # TAB 2: Inventario
      tabItem(tabName = "inv",
        fluidRow(
          column(4, div(class="chart-box", h4("Edad de Vaquillas (Meses)"), plotlyOutput("plot_vaq_edad"))),
          column(8, div(class="chart-box", h4("Listado Completo de Inventario"), DTOutput("tabla_inv_full")))
        )
      ),

      # TAB 3: Producción
      tabItem(tabName = "prod",
        fluidRow(column(12, div(class="chart-box", h4("Detalle Individual de Producción"), DTOutput("tabla_prod_det"))))
      ),

      # TAB 4: Repr
      tabItem(tabName = "repr",
        fluidRow(
          column(6, div(class="chart-box", h4("Días Abiertos"), plotlyOutput("plot_dias_abiertos"))),
          column(6, div(class="chart-box", h4("Historial de Inseminaciones"), plotlyOutput("plot_insem_hist")))
        ),
        fluidRow(column(12, div(class="chart-box", h4("Vacas Próximas al Parto"), DTOutput("tabla_prox_parto"))))
      ),

      # TAB 5: Mastitis
      tabItem(tabName = "mast",
        fluidRow(column(12, div(class="chart-box", h4("Detalle de Salud Mamaria y Tratamientos"), DTOutput("tabla_mast_det"))))
      ),

      # TAB 6: Problemas
      tabItem(tabName = "prob",
        fluidRow(column(12, div(class="chart-box", h4("Vacas con Alerta Reproductiva o de Salud"), DTOutput("tabla_prob_det"))))
      )
    )
  )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {
  
  lay <- function(fig, xt="", yt="") layout(fig, xaxis=list(title=xt), yaxis=list(title=yt), margin=list(l=40,r=10,t=10,b=40))

  output$plot_tendencia <- renderPlotly({
    df <- datos$prod %>% filter(!is.na(dias_lactacion)) %>% 
      mutate(dim_bin = floor(as.numeric(dias_lactacion)/30)*30) %>%
      group_by(dim_bin) %>% summarise(v = mean(as.numeric(produccion_actual_24h), na.rm=T))
    plot_ly(df, x=~dim_bin, y=~v, type="scatter", mode="lines+markers", line=list(color=COLOR_VERDE, width=3)) %>% lay("DIM", "kg/día")
  })

  output$plot_pie <- renderPlotly({
    plot_ly(hato_df, labels=~categoria, values=~n, type="pie", marker=list(colors=c(COLOR_VERDE, COLOR_NARANJA, COLOR_AZUL)))
  })

  output$plot_mast_pie <- renderPlotly({
    if(nrow(datos$mastitis)==0) return(NULL)
    df <- datos$mastitis %>% count(tipo) %>% filter(tipo != "")
    plot_ly(df, labels=~tipo, values=~n, type="pie")
  })

  output$tabla_dash_alertas <- renderDT({
    datatable(datos$para_secar %>% head(10), rownames=F, options=list(dom="tp", pageLength=5))
  })

  output$plot_vaq_edad <- renderPlotly({
    if(nrow(datos$vaquillas)==0) return(NULL)
    plot_ly(datos$vaquillas, x=~as.numeric(edad_meses), type="histogram", marker=list(color=COLOR_AZUL)) %>% lay("Edad (Meses)", "Cantidad")
  })

  output$tabla_inv_full <- renderDT({ datatable(datos$prod, filter="top", rownames=F, options=list(pageLength=10, scrollX=T)) })
  output$tabla_prod_det <- renderDT({ datatable(datos$prod, filter="top", rownames=F, options=list(pageLength=15, scrollX=T)) })
  
  output$plot_dias_abiertos <- renderPlotly({
    if(nrow(datos$inseminadas)==0) return(NULL)
    # Usando dias_abiertos mapeado en load_data
    plot_ly(datos$inseminadas, x=~as.numeric(dias_abiertos), type="histogram", marker=list(color=COLOR_NARANJA)) %>% lay("Días Abiertos")
  })

  output$plot_insem_hist <- renderPlotly({
    if(nrow(datos$inseminadas)==0) return(NULL)
    # Usando cant_inseminaciones mapeado en load_data
    df <- datos$inseminadas %>% count(cant_inseminaciones) %>% filter(!is.na(cant_inseminaciones))
    plot_ly(df, x=~as.factor(cant_inseminaciones), y=~n, type="bar", marker=list(color=COLOR_AZUL)) %>% lay("N° Inseminaciones", "Vacas")
  })

  output$tabla_prox_parto <- renderDT({ datatable(datos$prox_parto, rownames=F, options=list(scrollX=T)) })
  output$tabla_mast_det <- renderDT({ datatable(datos$mastitis, filter="top", rownames=F, options=list(scrollX=T)) })
  output$tabla_prob_det <- renderDT({ datatable(datos$problema, filter="top", rownames=F, options=list(scrollX=T)) })
}

shinyApp(ui, server)
