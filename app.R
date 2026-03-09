# =============================================================================
# STAN - STock ANalyser
# Application Shiny d'analyse des actions du CAC 40
# Université Clermont Auvergne - Master 2 MAS/STD - 2025-2026
# Auteur : Jean-Baptiste Knight Peterson
# =============================================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(quantmod)

# ========================= CONFIGURATION =====================================

DATA_DIR <- "data"
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR)

# Référentiel des tickers CAC 40 (Yahoo Finance, suffixe .PA)
CAC40_REF <- data.frame(
  ticker = c(
    "AI.PA", "AIR.PA", "ALO.PA", "MT.PA", "CS.PA", "BNP.PA", "EN.PA",
    "CAP.PA", "CA.PA", "ACA.PA", "BN.PA", "DSY.PA", "ENGI.PA", "EL.PA",
    "RMS.PA", "KER.PA", "LR.PA", "OR.PA", "MC.PA", "ML.PA", "ORA.PA",
    "RI.PA", "PUB.PA", "RNO.PA", "SAF.PA", "SGO.PA", "SAN.PA", "SU.PA",
    "GLE.PA", "STM.PA", "TEP.PA", "HO.PA", "TTE.PA", "VIE.PA", "DG.PA",
    "VIV.PA"
  ),
  name = c(
    "Air Liquide", "Airbus", "Alstom", "ArcelorMittal", "AXA",
    "BNP Paribas", "Bouygues", "Capgemini", "Carrefour",
    "Crédit Agricole", "Danone", "Dassault Systèmes", "Engie",
    "EssilorLuxottica", "Hermès", "Kering", "Legrand",
    "L'Oréal", "LVMH", "Michelin", "Orange", "Pernod Ricard",
    "Publicis", "Renault", "Safran", "Saint-Gobain", "Sanofi",
    "Schneider Electric", "Société Générale", "STMicroelectronics",
    "Teleperformance", "Thales", "TotalEnergies", "Veolia",
    "Vinci", "Vivendi"
  ),
  stringsAsFactors = FALSE
)

# ========================= FONCTIONS UTILITAIRES =============================

#' Lister les stocks disponibles dans le dossier data/
list_available_stocks <- function() {
  files <- list.files(DATA_DIR, pattern = "\\.csv$", full.names = FALSE)
  tickers <- gsub("\\.csv$", "", files)
  return(sort(tickers))
}

#' Lire un fichier CSV de stock (colonnes : Date, Close)
read_stock_csv <- function(ticker) {
  filepath <- file.path(DATA_DIR, paste0(ticker, ".csv"))
  if (!file.exists(filepath)) return(NULL)
  df <- tryCatch({
    d <- read.csv(filepath, stringsAsFactors = FALSE)
    d$Date <- as.Date(d$Date)
    d$Close <- as.numeric(d$Close)
    d <- d[!is.na(d$Date) & !is.na(d$Close), c("Date", "Close")]
    d[order(d$Date), ]
  }, error = function(e) NULL)
  return(df)
}

#' Sauvegarder un stock en CSV (format standardisé : Date, Close)
save_stock_csv <- function(df, ticker) {
  filepath <- file.path(DATA_DIR, paste0(ticker, ".csv"))
  write.csv(df[, c("Date", "Close")], filepath, row.names = FALSE)
}

#' Télécharger un stock depuis Yahoo Finance
#' @param ticker Ticker Yahoo Finance (ex: "BNP.PA")
#' @param from_date Date de début du téléchargement (format "YYYY-MM-DD")
#' @return data.frame avec colonnes Date et Close, ou NULL en cas d'erreur
download_stock_yahoo <- function(ticker, from_date = "2010-01-01") {
  data <- tryCatch(
    getSymbols(ticker, src = "yahoo", from = from_date, auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(data)) return(NULL)
  
  cols <- colnames(data)
  # Préférer le prix ajusté (Adjusted), sinon le Close
  adj_col <- grep("Adjusted", cols, value = TRUE)
  close_col <- grep("Close", cols, value = TRUE)
  use_col <- if (length(adj_col) > 0) adj_col[1] else if (length(close_col) > 0) close_col[1] else NULL
  if (is.null(use_col)) return(NULL)
  
  df <- data.frame(
    Date = index(data),
    Close = as.numeric(data[, use_col])
  )
  df <- df[!is.na(df$Close), ]
  return(df)
}

#' Parser un fichier format Boursorama (TSV avec colonnes date, ouv, haut, bas, clot, vol, devise)
#' @param filepath Chemin vers le fichier
#' @return data.frame avec colonnes Date et Close, ou NULL en cas d'erreur
parse_boursorama <- function(filepath) {
  df <- tryCatch(
    read.delim(filepath, sep = "\t", stringsAsFactors = FALSE,
               fill = TRUE, strip.white = TRUE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Trouver les colonnes pertinentes
  close_col <- grep("clot|close|cloture|adj", colnames(df), ignore.case = TRUE)
  date_col  <- grep("date", colnames(df), ignore.case = TRUE)
  
  if (length(close_col) == 0 || length(date_col) == 0) return(NULL)
  
  # Parser les dates (retirer la partie heure si présente)
  date_str <- gsub("\\s+\\d{2}:\\d{2}(:\\d{2})?.*$", "", trimws(df[[date_col[1]]]))
  dates <- as.Date(date_str, format = "%d/%m/%Y")
  if (all(is.na(dates))) dates <- as.Date(date_str, format = "%Y-%m-%d")
  if (all(is.na(dates))) dates <- as.Date(date_str, format = "%m/%d/%Y")
  
  # Parser les prix de clôture
  close_vals <- as.numeric(gsub(",", ".", as.character(df[[close_col[1]]])))
  
  result <- data.frame(Date = dates, Close = close_vals)
  result <- result[!is.na(result$Date) & !is.na(result$Close), ]
  result <- result[order(result$Date), ]
  return(result)
}

#' Import automatique des fichiers .txt Boursorama dans le répertoire courant
auto_import_txt_files <- function() {
  txt_files <- list.files(".", pattern = "\\.txt$", full.names = TRUE)
  imported <- character()
  for (f in txt_files) {
    df <- tryCatch(parse_boursorama(f), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      # Extraire le nom du ticker depuis le nom du fichier
      ticker <- gsub("_\\d{4}-\\d{2}-\\d{2}$", "", gsub("\\.txt$", "", basename(f)))
      ticker <- gsub("^\\./", "", ticker)
      save_stock_csv(df, ticker)
      imported <- c(imported, ticker)
      cat("Auto-import :", ticker, "avec", nrow(df), "observations\n")
    }
  }
  return(imported)
}

# Exécution de l'auto-import au démarrage
auto_import_txt_files()

# ========================= CALCULS FINANCIERS ================================

#' Volatilité : écart-type des accroissements journaliers (rendements log)
calc_volatility <- function(prices) {
  if (length(prices) < 2) return(NA)
  returns <- diff(log(prices))
  sd(returns, na.rm = TRUE)
}

#' CAGR : Compound Annual Growth Rate (taux de croissance annuel composé)
#' Formule : (Prix_final / Prix_initial)^(1/n_années) - 1
calc_cagr <- function(df) {
  if (nrow(df) < 2) return(NA)
  n_years <- as.numeric(difftime(max(df$Date), min(df$Date), units = "days")) / 365.25
  if (n_years <= 0) return(NA)
  (df$Close[nrow(df)] / df$Close[1])^(1 / n_years) - 1
}

#' Performance sur une période donnée en mois
calc_performance <- function(df, months) {
  if (nrow(df) < 2) return(NA)
  target_date <- max(df$Date) - months * 30.44
  idx <- which(df$Date >= target_date)
  if (length(idx) == 0) return(NA)
  first_price <- df$Close[min(idx)]
  last_price  <- df$Close[nrow(df)]
  (last_price / first_price - 1) * 100
}

#' Régression linéaire de log(prix) en fonction du temps
#' Retourne un liste contenant le modèle, les paramètres et les prévisions
calc_regression <- function(df) {
  if (nrow(df) < 10) return(NULL)
  
  df$t <- as.numeric(df$Date - min(df$Date))  # jours depuis le début
  df$log_price <- log(df$Close)
  model <- lm(log_price ~ t, data = df)
  
  beta      <- as.numeric(coef(model)["t"])
  intercept <- as.numeric(coef(model)["(Intercept)"])
  resid     <- residuals(model)
  sigma     <- sd(resid)
  fitted_v  <- fitted(model)
  
  # Position actuelle par rapport à la régression (en unités de sigma)
  last_residual  <- resid[length(resid)]
  position_sigma <- last_residual / sigma
  
  # Valeur théorique actuelle
  theoretical_current <- exp(tail(fitted_v, 1))
  
  # Prévisions : dans 1 an et 5 ans
  t_last <- tail(df$t, 1)
  theoretical_1y <- exp(intercept + beta * (t_last + 365.25))
  theoretical_5y <- exp(intercept + beta * (t_last + 5 * 365.25))
  
  list(
    model               = model,
    beta                = beta,
    intercept           = intercept,
    sigma               = sigma,
    position_sigma      = position_sigma,
    theoretical_current = as.numeric(theoretical_current),
    theoretical_1y      = as.numeric(theoretical_1y),
    theoretical_5y      = as.numeric(theoretical_5y),
    fitted              = as.numeric(fitted_v),
    residuals           = as.numeric(resid),
    t                   = df$t,
    dates               = df$Date
  )
}

#' Obtenir le nom lisible d'un ticker (depuis le référentiel CAC 40)
get_stock_name <- function(ticker) {
  idx <- which(CAC40_REF$ticker == ticker)
  if (length(idx) > 0) return(CAC40_REF$name[idx])
  return(ticker)
}

# ========================= INTERFACE UTILISATEUR =============================

ui <- dashboardPage(
  skin = "blue",
  
  # ---- Header ----
  dashboardHeader(
    title = span(icon("chart-line"), " STAN"),
    titleWidth = 250
  ),
  
  # ---- Sidebar (Panel 2 - Réglages) ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      
      # Réglages
      div(style = "padding: 15px 15px 5px 15px;",
        h4(icon("sliders-h"), " Réglages", 
           style = "color: #ecf0f1; margin-top: 0;"),
        selectInput("stock_ticker", 
                    label = "Action :",
                    choices = NULL, 
                    width = "100%"),
        sliderInput("start_year", 
                    label = "Début d'analyse :",
                    min = 2000, max = 2026, 
                    value = 2016, step = 1, sep = "",
                    width = "100%"),
        hr(style = "border-color: #34495e;")
      ),
      
      # Navigation
      menuItem("Indicateurs de base", tabName = "indicators", 
               icon = icon("tachometer-alt")),
      menuItem("Performance", tabName = "performance", 
               icon = icon("chart-line")),
      menuItem("Graphique", tabName = "chart", 
               icon = icon("chart-area")),
      menuItem("Gestion des données", tabName = "data_mgmt", 
               icon = icon("database"))
    )
  ),
  
  # ---- Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      
      # =========== Panel 1 : Indicateurs de base ============================
      tabItem(
        tabName = "indicators",
        h2("Indicateurs de base", style = "margin-bottom: 20px; color: #2c3e50;"),
        fluidRow(
          valueBoxOutput("last_price_box", width = 3),
          valueBoxOutput("last_date_box", width = 3),
          valueBoxOutput("volatility_box", width = 3),
          valueBoxOutput("cagr_box", width = 3)
        ),
        fluidRow(
          box(
            title = "À propos des indicateurs", 
            status = "info", solidHeader = FALSE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
            HTML("
              <ul>
                <li><strong>Dernier Prix</strong> : prix de clôture le plus récent disponible.</li>
                <li><strong>Dernière mise à jour</strong> : date correspondant au dernier prix.</li>
                <li><strong>Volatilité</strong> : écart-type des rendements logarithmiques journaliers 
                    (sur la période d'analyse sélectionnée).</li>
                <li><strong>CAGR</strong> (Compound Annual Growth Rate) : taux de croissance annuel composé. 
                    Formule : (Prix_final / Prix_initial)<sup>1/n</sup> - 1, 
                    où n est le nombre d'années de la période.</li>
              </ul>
            ")
          )
        )
      ),
      
      # =========== Panel 3 : Performance ====================================
      tabItem(
        tabName = "performance",
        h2("Performance & Régression", style = "margin-bottom: 20px; color: #2c3e50;"),
        fluidRow(
          box(
            title = span(icon("percent"), " Performance"), 
            status = "primary", solidHeader = TRUE, width = 5,
            tableOutput("perf_table"),
            helpText("Performance calculée sur le prix de clôture.")
          ),
          box(
            title = span(icon("square-root-alt"), " Régression linéaire (log prix)"), 
            status = "info", solidHeader = TRUE, width = 7,
            tableOutput("regression_table"),
            hr(),
            h4("Prévisions selon la droite de régression"),
            tableOutput("forecast_table"),
            helpText("Modèle : log(Prix) = α + β × t + ε, où t est le nombre de jours.")
          )
        )
      ),
      
      # =========== Panel 4 : Graphique ======================================
      tabItem(
        tabName = "chart",
        h2("Graphique interactif", style = "margin-bottom: 20px; color: #2c3e50;"),
        fluidRow(
          box(
            title = span(icon("chart-area"), " Prix en échelle logarithmique avec régression"),
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("stock_chart", height = "620px"),
            helpText("Le graphique montre le prix en échelle logarithmique, la droite de régression 
                      linéaire de log(prix), et les bandes de confiance à ±σ et ±2σ.")
          )
        )
      ),
      
      # =========== Panel 5 : Gestion de données =============================
      tabItem(
        tabName = "data_mgmt",
        h2("Gestion de données", style = "margin-bottom: 20px; color: #2c3e50;"),
        fluidRow(
          # Mise à jour depuis Yahoo Finance
          box(
            title = span(icon("sync"), " Mise à jour Yahoo Finance"), 
            status = "primary", solidHeader = TRUE, width = 4,
            p("Mettre à jour les données depuis Yahoo Finance."),
            actionButton("update_btn", "Mettre à jour le stock sélectionné", 
                         icon = icon("refresh"), class = "btn-block"),
            br(),
            actionButton("update_all_btn", "Mettre à jour tous les stocks", 
                         icon = icon("sync"), class = "btn-block"),
            br(),
            verbatimTextOutput("update_log")
          ),
          
          # Ajouter depuis Yahoo Finance
          box(
            title = span(icon("plus-circle"), " Ajouter depuis Yahoo Finance"), 
            status = "success", solidHeader = TRUE, width = 4,
            p("Entrez un ticker Yahoo Finance pour télécharger ses données."),
            textInput("new_ticker", "Ticker :", placeholder = "ex: BNP.PA"),
            textInput("new_ticker_from", "Date de début :", 
                      value = "2015-01-01", placeholder = "YYYY-MM-DD"),
            actionButton("add_yahoo_btn", "Télécharger et ajouter", 
                         icon = icon("download"), class = "btn-block"),
            br(),
            verbatimTextOutput("add_yahoo_log")
          ),
          
          # Importer depuis un fichier CSV
          box(
            title = span(icon("file-upload"), " Importer depuis un fichier"), 
            status = "warning", solidHeader = TRUE, width = 4,
            p("Importez un fichier CSV ou TXT contenant des données boursières."),
            fileInput("csv_file", "Fichier CSV / TXT :",
                      accept = c(".csv", ".txt", ".tsv"),
                      buttonLabel = "Parcourir",
                      placeholder = "Aucun fichier"),
            textInput("import_ticker", "Nom du ticker :", 
                      placeholder = "ex: BNPPARIBAS"),
            selectInput("csv_format", "Format du fichier :",
                        choices = c(
                          "Standard (Date, Close)" = "standard",
                          "Boursorama (TSV : date, ouv, haut, bas, clot, vol)" = "boursorama"
                        )),
            actionButton("import_csv_btn", "Importer", 
                         icon = icon("upload"), class = "btn-block"),
            br(),
            verbatimTextOutput("import_csv_log")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("table"), " Base de données locale"), 
            status = "info", solidHeader = TRUE, width = 12,
            DTOutput("data_overview_table"),
            helpText("Fichiers CSV stockés dans le dossier data/. Format : Date (YYYY-MM-DD), Close.")
          )
        )
      )
    )
  )
)

# ========================= SERVEUR ==========================================

server <- function(input, output, session) {
  
  # ---- Reactive : liste des stocks disponibles ----
  available_stocks <- reactiveVal(list_available_stocks())
  
  # ---- Observer : mise à jour du sélecteur de stocks ----
  observe({
    stocks <- available_stocks()
    if (length(stocks) == 0) {
      choices <- c("Aucun stock disponible" = "")
    } else {
      choices <- stocks
      names(choices) <- sapply(stocks, function(t) {
        nm <- get_stock_name(t)
        if (nm != t) paste0(nm, " (", t, ")") else t
      })
    }
    selected <- isolate(input$stock_ticker)
    if (is.null(selected) || !(selected %in% stocks)) {
      selected <- if (length(stocks) > 0) stocks[1] else NULL
    }
    updateSelectInput(session, "stock_ticker", choices = choices, selected = selected)
  })
  
  # ---- Notification si aucun stock ----
  observe({
    if (length(available_stocks()) == 0) {
      showNotification(
        "Aucune donnée disponible. Allez dans 'Gestion des données' pour ajouter des stocks.",
        type = "warning", duration = 10
      )
      updateTabItems(session, "tabs", "data_mgmt")
    }
  })
  
  # ---- Reactive : données brutes du stock sélectionné ----
  stock_data <- reactive({
    req(input$stock_ticker, input$stock_ticker != "")
    read_stock_csv(input$stock_ticker)
  })
  
  # ---- Reactive : données filtrées par la période d'analyse ----
  filtered_data <- reactive({
    df <- stock_data()
    req(df, nrow(df) > 0)
    start_date <- as.Date(paste0(input$start_year, "-01-01"))
    df_filt <- df[df$Date >= start_date, ]
    req(nrow(df_filt) > 0)
    df_filt
  })
  
  # ---- Reactive : résultat de la régression ----
  regression <- reactive({
    df <- filtered_data()
    req(df, nrow(df) >= 10)
    calc_regression(df)
  })
  
  # ===================== Panel 1 : Indicateurs de base =======================
  
  output$last_price_box <- renderValueBox({
    df <- stock_data()
    val <- if (!is.null(df) && nrow(df) > 0) {
      sprintf("%.2f €", tail(df$Close, 1))
    } else "N/A"
    valueBox(val, "Dernier Prix", icon = icon("euro-sign"), color = "green")
  })
  
  output$last_date_box <- renderValueBox({
    df <- stock_data()
    val <- if (!is.null(df) && nrow(df) > 0) {
      format(max(df$Date), "%d/%m/%Y")
    } else "N/A"
    valueBox(val, "Dernière mise à jour", icon = icon("calendar"), color = "blue")
  })
  
  output$volatility_box <- renderValueBox({
    df <- filtered_data()
    val <- if (!is.null(df) && nrow(df) > 1) {
      vol <- calc_volatility(df$Close)
      sprintf("%.4f (%.2f%%)", vol, vol * 100)
    } else "N/A"
    valueBox(val, "Volatilité journalière", icon = icon("chart-bar"), color = "yellow")
  })
  
  output$cagr_box <- renderValueBox({
    df <- filtered_data()
    val <- if (!is.null(df) && nrow(df) > 1) {
      cagr <- calc_cagr(df)
      color_prefix <- if (!is.na(cagr) && cagr >= 0) "+" else ""
      sprintf("%s%.2f%%", color_prefix, cagr * 100)
    } else "N/A"
    
    box_color <- "purple"
    if (!is.null(df) && nrow(df) > 1) {
      cagr <- calc_cagr(df)
      if (!is.na(cagr)) box_color <- if (cagr >= 0) "green" else "red"
    }
    valueBox(val, "CAGR", icon = icon("arrow-up"), color = box_color)
  })
  
  # ===================== Panel 3 : Performance ===============================
  
  output$perf_table <- renderTable({
    df <- filtered_data()
    req(df, nrow(df) > 1)
    
    periods <- c("1 Mois" = 1, "6 Mois" = 6, "1 An" = 12, "3 Ans" = 36, "5 Ans" = 60)
    perfs <- sapply(periods, function(m) calc_performance(df, m))
    
    data.frame(
      `Période` = names(periods),
      `Performance (%)` = ifelse(
        is.na(perfs), "N/A",
        ifelse(perfs >= 0, 
               paste0("+", sprintf("%.2f%%", perfs)),
               sprintf("%.2f%%", perfs))
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, align = "lr", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$regression_table <- renderTable({
    reg <- regression()
    req(reg)
    
    data.frame(
      Indicateur = c(
        "Beta (pente journalière)",
        "Beta annualisé",
        "Sigma (std des résidus)",
        "Valeur théorique actuelle",
        "Position σ (résidu / sigma)"
      ),
      Valeur = c(
        sprintf("%.6f", reg$beta),
        sprintf("%.4f (%.2f%% / an)", reg$beta * 365.25, (exp(reg$beta * 365.25) - 1) * 100),
        sprintf("%.4f", reg$sigma),
        sprintf("%.2f €", reg$theoretical_current),
        sprintf("%.2f σ", reg$position_sigma)
      ),
      stringsAsFactors = FALSE
    )
  }, align = "lr", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$forecast_table <- renderTable({
    reg <- regression()
    req(reg)
    
    data.frame(
      Horizon = c("Dans 1 an", "Dans 5 ans"),
      `Valeur théorique (€)` = c(
        sprintf("%.2f €", reg$theoretical_1y),
        sprintf("%.2f €", reg$theoretical_5y)
      ),
      `Croissance attendue` = c(
        sprintf("%+.2f%%", (reg$theoretical_1y / reg$theoretical_current - 1) * 100),
        sprintf("%+.2f%%", (reg$theoretical_5y / reg$theoretical_current - 1) * 100)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, align = "lrr", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ===================== Panel 4 : Graphique =================================
  
  output$stock_chart <- renderPlotly({
    df  <- filtered_data()
    reg <- regression()
    req(df, reg)
    
    # Construire le dataframe pour le graphique
    plot_df <- data.frame(
      Date   = df$Date,
      Close  = df$Close,
      Fitted = exp(reg$fitted),
      Upper1 = exp(reg$fitted + reg$sigma),
      Lower1 = exp(reg$fitted - reg$sigma),
      Upper2 = exp(reg$fitted + 2 * reg$sigma),
      Lower2 = exp(reg$fitted - 2 * reg$sigma)
    )
    
    ticker     <- input$stock_ticker
    stock_name <- get_stock_name(ticker)
    title_text <- if (stock_name != ticker) {
      paste0(stock_name, " (", ticker, ")")
    } else ticker
    
    # Graphique Plotly interactif
    p <- plot_ly(plot_df, x = ~Date) %>%
      # Bande ±2σ
      add_ribbons(
        ymin = ~Lower2, ymax = ~Upper2, 
        name = "±2σ",
        fillcolor = "rgba(52, 152, 219, 0.10)",
        line = list(color = "rgba(52, 152, 219, 0.20)", width = 0.5),
        hoverinfo = "skip"
      ) %>%
      # Bande ±σ
      add_ribbons(
        ymin = ~Lower1, ymax = ~Upper1, 
        name = "±σ",
        fillcolor = "rgba(52, 152, 219, 0.20)",
        line = list(color = "rgba(52, 152, 219, 0.35)", width = 0.5),
        hoverinfo = "skip"
      ) %>%
      # Droite de régression
      add_lines(
        y = ~Fitted, name = "Régression",
        line = list(color = "#e74c3c", dash = "dash", width = 2),
        hovertemplate = "Régression: %{y:.2f} €<extra></extra>"
      ) %>%
      # Prix réel
      add_lines(
        y = ~Close, name = "Prix de clôture",
        line = list(color = "#2c3e50", width = 1.5),
        hovertemplate = "%{x|%d/%m/%Y}<br>Prix: %{y:.2f} €<extra></extra>"
      ) %>%
      # Mise en forme
      layout(
        title = list(
          text = title_text,
          font = list(size = 18, color = "#2c3e50", family = "Roboto")
        ),
        yaxis = list(
          type = "log", 
          title = "Prix (€) — échelle logarithmique",
          gridcolor = "#ecf0f1",
          tickformat = ".0f"
        ),
        xaxis = list(
          title = "",
          gridcolor = "#ecf0f1",
          rangeslider = list(visible = FALSE)
        ),
        hovermode = "x unified",
        legend = list(
          orientation = "h", 
          x = 0.5, xanchor = "center",
          y = -0.08
        ),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "#ffffff",
        margin = list(t = 60, b = 60)
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d"),
        locale = "fr"
      )
    
    p
  })
  
  # ===================== Panel 5 : Gestion de données ========================
  
  # ---- Mise à jour du stock sélectionné ----
  log_update <- reactiveVal("")
  
  observeEvent(input$update_btn, {
    ticker <- input$stock_ticker
    req(ticker, ticker != "")
    
    log_update(paste0("Téléchargement de ", ticker, "...\n"))
    
    df <- download_stock_yahoo(ticker, from_date = "2010-01-01")
    if (!is.null(df) && nrow(df) > 0) {
      save_stock_csv(df, ticker)
      log_update(paste0("✓ ", ticker, " mis à jour : ", nrow(df), " observations\n",
                        "  Du ", format(min(df$Date), "%d/%m/%Y"), 
                        " au ", format(max(df$Date), "%d/%m/%Y")))
      available_stocks(list_available_stocks())
    } else {
      log_update(paste0("✗ Erreur lors du téléchargement de ", ticker,
                        "\n  Vérifiez votre connexion internet et le ticker."))
    }
  })
  
  # ---- Mise à jour de tous les stocks ----
  observeEvent(input$update_all_btn, {
    stocks <- available_stocks()
    req(length(stocks) > 0)
    
    logs <- character()
    withProgress(message = "Mise à jour en cours...", value = 0, {
      for (i in seq_along(stocks)) {
        ticker <- stocks[i]
        incProgress(1 / length(stocks), detail = ticker)
        
        df <- download_stock_yahoo(ticker, from_date = "2010-01-01")
        if (!is.null(df) && nrow(df) > 0) {
          save_stock_csv(df, ticker)
          logs <- c(logs, paste0("✓ ", ticker, " : ", nrow(df), " obs."))
        } else {
          logs <- c(logs, paste0("✗ ", ticker, " : erreur"))
        }
        Sys.sleep(0.5)
      }
    })
    log_update(paste(logs, collapse = "\n"))
    available_stocks(list_available_stocks())
  })
  
  output$update_log <- renderText({ log_update() })
  
  # ---- Ajouter depuis Yahoo Finance ----
  log_add_yahoo <- reactiveVal("")
  
  observeEvent(input$add_yahoo_btn, {
    ticker <- trimws(toupper(input$new_ticker))
    from_d <- trimws(input$new_ticker_from)
    req(ticker != "")
    
    log_add_yahoo(paste0("Téléchargement de ", ticker, "...\n"))
    
    df <- download_stock_yahoo(ticker, from_date = from_d)
    if (!is.null(df) && nrow(df) > 0) {
      save_stock_csv(df, ticker)
      available_stocks(list_available_stocks())
      log_add_yahoo(paste0("✓ ", ticker, " ajouté avec succès !\n",
                           "  ", nrow(df), " observations du ",
                           format(min(df$Date), "%d/%m/%Y"), " au ",
                           format(max(df$Date), "%d/%m/%Y")))
    } else {
      log_add_yahoo(paste0("✗ Impossible de télécharger ", ticker, 
                           "\n  Vérifiez le ticker (ex: BNP.PA pour BNP Paribas)."))
    }
  })
  
  output$add_yahoo_log <- renderText({ log_add_yahoo() })
  
  # ---- Importer depuis un fichier CSV/TXT ----
  log_import_csv <- reactiveVal("")
  
  observeEvent(input$import_csv_btn, {
    req(input$csv_file)
    ticker <- trimws(toupper(input$import_ticker))
    req(ticker != "")
    
    filepath <- input$csv_file$datapath
    
    df <- NULL
    if (input$csv_format == "boursorama") {
      df <- parse_boursorama(filepath)
    } else {
      df <- tryCatch({
        d <- read.csv(filepath, stringsAsFactors = FALSE)
        d$Date  <- as.Date(d$Date)
        d$Close <- as.numeric(d$Close)
        d <- d[!is.na(d$Date) & !is.na(d$Close), c("Date", "Close")]
        d[order(d$Date), ]
      }, error = function(e) NULL)
    }
    
    if (!is.null(df) && nrow(df) > 0) {
      save_stock_csv(df, ticker)
      available_stocks(list_available_stocks())
      log_import_csv(paste0("✓ ", ticker, " importé avec succès !\n",
                            "  ", nrow(df), " observations du ",
                            format(min(df$Date), "%d/%m/%Y"), " au ",
                            format(max(df$Date), "%d/%m/%Y")))
    } else {
      log_import_csv("✗ Erreur lors de l'importation.\n  Vérifiez le format du fichier.")
    }
  })
  
  output$import_csv_log <- renderText({ log_import_csv() })
  
  # ---- Tableau récapitulatif de la base de données ----
  output$data_overview_table <- renderDT({
    stocks <- available_stocks()
    if (length(stocks) == 0) {
      return(data.frame(Message = "Aucune donnée disponible."))
    }
    
    info_list <- lapply(stocks, function(t) {
      df <- read_stock_csv(t)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      data.frame(
        Ticker            = t,
        Nom               = get_stock_name(t),
        `Date début`      = format(min(df$Date), "%d/%m/%Y"),
        `Date fin`        = format(max(df$Date), "%d/%m/%Y"),
        `Nb observations` = nrow(df),
        `Dernier prix`    = sprintf("%.2f €", tail(df$Close, 1)),
        check.names       = FALSE,
        stringsAsFactors   = FALSE
      )
    })
    
    do.call(rbind, Filter(Negate(is.null), info_list))
  },
  options = list(
    pageLength = 20,
    searching  = TRUE,
    ordering   = TRUE,
    language   = list(
      url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json"
    )
  ),
  rownames = FALSE
  )
}

# ========================= LANCEMENT =========================================

shinyApp(ui = ui, server = server)
