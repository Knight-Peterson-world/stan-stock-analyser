# =============================================================================
# STAN - Script de téléchargement initial des données CAC 40
# Exécuter ce script UNE FOIS pour peupler la base de données locale.
#
# Usage : Rscript download_data.R
#         ou dans RStudio : source("download_data.R")
# =============================================================================

cat("══════════════════════════════════════════════════════════════\n")
cat("  STAN - Téléchargement des données CAC 40\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# --- Installation des packages si nécessaire ---
required_packages <- c("quantmod")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installation du package", pkg, "...\n")
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}
library(quantmod)

# --- Créer le dossier data ---
if (!dir.exists("data")) {
  dir.create("data")
  cat("Dossier data/ créé.\n\n")
}

# --- Liste des tickers CAC 40 (Yahoo Finance) ---
tickers <- c(
  "AI.PA",    # Air Liquide
  "AIR.PA",   # Airbus
  "ALO.PA",   # Alstom
  "MT.PA",    # ArcelorMittal
  "CS.PA",    # AXA
  "BNP.PA",   # BNP Paribas
  "EN.PA",    # Bouygues
  "CAP.PA",   # Capgemini
  "CA.PA",    # Carrefour
  "ACA.PA",   # Crédit Agricole
  "BN.PA",    # Danone
  "DSY.PA",   # Dassault Systèmes
  "ENGI.PA",  # Engie
  "EL.PA",    # EssilorLuxottica
  "RMS.PA",   # Hermès
  "KER.PA",   # Kering
  "LR.PA",    # Legrand
  "OR.PA",    # L'Oréal
  "MC.PA",    # LVMH
  "ML.PA",    # Michelin
  "ORA.PA",   # Orange
  "RI.PA",    # Pernod Ricard
  "PUB.PA",   # Publicis
  "RNO.PA",   # Renault
  "SAF.PA",   # Safran
  "SGO.PA",   # Saint-Gobain
  "SAN.PA",   # Sanofi
  "SU.PA",    # Schneider Electric
  "GLE.PA",   # Société Générale
  "STM.PA",   # STMicroelectronics
  "TEP.PA",   # Teleperformance
  "HO.PA",    # Thales
  "TTE.PA",   # TotalEnergies
  "VIE.PA",   # Veolia
  "DG.PA",    # Vinci
  "VIV.PA"    # Vivendi
)

cat("Téléchargement de", length(tickers), "actions du CAC 40...\n")
cat("Source : Yahoo Finance | Période : depuis 2015-01-01\n\n")

success <- 0
errors  <- 0

for (ticker in tickers) {
  cat(sprintf("[%2d/%d] %-10s ", which(tickers == ticker), length(tickers), ticker))
  
  data <- tryCatch(
    getSymbols(ticker, src = "yahoo", from = "2015-01-01", auto.assign = FALSE),
    error = function(e) NULL
  )
  
  if (!is.null(data)) {
    # Extraire le prix de clôture ajusté (ou Close si Adjusted non dispo)
    cols <- colnames(data)
    adj_col   <- grep("Adjusted", cols, value = TRUE)
    close_col <- grep("Close", cols, value = TRUE)
    use_col   <- if (length(adj_col) > 0) adj_col[1] else close_col[1]
    
    data_df <- data.frame(
      Date  = index(data),
      Close = as.numeric(data[, use_col])
    )
    data_df <- data_df[!is.na(data_df$Close), ]
    
    write.csv(data_df, paste0("data/", ticker, ".csv"), row.names = FALSE)
    cat(sprintf("✓ %d obs. (%s → %s)\n", 
                nrow(data_df),
                format(min(data_df$Date), "%Y-%m-%d"),
                format(max(data_df$Date), "%Y-%m-%d")))
    success <- success + 1
  } else {
    cat("✗ Erreur de téléchargement\n")
    errors <- errors + 1
  }
  
  # Pause pour ne pas surcharger les serveurs Yahoo Finance
  Sys.sleep(1)
}

# --- Import du fichier BNPPARIBAS local (format Boursorama) ---
cat("\n── Import des fichiers locaux ─────────────────────────────\n")
bnp_files <- list.files(".", pattern = "BNPPARIBAS.*\\.txt$", full.names = TRUE)

for (bnp_file in bnp_files) {
  cat(sprintf("Import de %s ... ", basename(bnp_file)))
  df <- tryCatch({
    d <- read.delim(bnp_file, sep = "\t", stringsAsFactors = FALSE,
                    fill = TRUE, strip.white = TRUE)
    
    # Parser les dates (format Boursorama : DD/MM/YYYY HH:MM)
    date_str <- gsub("\\s+\\d{2}:\\d{2}(:\\d{2})?.*$", "", trimws(d$date))
    dates    <- as.Date(date_str, format = "%d/%m/%Y")
    
    # Parser les prix de clôture
    close_vals <- as.numeric(gsub(",", ".", as.character(d$clot)))
    
    result <- data.frame(Date = dates, Close = close_vals)
    result <- result[!is.na(result$Date) & !is.na(result$Close), ]
    result[order(result$Date), ]
  }, error = function(e) NULL)
  
  if (!is.null(df) && nrow(df) > 0) {
    ticker_name <- gsub("_\\d{4}-\\d{2}-\\d{2}$", "", gsub("\\.txt$", "", basename(bnp_file)))
    write.csv(df, paste0("data/", ticker_name, ".csv"), row.names = FALSE)
    cat(sprintf("✓ %d obs.\n", nrow(df)))
    success <- success + 1
  } else {
    cat("✗ Erreur\n")
    errors <- errors + 1
  }
}

# --- Résumé ---
cat("\n══════════════════════════════════════════════════════════════\n")
cat(sprintf("  Terminé ! %d succès, %d erreurs\n", success, errors))
cat("══════════════════════════════════════════════════════════════\n")
cat("\nFichiers disponibles dans data/ :\n")
files <- list.files("data", pattern = "\\.csv$")
cat(paste(" ", files, collapse = "\n"), "\n")
cat("\nVous pouvez maintenant lancer l'application :\n")
cat("  shiny::runApp()\n")
