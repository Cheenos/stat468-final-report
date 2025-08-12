# --- EDIT THESE TWO LINES -----------------------------------------------------
BUCKET   <- "pokermetricbucket"     # your S3 bucket (no s3://)
DATAPATH <- "C:/Users/eddy2/OneDrive/Documents/Stat 468/Final Project/PokerDta/poker_log-Hand1.xlsx"
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(vetiver)
  library(pins)
  library(depmixS4)   # HMM
})

# Helpers (build_hmm_features, train_hmm, predict.hmm_bluff)
# Make sure this file exists; otherwise paste those functions here.
source("PokerMetricGit.R")

# --- Load data ---------------------------------------------------------------
if (!file.exists(DATAPATH)) stop("Can't find file: ", DATAPATH)

ext <- tolower(tools::file_ext(DATAPATH))
df  <- switch(
  ext,
  rds  = readRDS(DATAPATH),
  csv  = readr::read_csv(DATAPATH, show_col_types = FALSE),
  xlsx = readxl::read_xlsx(DATAPATH),
  stop("Unsupported file type: ", ext, " (use .csv, .xlsx, or .rds)")
)

# If the file is raw actions, make sure analytics columns exist
needed_cols <- c("WagerOdds","Equity","PotBefore","BetAmount","Action")
if (!all(needed_cols %in% names(df))) {
  # If recalc_analytics() lives in your app file, source it:
  if (!exists("recalc_analytics")) {
    if (file.exists("app.R")) source("app.R")
  }
  if (!exists("recalc_analytics")) {
    stop("Data is raw and recalc_analytics() is not available. ",
         "Either export a processed log from your app, ",
         "or source a file that defines recalc_analytics().")
  }
  df <- recalc_analytics(df)
}

# --- Build features & train HMM ----------------------------------------------
at <- build_hmm_features(df)
if (nrow(at) < 3) stop("Need at least 3 bet/raise rows to train HMM; got ", nrow(at))

hmm_model <- train_hmm(at)

# --- Wrap for vetiver --------------------------------------------------------
ptype <- at[0, c("BluffGap","RelSize")]  # schema vetiver expects at predict time
v <- vetiver_model(
  model          = hmm_model,
  model_name     = "hmm_bluff",
  prototype_data = ptype,
  description    = "2-state Gaussian HMM for bluff probability"
)

# --- Pin to S3 ---------------------------------------------------------------
# Ensure ~/.Renviron has AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_DEFAULT_REGION
board <- pins::board_s3(bucket = BUCKET)
vetiver_pin_write(board, v)

cat("✓ HMM pinned to S3 bucket:", BUCKET, "as 'hmm_bluff'\n")
