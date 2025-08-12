BUCKET   <- "pokermetricbucket"  
DATAPATH <- "C:/Users/eddy2/OneDrive/Documents/Stat 468/Final Project/PokerDta/poker_log-Hand1.xlsx"

library(dplyr)
library(readr)
library(readxl)
library(vetiver)
library(pins)
library(depmixS4)
library(tibble)


# ---- Inline helpers ---------------------------------------------------------
build_hmm_features <- function(df) {
  df %>%
    filter(Action %in% c("bet", "raise")) %>%
    mutate(
      BluffGap = WagerOdds - Equity,
      RelSize  = if_else(PotBefore > 0, BetAmount / PotBefore, NA_real_)
    ) %>%
    filter(
      !is.na(BluffGap), is.finite(BluffGap),
      !is.na(RelSize),  is.finite(RelSize)
    ) %>%
    select(BluffGap, RelSize, everything())
}

train_hmm <- function(at) {
  mod <- depmixS4::depmix(
    response = list(BluffGap ~ 1, RelSize ~ 1),
    data     = at,
    nstates  = 2,
    family   = list(gaussian(), gaussian())
  )
  fit_mod <- depmixS4::fit(mod, verbose = FALSE)
  post_tr <- depmixS4::posterior(fit_mod)
  cluster_means <- tapply(at$BluffGap, post_tr$state, mean)
  bluff_state <- as.integer(names(which.max(cluster_means)))
  mdl <- list(
    fit = fit_mod,
    bluff_state = bluff_state,
    feature_names = c("BluffGap","RelSize")
  )
  class(mdl) <- "hmm_bluff"
  mdl
}

predict.hmm_bluff <- function(object, new_data, ...) {
  nd <- as.data.frame(new_data)[, object$feature_names, drop = FALSE]
  post <- depmixS4::posterior(object$fit, newdata = nd)
  as.numeric(post[[paste0("S", object$bluff_state)]])
}

# ---- Load data ---------------------------------------------------------------
if (!file.exists(DATAPATH)) stop("Can't find file: ", DATAPATH)
ext <- tolower(tools::file_ext(DATAPATH))
df  <- switch(
  ext,
  rds  = readRDS(DATAPATH),
  csv  = readr::read_csv(DATAPATH, show_col_types = FALSE),
  xlsx = readxl::read_xlsx(DATAPATH),
  stop("Unsupported file type: ", ext)
)

needed_cols <- c("WagerOdds","Equity","PotBefore","BetAmount","Action")
if (!all(needed_cols %in% names(df))) {
  if (!exists("recalc_analytics")) {
    stop("Missing recalc_analytics() to process raw data.")
  }
  df <- recalc_analytics(df)
}

# ---- Train HMM ---------------------------------------------------------------
at <- build_hmm_features(df)
if (nrow(at) < 3) stop("Need at least 3 bet/raise rows to train HMM; got ", nrow(at))
hmm_model <- train_hmm(at)

# ---- Wrap for vetiver & pin to S3 --------------------------------------------
proto <- tibble(BluffGap = double(), RelSize = double())

v <- vetiver::vetiver_model(
  model          = hmm_model,
  model_name     = "hmm_bluff",
  description    = "2-state Gaussian HMM for bluff probability",
  save_ptype     = TRUE,
  ptype          = proto   # passed via ... in your vetiver version
)

board <- pins::board_s3(bucket = BUCKET)
vetiver::vetiver_pin_write(board, v)

cat("✓ HMM pinned to S3 bucket:", BUCKET, "as 'hmm_bluff'\n")

