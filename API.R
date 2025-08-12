# API.R (no auto-run)
library(plumber)
library(depmixS4)
library(dplyr)
library(tibble)
library(jsonlite)

hmm_bluff_probs <- function(df) {
  probs <- rep(NA_real_, nrow(df))
  good  <- is.finite(df$BluffGap) & is.finite(df$RelSize)
  at    <- df[good, , drop = FALSE]
  if (sum(good) < 3) return(probs)
  mod <- depmix(
    response = list(BluffGap ~ 1, RelSize ~ 1),
    data     = at,
    nstates  = 2,
    family   = list(gaussian(), gaussian())
  )
  fit_mod <- tryCatch(fit(mod, verbose = FALSE), error = function(e) NULL)
  if (is.null(fit_mod)) return(probs)
  post_df       <- posterior(fit_mod)
  cluster_means <- tapply(at$BluffGap, post_df$state, mean)
  bluff_state   <- as.integer(names(which.max(cluster_means)))
  probs[good]   <- post_df[[paste0("S", bluff_state)]]
  probs
}
fallback_probs <- function(df) {
  z <- (df$BluffGap - mean(df$BluffGap, na.rm = TRUE)) /
    (sd(df$BluffGap,  na.rm = TRUE) + 1e-9)
  as.numeric(plogis(1.5 * z + 0.5 * df$RelSize))
}

#* @get /ping
function() list(status = "ok")

#* @post /predict
function(req, res) {
  body <- tryCatch(fromJSON(req$postBody, simplifyDataFrame = TRUE),
                   error = function(e) NULL)
  if (is.null(body) || is.null(body$data))
    return(list(error="POST JSON must be {\"data\":[{\"BluffGap\":...,\"RelSize\":...}]}"))
  df <- as.data.frame(body$data)
  need <- c("BluffGap","RelSize")
  if (!all(need %in% names(df))) return(list(error="Missing columns: BluffGap, RelSize"))
  df$BluffGap <- as.numeric(df$BluffGap); df$RelSize <- as.numeric(df$RelSize)
  probs <- hmm_bluff_probs(df); if (all(is.na(probs))) probs <- fallback_probs(df)
  list(bluff_prob = probs)
}

#* @plumber
function(pr) pr
