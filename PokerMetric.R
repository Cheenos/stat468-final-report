# +++ Package Installation (Run once) +++
# Install required packages for the project from CRAN using Windows binaries
#install.packages(
#  c("xfun","nanonext","parallelly","promises","writexl","readxl","readr", "ggrepel"),
#  repos="https://cloud.r-project.org", type="win.binary"
#)
# Install Shiny specifically from CRAN
#install.packages(
#  "shiny",
#  repos="https://cloud.r-project.org",
#  type="win.binary"
#)
# +++ Load necessary libraries +++
#install.packages("aws.s3")
library(aws.s3)
library(shiny)        # Framework for web application
library(bslib)        # Bootstrap library for enhanced UI styling
library(dplyr)        # Data manipulation
library(tidyr)        # Data tidying
library(stringr)      # String manipulation
library(purrr)        # Functional programming tools
library(magrittr)     # Pipe operator %>%
library(readr)        # CSV file reading and writing
library(readxl)       # Excel file reading
library(writexl)      # Excel file writing
library(depmixS4)     # Hidden Markov Models (HMMs)
library(conflicted)   # Handling function conflicts
library(ggplot2)      # Data visualization
library(ggrepel)      # Enhanced text labeling in plots

# Custom coalesce operator: returns 'a' if not null, otherwise 'b'
`%||%` <- function(a, b) if (is.null(a)) b else a

# +++ Conflict management +++
# Explicitly resolve function conflicts to ensure the correct package function is used
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# ─── Robust Chen-Score & Equity Proxy Calculation ──────────────────────────
# Functions for calculating Chen scores and estimating poker hand equity

# Calculates the Chen formula score for two-card poker hands
chen_score <- function(cards) {
  # cards must be exactly two non‐NA strings
  if (length(cards) != 2 || any(is.na(cards))) return(NA_real_)
  
  ranks  <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  values <- setNames(2:14, ranks)
  r1 <- values[substr(cards[1],1,1)]
  r2 <- values[substr(cards[2],1,1)]
  
  # bail if either rank lookup failed
  if (is.na(r1) || is.na(r2)) return(NA_real_)
  
  high <- max(r1, r2)
  low  <- min(r1, r2)
  
  base <- if (high == low) 2*high else high
  if (substr(cards[1],2,2) == substr(cards[2],2,2)) base <- base * 1.25
  
  gap <- high - low
  penalty <- if      (gap <= 1) 0
  else if (gap == 2) 1
  else if (gap == 3) 2
  else             gap + 1
  
  score <- min(base - penalty, 20)
  round(score * 2) / 2
}

# Estimates the number of outs to improve poker hand strength
estimate_outs <- function(hole, board) {
  suits <- substr(c(hole,board),2,2)
  ranks <- substr(c(hole,board),1,1)
  outs  <- 0L
  
  # Check for flush draws
  sb <- table(suits)
  if (any(sb >= 4)) {
    fs   <- names(sb)[which.max(sb)]
    have <- sum(suits == fs)
    outs <- outs + (13 - have)
  }
  
  # Check for straight draws
  vals <- match(ranks, c("2","3","4","5","6","7","8","9","T","J","Q","K","A"))
  u    <- sort(unique(vals))
  for (st in (min(u)-4):(max(u))) {
    window <- st:(st+4)
    miss   <- setdiff(window, u)
    if (length(miss)==1 && all(window >= 2) && all(window <= 14)) {
      outs <- outs + 4L
    }
  }
  
  # Check for set draws
  tb <- table(ranks)
  if (any(tb == 2 & names(tb) %in% hole)) outs <- outs + 2L
  
  min(outs, 47L)
}

# Approximate player's hand equity based on the current game stage
# Drop-in safe replacement
approx_equity <- function(hole_str, board_vec = character(0)) {
  # guards so we never index [[1]] on empty input
  if (length(hole_str)==0 || is.na(hole_str) || !nzchar(hole_str)) return(NA_real_)
  hole_str <- gsub("10", "T", hole_str, fixed = TRUE)
  
  parts <- strsplit(hole_str, "\\s+")[[1]]
  if (length(parts) != 2 || any(!nzchar(parts))) return(NA_real_)
  hole <- parts
  
  # clean board
  if (is.null(board_vec)) board_vec <- character(0)
  board_vec <- gsub("10","T", board_vec, fixed = TRUE)
  board_vec <- board_vec[!is.na(board_vec) & nzchar(board_vec)]
  L <- length(board_vec)
  
  # === your original heuristic ===
  if      (L == 0) eq <- (chen_score(hole) - 2) / 18
  else if (L == 3) eq <- min(estimate_outs(hole, board_vec[1:3]) * 4/47, 1)
  else if (L == 4) eq <- min(estimate_outs(hole, board_vec[1:4])      /46, 1)
  else if (L >= 5) eq <- min(estimate_outs(hole, board_vec[1:4])      /46, 1)
  else                eq <- NA_real_
  
  eq <- pmin(pmax(eq, 0), 1)
  if (is.finite(eq)) eq else NA_real_
}

add_bluff_prob <- function(df) {
  df <- df %>% dplyr::mutate(.row = dplyr::row_number())
  
  at <- df %>%
    dplyr::filter(Action %in% c("bet","raise"),
                  is.finite(WagerOdds), is.finite(Equity)) %>%
    dplyr::mutate(
      BluffGap = WagerOdds - Equity,
      RelSize  = dplyr::if_else(PotBefore > 0, BetAmount / PotBefore, NA_real_)
    ) %>%
    dplyr::filter(is.finite(RelSize))
  
  # default: NA for everyone
  bluff_df <- tibble::tibble(.row = df$.row, bluff_prob = NA_real_)
  
  if (nrow(at) >= 3) {
    fit_mod <- tryCatch({
      feats <- scale(at[, c("BluffGap","RelSize")])
      fit(depmix(
        response = list(BluffGap ~ 1, RelSize ~ 1),
        data     = as.data.frame(feats),
        nstates  = 2,
        family   = list(gaussian(), gaussian())
      ), verbose = FALSE)
    }, error = function(e) NULL)
    
    if (!is.null(fit_mod)) {
      post <- posterior(fit_mod)
      cms  <- tapply(at$BluffGap, post$state, mean, na.rm = TRUE)
      bs   <- as.integer(names(which.max(cms)))
      bluff_df <- tibble::tibble(
        .row       = at$.row,
        bluff_prob = post[[paste0("S", bs)]]
      ) %>% dplyr::right_join(tibble::tibble(.row = df$.row), by = ".row")
    }
  }
  
  df %>%
    dplyr::select(-dplyr::any_of("bluff_prob")) %>%   # avoid .x/.y suffixes
    dplyr::left_join(bluff_df, by = ".row") %>%
    dplyr::select(-.row)
}



calc_ev_columns <- function(df) {
  df <- df %>%
    arrange(HandNumber, StreetOrder) %>%
    group_by(HandNumber) %>%
    mutate(
      PotBefore  = lag(cumsum(pmax(0, BetAmount)), default = 0),
      CostToCall = if_else(Action %in% c("call", "raise"), BetAmount, 0),
      WagerOdds  = if_else(CostToCall > 0,
                           CostToCall / (PotBefore + CostToCall),
                           NA_real_),
      # EVs for each type
      EV_call    = if_else(Action == "call",
                           (Equity - WagerOdds) * PotBefore,
                           NA_real_),
      EV_bet     = if_else(Action == "bet",
                           (Equity - 0.5) * BetAmount,
                           NA_real_),
      EV_raise   = if_else(Action == "raise",
                           (Equity - WagerOdds) * (PotBefore + CostToCall),
                           NA_real_)
    ) %>%
    ungroup()
  
  df
}


recalc_analytics <- function(df) {
  df %>%
    mutate(HoleCards = paste(Hole1, Hole2)) %>%
    group_by(HandNumber) %>%
    arrange(HandNumber,
            case_when(Street=="Pre-flop" ~ 1L,
                      str_starts(Street,"Flop") ~ 2L,
                      str_starts(Street,"Turn") ~ 3L,
                      str_starts(Street,"River")~ 4L,
                      TRUE ~ 5L)) %>%
    # make sure every row carries the known board
    tidyr::fill(Flop1, Flop2, Flop3, Turn, River, .direction = "downup") %>%
    ungroup() %>%
    mutate(
      BoardVec = purrr::pmap_chr(
        list(Flop1,Flop2,Flop3,Turn,River),
        ~ paste(na.omit(c(..1,..2,..3,..4,..5)), collapse=" ")
      ),
      Equity = mapply(
        approx_equity,
        HoleCards,
        strsplit(BoardVec, " ")
      )
    ) %>%
    # ... keep your existing CostToCall / WagerOdds / EV_* logic ...
    select(-BoardVec)
}



get_action_summary <- function(log_path) {
  # ─── 1) Read & normalize ─────────────────────────────────────────────────
  lines <- readLines(log_path, warn = FALSE)
  lines <- gsub("[“”]", "\"", lines)
  
  # ─── 2) Seats & starting stacks ──────────────────────────────────────────
  seat_lines <- grep("^Seat [0-9]+:", lines, value = TRUE)
  initial_stacks <- data.frame(
    Player        = gsub('"','', sub("^Seat [0-9]+:\\s*(.*?)\\s*\\(.*$", "\\1", seat_lines)),
    StartingStack = as.numeric(sub("^.*\\((\\d+)\\).*$", "\\1", seat_lines)),
    stringsAsFactors = FALSE
  )
  
  # ─── 3) Split into hands ─────────────────────────────────────────────────
  markers <- grep("^[-–—]+\\s*Hand [0-9]+\\s*[-–—]+$", trimws(lines))
  starts  <- markers
  ends    <- c(markers[-1] - 1, length(lines))
  hands   <- lapply(seq_along(starts), function(i) lines[starts[i]:ends[i]])
  
  # ─── 4) Parse hole cards ─────────────────────────────────────────────────
  parse_holecards <- function(hands) {
    do.call(rbind, lapply(seq_along(hands), function(h) {
      ln   <- hands[[h]]
      hc_i <- grep("^\\*\\*Hole Cards", ln)
      pf_i <- grep("^\\*\\*Pre-flop",  ln)
      hc   <- ln[(hc_i+1):(pf_i-1)]
      hc   <- sub("^\\s*[-–]\\s*", "", hc)
      data.frame(
        Hand      = h,
        Player    = sub("\\s*\\[.*","", hc),
        HoleCards = sub("^.*\\[([^]]+)\\].*$","\\1", hc),
        stringsAsFactors = FALSE
      )
    }))
  }
  hole_df <- parse_holecards(hands)
  
  # ─── 5) Build raw action rows ─────────────────────────────────────────────
  rows <- do.call(rbind, lapply(seq_along(hands), function(h) {
    ln <- hands[[h]]; n <- length(ln)
    pf_i    <- grep("\\*\\*Pre-flop", ln)
    flop_i  <- grep("\\*\\*Flop",    ln)
    turn_i  <- grep("\\*\\*Turn",    ln)
    river_i <- grep("\\*\\*River",   ln)
    slice   <- function(s,e) {
      if (!length(s) || is.na(s) || s>n) return(character(0))
      e2 <- if (!length(e) || is.na(e) || e<=s) n else e-1
      ln[(s+1):e2]
    }
    clean   <- function(x) x[!grepl("^(\\*\\*|\\s*$)", x)]
    explode <- function(x) if (length(x)) unlist(strsplit(clean(x), ";\\s*")) else character(0)
    pf_s    <- explode(slice(pf_i,   flop_i))
    flop_s  <- explode(slice(flop_i, turn_i))
    turn_s  <- explode(slice(turn_i, river_i))
    river_s <- explode(slice(river_i, n+1))
    tag     <- function(idx, lab) {
      if (!length(idx)) return(NULL)
      b <- sub("^.*\\[([^]]+)\\].*$","\\1", ln[idx][1])
      paste0(lab, if (lab!="Pre-flop") paste0(" [", b, "]") else "")
    }
    tags <- c(Pre="Pre-flop",
              Flop=tag(flop_i,"Flop"),
              Turn=tag(turn_i,"Turn"),
              Riv =tag(river_i,"River"))
    mk <- function(acts, street) {
      if (!length(acts)) return(NULL)
      data.frame(Hand=h, Street=street,
                 Player=sub("^(\\S+).*","\\1",acts),
                 Action=acts, stringsAsFactors=FALSE)
    }
    dplyr::bind_rows(
      mk(pf_s,    tags["Pre"]),
      mk(flop_s,  tags["Flop"]),
      mk(turn_s,  tags["Turn"]),
      mk(river_s, tags["Riv"])
    )
  })) %>%
    dplyr::group_by(Hand) %>%
    dplyr::mutate(Seq = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # ─── 6) Precompute equity lookup ──────────────────────────────────────────
  streets_df  <- dplyr::distinct(rows, Hand, Street)
  lookup_grid <- hole_df %>%
    dplyr::inner_join(streets_df, by="Hand") %>%
    dplyr::filter(!is.na(HoleCards), HoleCards!="")
  equity_lookup <- purrr::pmap_dfr(
    lookup_grid,
    function(Hand, Player, HoleCards, Street) {
      tibble::tibble(
        Hand   = Hand,
        Player = Player,
        Street = Street,
        Equity = approx_equity(HoleCards, Street)
      )
    }
  )
  
  # ─── Data Analytics for Poker Hands ────────────────────────────────────
  # Functions to recalculate analytics and summarize actions from poker logs
  action_table <- rows %>%
    dplyr::left_join(hole_df,        by=c("Hand","Player")) %>%
    dplyr::left_join(initial_stacks, by="Player") %>%
    dplyr::filter(
      !grepl("^Stacks:", Action),
      !grepl("^Final",  Player),
      Player!="" & !is.na(Player)
    ) %>%
    tidyr::separate_rows(Action, sep="[,;]\\s*") %>%
    dplyr::mutate(Action = stringr::str_trim(Action)) %>%
    
    ### STEP A: extract BetAmount first ###
    dplyr::mutate(
      BetAmount = as.numeric(stringr::str_extract(Action, "\\d+"))
    ) %>%
    
    ### STEP B: then recode the Action text ###
    dplyr::mutate(
      Action = dplyr::case_when(
        stringr::str_detect(Action, "\\bposts SB\\b") ~ "post_sb",
        stringr::str_detect(Action, "\\bposts BB\\b") ~ "post_bb",
        stringr::str_detect(Action, regex("\\braise",ignore_case=TRUE)) ~ "raise",
        stringr::str_detect(Action, regex("\\bbet",  ignore_case=TRUE)) ~ "bet",
        stringr::str_detect(Action, regex("\\bcall", ignore_case=TRUE)) ~ "call",
        stringr::str_detect(Action, regex("\\bcheck",ignore_case=TRUE)) ~ "check",
        stringr::str_detect(Action, regex("\\bfold", ignore_case=TRUE)) ~ "fold",
        TRUE ~ NA_character_
      )
    ) %>%
    
    # ─── track stacks ────────────────────────────────────────────────────
    dplyr::group_by(Hand,Player) %>%
    dplyr::arrange(Seq, .by_group=TRUE) %>%
    dplyr::mutate(
      StackAfter  = StartingStack - cumsum(coalesce(BetAmount,0)),
      StackBefore = dplyr::lag(StackAfter, default=first(StartingStack))
    ) %>%
    dplyr::ungroup() %>%
    
    # ─── pot accumulation ────────────────────────────────────────────────
    dplyr::mutate(Contribution = pmin(coalesce(BetAmount,0), StackBefore)) %>%
    dplyr::group_by(Hand) %>%
    dplyr::arrange(Seq, .by_group=TRUE) %>%
    dplyr::mutate(
      Pot       = cumsum(Contribution),
      PotBefore = dplyr::lag(Pot, default=0)
    ) %>%
    dplyr::ungroup() %>%
    
    # ─── equity & pot‐odds ────────────────────────────────────────────────
    dplyr::left_join(equity_lookup, by=c("Hand","Player","Street")) %>%
    dplyr::mutate(
      Equity = mapply(approx_equity, HoleCards, Street),
      CostToCall  = if_else(Action=="call", BetAmount, NA_real_),
      PotOdds     = CostToCall / (PotBefore + CostToCall),
      Deviance    = Equity - PotOdds
    ) %>%
    
    # ─── wager‐odds & EVs ────────────────────────────────────────────────
    dplyr::group_by(Hand) %>%
    dplyr::mutate(
      PrevBet     = dplyr::lag(if_else(Action %in% c("bet","raise","call"), BetAmount,0),
                               default=0),
      CostOfWager = dplyr::case_when(
        Action=="call"  ~ BetAmount,
        Action=="bet"   ~ BetAmount,
        Action=="raise" ~ BetAmount - PrevBet,
        TRUE            ~ NA_real_
      ),
      WagerOdds = CostOfWager / (PotBefore + CostOfWager),
      EV_call   = if_else(
        Action=="call",
        Equity * (PotBefore + CostToCall) - CostToCall,
        NA_real_
      ),
      EV_bet    = if_else(
        Action=="bet",
        Equity * (PotBefore + BetAmount) - (1 - Equity) * BetAmount,
        NA_real_
      ),
      EV_raise  = if_else(
        Action=="raise",
        Equity * (PotBefore + CostOfWager)
        - (1 - Equity) * CostOfWager,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    
    # ─── final select & order ────────────────────────────────────────
    dplyr::arrange(Hand, Street) %>%
    dplyr::select(
      Hand, Player, HoleCards, Street,
      Action, BetAmount,
      StackBefore, StackAfter,
      Equity, WagerOdds,
      EV_call, EV_bet, EV_raise
    )
  
  action_table

  
}

# empty template
initial_log <- data.frame(
  HandNumber   = integer(),
  Button       = character(),
  Player       = character(),
  Action       = character(),
  BetAmount    = numeric(),
  StackBefore  = numeric(),
  StackAfter   = numeric(),
  PotBefore    = numeric(),   
  Hole1        = character(),
  Hole2        = character(),
  Street       = character(),
  Flop1        = character(),
  Flop2        = character(),
  Flop3        = character(),
  Turn         = character(),
  River        = character(),
  Equity       = numeric(),
  WagerOdds    = numeric(),
  EV_call      = numeric(),
  EV_bet       = numeric(),
  EV_raise     = numeric(),
  stringsAsFactors = FALSE
)

# Poker Data Recorder – Shiny UI ----------------------------------------------------
ui <- fluidPage(
  
  
  # --- Force browser into full‑screen mode when the Shiny session connects ----
  tags$script(HTML("
    document.addEventListener('shiny:connected', function(event) {
      var el = document.documentElement;
      if      (el.requestFullscreen)       el.requestFullscreen();
      else if (el.mozRequestFullScreen)    el.mozRequestFullScreen();
      else if (el.webkitRequestFullscreen) el.webkitRequestFullscreen();
      else if (el.msRequestFullscreen)     el.msRequestFullscreen();
    });
  ")),
  
  # --- Theming (Bootswatch "minty" + custom palette & font) ------------------  
  theme = bs_theme(
    bootswatch  = "minty",
    primary     = "#2C3E50",
    secondary   = "#18BC9C",
    base_font   = "Source Sans Pro"
  ),
  
  # 1. Buttons – slimmer padding & radius
  tags$head(tags$style(HTML("
    .btn { padding: 0.4rem 0.8rem !important; font-size: 0.9rem !important;
           border-radius: 0.25rem !important; margin-right: 0.5rem !important; }
    .action-buttons { display: flex; gap: 0.5rem; margin-top: 0.5rem; }
    .btn-next { font-size: 0.95rem !important; padding: 0.5rem 1rem !important; }
  "))),
  
  # 2. Hand‑history table – tighter, non‑wrapping cell layout
  tags$head(tags$style(HTML("
    /* target the log_table output */
    #log_table table {
      width: 100%;
      border-collapse: collapse;
      font-size: 0.85rem;      /* a bit smaller text */
    }
    #log_table th,
    #log_table td {
      padding: 0.25rem 0.5rem;  /* tighter padding */
      line-height: 1.2;        /* less vertical space */
      white-space: nowrap;     /* keep things on one line */
    }
    /* optional: reduce header weight */
    #log_table th {
      font-weight: 500;
    }
  "))),
  
  # --- Title ------------------------------------------------------------------
  titlePanel(div(icon("hand-spock"), " Poker Data Recorder")),
  
  # LAYOUT: sidebar (input) on the left, main panel (outputs) on the right
  sidebarLayout(
    #Sidebar
    sidebarPanel(width = 4,
                 tabsetPanel(id = "mode",
                             
                             # ─── Hand Setup ─────────────────────────────────────
                             tabPanel("Hand Setup",
                                      br(),
                                      # Player names text area – accept one name per line 
                                      textAreaInput("players","Player names (one per line):",
                                                    placeholder="Alice\nBob\nCarol\nDavid",
                                                    rows=3, width="100%"),
                                      hr(),
                                      
                                      # Upload raw PokerStars text log
                                      fileInput("upload_txt","Upload PokerStars text log (.txt):",
                                                accept=".txt", multiple=FALSE),
                                      hr(),
                                      
                                      # Upload existing cleaned logs (xlsx / csv)
                                      fileInput("upload_logs","Upload existing .xlsx/.csv logs:",
                                                accept=c(".xlsx",".csv"), multiple=TRUE),
                                      hr(),
                                      
                                      # Street selector + contextual card inputs
                                      radioButtons("street","Street:",
                                                   choices=c("Pre-flop","Flop","Turn","River"),
                                                   inline=TRUE),
                                      
                                      # Only show card inputs relevant to chosen street
                                      conditionalPanel("input.street=='Flop'",
                                                       fluidRow(
                                                         column(4, textInput("flop1","Flop 1:", width="80px")),
                                                         column(4, textInput("flop2","Flop 2:", width="80px")),
                                                         column(4, textInput("flop3","Flop 3:", width="80px"))
                                                       )
                                      ),
                                      conditionalPanel("input.street=='Turn'",
                                                       textInput("turn","Turn:", width="80px")
                                      ),
                                      conditionalPanel("input.street=='River'",
                                                       textInput("river","River:", width="80px")
                                      ),
                                      hr(),
                                      
                                      # Dealer button chooser – populated server‑side
                                      selectInput("button","Dealer Button:", choices=character(0), width="100%"),
                                      hr(),
                                      
                                      # Mass‑update: set all stacks to same value
                                      numericInput("set_all_stack","Set all stacks to:", value=0, min=0, width="100%"),
                                      actionButton("set_all","Set All Stacks", icon=icon("sync-alt"),
                                                   class="btn-sm btn-outline-info", width="100%"),
                                      
                                      # Expandable details showing current stack list
                                      tags$details(open=NA,
                                                   tags$summary("Show Setup Details"), br(),
                                                   uiOutput("player_setup_ui")
                                      )
                             ),
                             
                             # ─── Player Action ─────────────────────────────────
                             tabPanel("Player Action",
                                      br(),
                                      
                                      # Header row: next‑hand button + hand counter
                                      fluidRow(
                                        column(6,
                                               actionButton("next_hand","Next Hand", icon=icon("fast-forward"),
                                                            class="btn-primary btn-sm btn-next")
                                        ),
                                        column(6,
                                               strong("Hand #:"), textOutput("hand_number", inline=TRUE)
                                        )
                                      ),
                                      hr(),
                                      
                                      # Player & hole‑card context
                                      selectInput("player","Player:", choices=character(0), width="100%"),
                                      br(),
                                      uiOutput("holecards_display"),
                                      hr(),
                                      
                                      # Action + amount inputs
                                      fluidRow(
                                        column(6,
                                               selectInput("action","Action:",
                                                           choices=c("post_sb","post_bb","fold","check","call",
                                                                     "bet","raise","all-in"),
                                                           width="100%")
                                        ),
                                        column(6,
                                               numericInput("bet","Amount:",0,min=0,width="100%")
                                        )
                                      ),
                                      hr(),
                                      
                                      # Action buttons: add / download / delete
                                      div(class="action-buttons",
                                          actionButton("add", tagList(icon("plus"), "Add Entry"),
                                                       class="btn-outline-success btn-sm"),
                                          downloadButton("dl_xlsx", tagList(icon("file-excel"), "Download"),
                                                         class="btn-outline-info btn-sm"),
                                          actionButton("delete_last", tagList(icon("trash-alt"), "Delete Last"),
                                                       class="btn-outline-danger btn-sm")
                                      )
                             ),
                             sliderInput("bluff_thresh", "Flag bluff when Pr ≥", min = 0.5, max = 0.95, value = 0.7, step = 0.01)
                             
                             
                 ) # end tabsetPanel
    ),
    
    mainPanel(
      
      # ----- Collapsible: Hand History Log -----------------------------------
      tags$details(style = "margin-top:1em;", open = FALSE,
                   tags$summary(style = "font-size:1.2em; font-weight:600; cursor:pointer;", "Hand History Log"),
                   tableOutput("log_table")
      ),
      
      # ----- Collapsible: Stack trajectories plot ----------------------------
      tags$details(style = "margin-top:1em;", open = FALSE,
                   tags$summary(style = "font-size:1.2em; font-weight:600; cursor:pointer;", "Stack Trajectories"),
                   plotOutput("stack_plot", height = "400px")
      ),
      
      # ----- Collapsible: Player aggression profile --------------------------
      tags$details(style = "margin-top:1em;", open = FALSE,
                   tags$summary(style = "font-size:1.2em; font-weight:600; cursor:pointer;", "Player Aggression Profile"),
                   selectInput("focus_player", "View EV Score for:", choices = NULL),
                   
                   # When 'Summary' is chosen: show overview plot
                   conditionalPanel(
                     condition = "input.focus_player == 'Summary'",
                     plotOutput("player_profile", height = "400px")
                     # removed htmlOutput("profile_summary") from here
                   ),
                   
                   # Otherwise: show EV trend for chosen player 
                   conditionalPanel(
                     condition = "input.focus_player != 'Summary'",
                     plotOutput("ev_trend", height = "400px")
                   )
      ),
      
      # ----- Collapsible: Optimality rankings (table + narrative)
      tags$details(style = "margin-top:1em;", open = FALSE,
                   tags$summary(style = "font-size:1.2em; font-weight:600; cursor:pointer;", "Optimality Rankings"),
                   div(style = "display: flex; gap: 2rem; align-items: flex-start;",
                       # left: the table
                       div(style = "flex: 1;",
                           tableOutput("opt_table")
                       ),
                       # right: move Player Aggression summary here
                       div(style = "flex: 1;",
                           htmlOutput("profile_summary", style = "margin-top:0.5em; font-size:0.95em;")
                       )
                   )
      )
    )
    
    
    
  )
)

# Poker Data Recorder – Shiny SERVER
server <- function(input, output, session) {
  # Reactive VALUE to hold the growing hand‑history data.frame. 
  rv <- reactiveVal(initial_log)
  
  # Helper reactives to parse the player list that the user enters:
  players    <- reactive({
    strsplit(input$players, "\n")[[1]] %>%
      trimws() %>%
      (\(x) x[nzchar(x)])()
  })
  player_ids <- reactive(make.names(players()))
  
  # Dynamic setup UI (stacks + hole‑cards) – this chunk builds an HTML fragment on‑the‑fly whenever the player list changes. 
  output$player_setup_ui <- renderUI({
    req(players())
    tagList(
      fluidRow(
        column(3, strong("Player")), column(4, strong("Stack")),
        column(2, strong("Hole 1")), column(2, strong("Hole 2"))
      ),
      lapply(seq_along(players()), function(i) {
        id <- player_ids()[i]; nm <- players()[i]
        fluidRow(
          column(3, strong(nm)),
          column(4, numericInput(paste0("start_",id), NULL, value=0, min=0, width="100%")),
          column(2, textInput(paste0("hc1_",id), NULL, placeholder="♠A", width="100%")),
          column(2, textInput(paste0("hc2_",id), NULL, placeholder="♥K", width="100%"))
        )
      })
    )
  })
  
  # Dynamically renders the two hole-cards for the player currently chosen in
  # the “Player Action” tab so you can double-check what you typed before adding
  # an action to the log.
  output$holecards_display <- renderUI({
    req(input$player)                             # ensure a player is selected first
    pid <- make.names(input$player)
    h1  <- input[[paste0("hc1_",pid)]] %||% ""    # first card (or blank)
    h2  <- input[[paste0("hc2_",pid)]] %||% ""    # second card (or blank)
    
    # Show label + the two cards in a single row
    fluidRow(
      column(3, strong("Hole Cards:")), column(3, h5(h1)), column(3, h5(h2))
    )
  })
  
  # Keeps a live copy of each player’s current stack size.
  stacks <- reactiveValues()
  
  ## 4a) Initialize stacks whenever the player list changes
  observeEvent(player_ids(), {
    for(id in player_ids()) stacks[[id]] <- input[[paste0("start_",id)]] %||% 0
  }, ignoreNULL=FALSE)
  
  ## 4b) Bidirectional sync: reflect edits in the “Setup” panel
  observe({
    req(player_ids())
    for(id in player_ids()) {
      val <- input[[paste0("start_",id)]]
      if (!is.null(val)) stacks[[id]] <- val
    }
  })
  
  ## 4c) Mass-update: “Set All Stacks” button 
  observeEvent(input$set_all, {
    for(id in player_ids()) {
      stacks[[id]] <- input$set_all_stack
      updateNumericInput(session, paste0("start_",id), value=input$set_all_stack)
    }
  })
  
  # 5) hand counter
  hand_num <- reactiveVal(1L)
  
  # “Next Hand” button → increment counter & jump back to Hand Setup tab
  observeEvent(input$next_hand, {
    hand_num(hand_num()+1L)
    updateTabsetPanel(session, "mode", selected="Hand Setup")
    
    # Carry forward any stack edits made during the previous hand
    for(id in player_ids()) {
      stacks[[id]] <- input[[paste0("start_",id)]] %||% stacks[[id]]
    }
  })
  
  # Display the live hand number in the UI
  output$hand_number <- renderText(hand_num())
  
  # 6) ── DROPDOWN CHOICES (Player / Dealer Button)
  observeEvent(players(), {
    updateSelectInput(session, "button", choices=players())
    updateSelectInput(session, "player", choices=players())
  })
  
  # 7) ingest Excel/CSV
  observeEvent(input$upload_logs, {
    req(input$upload_logs)
    
    # 1) read everything
    imported <- lapply(seq_len(nrow(input$upload_logs)), function(i) {
      f   <- input$upload_logs[i, ]
      ext <- tolower(tools::file_ext(f$name))
      switch(ext,
             xlsx = readxl::read_xlsx(f$datapath),
             csv  = readr::read_csv (f$datapath, show_col_types = FALSE),
             stop("Unsupported file type: ", ext)
      )
    }) %>% bind_rows()
    
    # 2) unify column names (HandNumber ↔ Hand, BetAmount ↔ Amount)
    imported2 <- imported %>%
      # “HandNumber” sometimes arrives as plain “Hand”
      rename_with(~ "HandNumber", any_of(c("HandNumber","Hand"))) %>%
      # Same for wager size
      rename_with(~ "BetAmount", any_of(c("BetAmount","Amount"))) %>%
      
      # 3) Re-run the pot / odds / equity logic so external logs play nicely with
      # everything you calculate inside the app.
      group_by(HandNumber) %>%
      arrange(HandNumber, Street) %>%
      mutate(
        HoleCards    = paste(Hole1, Hole2),
        
        # --- Pot evolution s
        Contribution = pmin(BetAmount, StackBefore),
        Pot          = cumsum(Contribution),
        PotBefore    = lag(Pot, default = 0),
        
        # --- Equity estimate
        Equity       = mapply(approx_equity, HoleCards, Street),
        
        # --- Pot odds for calls
        CostToCall   = if_else(Action == "call", BetAmount, NA_real_),
        PotOdds      = CostToCall / (PotBefore + CostToCall),
        
        # --- Wager odds
        PrevBet      = lag(if_else(Action %in% c("bet","raise","call"), BetAmount, 0), default = 0),
        CostOfWager  = case_when(
          Action=="call"  ~ BetAmount,
          Action=="bet"   ~ BetAmount,
          Action=="raise" ~ BetAmount - PrevBet,
          TRUE            ~ NA_real_
        ),
        WagerOdds    = CostOfWager / (PotBefore + CostOfWager),
        
        # --- Expected-value calcs 
        EV_call      = if_else(
          Action=="call",
          Equity * (PotBefore + CostToCall) - CostToCall,
          NA_real_
        ),
        EV_bet       = if_else(
          Action=="bet",
          Equity * (PotBefore + BetAmount) - (1 - Equity) * BetAmount,
          NA_real_
        ),
        EV_raise     = if_else(
          Action=="raise",
          Equity * (PotBefore + CostOfWager) - (1 - Equity) * CostOfWager,
          NA_real_
        )
      ) %>%
      ungroup() %>%
      
      # 4) drop the helpers
      select(-Contribution, -Pot, -CostToCall, -PrevBet, -CostOfWager)
    
    # 5) append into your reactive log
    rv(recalc_analytics(bind_rows(rv(), imported2)))
  })
  
  
  # 8) ── IMPORT & PARSE A RAW .TXT HAND HISTORY 
  observeEvent(input$upload_txt, {
    req(input$upload_txt)
    
    ## 8-1) Parse the PokerStars-style text file into tidy action row
    parsed <- get_action_summary(input$upload_txt$datapath) %>%
      rename(HandNumber = Hand) %>%
      mutate(
        Button    = NA_character_,
        Hole1     = word(HoleCards, 1),
        Hole2     = word(HoleCards, 2),
        Flop1     = NA_character_, Flop2 = NA_character_, Flop3 = NA_character_,
        Turn      = NA_character_, River     = NA_character_,
        PotBefore = NA_real_        # ← now exists
      ) %>%
      select(all_of(names(initial_log)))
    
    ## 8-2) Re-run all analytics so this new data meshes with in-app entries
    rv( recalc_analytics(bind_rows(rv(), parsed)) )
  })
  
  # 9) ── MANUAL “ADD ENTRY” BUTTON 
  observeEvent(input$add, {
    # Basic sanity: must have chosen a player & a dealer button
    req(input$player, input$button)
    pid <- make.names(input$player)
    sb  <- stacks[[pid]]; bet <- input$bet; sa <- sb - bet
    stacks[[pid]] <- sa
    
    ## Build one tidy row representing the action just entered
    new_row <- data.frame(
      HandNumber  = hand_num(),
      Button      = input$button,
      Player      = input$player,
      Action      = input$action,
      BetAmount   = bet,
      StackBefore = sb,
      StackAfter  = sa,
      Hole1       = input[[paste0("hc1_",pid)]],
      Hole2       = input[[paste0("hc2_",pid)]],
      Street      = input$street,
      # board cards are only populated on the matching street
      Flop1       = ifelse(input$street=="Flop", input$flop1, NA_character_),
      Flop2       = ifelse(input$street=="Flop", input$flop2, NA_character_),
      Flop3       = ifelse(input$street=="Flop", input$flop3, NA_character_),
      Turn        = ifelse(input$street=="Turn", input$turn, NA_character_),
      River       = ifelse(input$street=="River", input$river, NA_character_),
      stringsAsFactors = FALSE
    )
    ## Append, then recalc EV / odds / etc.
    rv(recalc_analytics(bind_rows(rv(), new_row)))
    updateNumericInput(session, "bet", value=0)
  })
  
  # 10) manual Delete Last Entry
  observeEvent(input$delete_last, {
    df <- rv()
    if (nrow(df)>0) {
      last_row <- df[nrow(df),]
      rv(df[-nrow(df),])
      
      ## Restore the stack for that player so numbers stay consistent
      pid <- make.names(last_row$Player)
      stacks[[pid]] <- last_row$StackBefore
      updateNumericInput(session, paste0("start_",pid),
                         value=last_row$StackBefore)
    }
  })
  
  # Ensure EV columns exist for all relevant actions
  df_with_ev <- function(df) {
    df %>%
      mutate(
        EV_call  = if_else(Action == "call"  & !is.na(Equity) & !is.na(WagerOdds),
                           Equity - WagerOdds, EV_call,  # keep pre-computed if present
                           missing = 0),
        EV_bet   = if_else(Action == "bet"   & !is.na(Equity),
                           Equity - 0.5, EV_bet,          # example: simple bet EV calc
                           missing = 0),
        EV_raise = if_else(Action == "raise" & !is.na(Equity) & !is.na(WagerOdds),
                           Equity - WagerOdds, EV_raise,  # adjust formula as needed
                           missing = 0)
      )
  }
  
  
  output$log_table <- renderTable({
    # --- 1) Load data safely
    df <- try(df_with_bluff(), silent = TRUE)
    if (inherits(df, "try-error") || is.null(df)) df <- rv()
    
    # --- 2) Hole cards in one field
    df$HoleCards <- ifelse(
      !is.na(df$Hole1) & !is.na(df$Hole2),
      paste(df$Hole1, df$Hole2),
      NA_character_
    )
    
    # --- 3) Pretty street labels with board cards
    df$Street <- dplyr::case_when(
      grepl("\\[", df$Street) ~ df$Street,
      df$Street == "Flop"  & !is.na(df$Flop1) ~ paste0("Flop [", df$Flop1, " ", df$Flop2, " ", df$Flop3, "]"),
      df$Street == "Turn"  & !is.na(df$Turn)  ~ paste0("Turn [", df$Turn, "]"),
      df$Street == "River" & !is.na(df$River) ~ paste0("River [", df$River, "]"),
      TRUE ~ df$Street
    )
    
    # --- 4) Order streets chronologically
    df$StreetOrder <- dplyr::case_when(
      df$Street == "Pre-flop"                 ~ 1L,
      stringr::str_starts(df$Street, "Flop")  ~ 2L,
      stringr::str_starts(df$Street, "Turn")  ~ 3L,
      stringr::str_starts(df$Street, "River") ~ 4L,
      TRUE                                    ~ 5L
    )
    
    # --- 5) Ensure bluff_prob exists
    if (!"bluff_prob" %in% names(df) || is.null(df$bluff_prob)) {
      df$bluff_prob <- NA_real_
    } else {
      df$bluff_prob <- as.numeric(df$bluff_prob)
    }
    
    # --- 6) Build PotBefore & PotOdds here so we don’t depend on upstream Pot column
    df <- df_with_ev(df)
    
    
    # --- 7) Pick the EV for *this* action
    ev_act <- dplyr::case_when(
      df$Action == "bet"   ~ df$EV_bet,
      df$Action == "raise" ~ df$EV_raise,
      df$Action == "call"  ~ df$EV_call,
      TRUE ~ NA_real_
    )
    
    # --- 8) Bluff flag
    thresh <- if (!is.null(input$bluff_thresh)) input$bluff_thresh else 0.70
    is_likely_bluff <- df$Action %in% c("bet","raise") &
      !is.na(df$bluff_prob) &
      df$bluff_prob >= thresh &
      !is.na(ev_act) & ev_act < 0 &
      df$Equity < 0.5   # only mark as bluff if low equity (< 50%)
    
    
    df$ActionLabel <- ifelse(is_likely_bluff,
                             paste0(df$Action, " \u26A0\uFE0F"),
                             df$Action)
    
    # --- 9) Arrange & select for output
    out <- df %>%
      arrange(HandNumber, StreetOrder) %>%
      dplyr::select(
        HandNumber, Street, Player, HoleCards,
        ActionLabel, BetAmount, StackBefore, StackAfter,
        Equity, WagerOdds, EV_call, EV_bet, EV_raise,
        bluff_prob
      )
    
    # --- 10) Friendly headers
    names(out) <- c(
      "Hand", "Street", "Player", "Hole Cards",
      "Action", "Amount", "Stack Before", "Stack After",
      "Equity", "Wager Odds", "EV call", "EV bet", "EV raise",
      "Bluff p"
    )
    
    out
  }, rownames = FALSE, striped = TRUE, hover = TRUE, na = "")

  
  # 12) ── DOWNLOAD BUTTON (Excel)
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("poker_log-",Sys.Date(),".xlsx"),
    content  = function(file) write_xlsx(rv(), path=file)
  )
  
  # 13) ── STACK & POT-VOLUME PLOT
  output$stack_plot <- renderPlot({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    
    df <- rv()
    
    ## 13-1) Per-hand starting stacks 
    df_stacks <- df %>%
      group_by(HandNumber, Player) %>%
      arrange(HandNumber, Street) %>%
      summarise(StackBefore = first(StackBefore), .groups = "drop")
    
    # make sure every (HandNumber × Player) pair exists,
    df_stacks <- df_stacks %>%
      complete(HandNumber = unique(df$HandNumber),
               Player     = unique(df$Player),
               fill = list(StackBefore = 0))
    
    ## 13-2) Per-hand total pot volume (simple sum of BetAmount)
    df_pot <- df %>%
      group_by(HandNumber) %>%
      summarise(PotEnd = sum(coalesce(BetAmount, 0)), .groups = "drop")
    
    ## 13-3) Draw the area (pot) and the lines (stacks)
    ggplot() +
      geom_area(
        data  = df_pot,
        aes(x = HandNumber, y = PotEnd, fill = "Pot Volume"),
        alpha = 0.4
      ) +
      geom_line(
        data   = df_stacks,
        aes(x = HandNumber, y = StackBefore, color = Player, group = Player),
        size   = 1.5,
        lineend = "round"
      ) +
      geom_point(
        data = df_stacks,
        aes(x = HandNumber, y = StackBefore, color = Player),
        size = 3
      ) +
      
      ## 13-4) Styling
      scale_x_continuous(breaks = unique(df_stacks$HandNumber)) +
      scale_fill_manual(
        name   = NULL,
        values = c("Pot Volume" = "skyblue")
      ) +
      scale_color_discrete(name = "Player") +
      labs(
        title = "Stack & Pot Volume by Hand",
        x     = "Hand Number",
        y     = "Chips / Pot Volume"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        axis.text.x     = element_text(angle = 45, hjust = 1)
      )
  })
  
  # 14) ── HMM-BASED BLUFF-PROBABILITY (return just row id + prob)
  output$log_table <- renderTable({
    df <- df_with_bluff()
    
    # Hole cards combined
    df$HoleCards <- ifelse(
      !is.na(df$Hole1) & !is.na(df$Hole2),
      paste(df$Hole1, df$Hole2),
      NA_character_
    )
    
    # Pretty street labels
    df$Street <- dplyr::case_when(
      grepl("\\[", df$Street) ~ df$Street,
      df$Street == "Flop"  & !is.na(df$Flop1) ~ paste0("Flop [", df$Flop1, " ", df$Flop2, " ", df$Flop3, "]"),
      df$Street == "Turn"  & !is.na(df$Turn)  ~ paste0("Turn [", df$Turn, "]"),
      df$Street == "River" & !is.na(df$River) ~ paste0("River [", df$River, "]"),
      TRUE ~ df$Street
    )
    
    # Street order for sorting
    df$StreetOrder <- dplyr::case_when(
      df$Street == "Pre-flop"                 ~ 1L,
      stringr::str_starts(df$Street, "Flop")  ~ 2L,
      stringr::str_starts(df$Street, "Turn")  ~ 3L,
      stringr::str_starts(df$Street, "River") ~ 4L,
      TRUE                                    ~ 5L
    )
    
    # EV for current action (already computed, but pick the right one)
    ev_act <- dplyr::case_when(
      df$Action == "bet"   ~ df$EV_bet,
      df$Action == "raise" ~ df$EV_raise,
      df$Action == "call"  ~ df$EV_call,
      TRUE ~ NA_real_
    )
    
    # Bluff flag
    thresh <- if (!is.null(input$bluff_thresh)) input$bluff_thresh else 0.70
    is_likely_bluff <- df$Action %in% c("bet","raise") &
      !is.na(df$bluff_prob) &
      df$bluff_prob >= thresh &
      !is.na(ev_act) & ev_act < 0 &
      df$Equity < 0.5
    
    df$ActionLabel <- ifelse(is_likely_bluff,
                             paste0(df$Action, " \u26A0\uFE0F"),
                             df$Action)
    
    out <- df %>%
      arrange(HandNumber, StreetOrder) %>%
      dplyr::select(
        HandNumber, Street, Player, HoleCards,
        ActionLabel, BetAmount, StackBefore, StackAfter,
        Equity, WagerOdds, EV_call, EV_bet, EV_raise,
        bluff_prob
      )
    
    names(out) <- c(
      "Hand", "Street", "Player", "Hole Cards",
      "Action", "Amount", "Stack Before", "Stack After",
      "Equity", "Wager Odds", "EV call", "EV bet", "EV raise",
      "Bluff p"
    )
    
    out
  }, rownames = FALSE, striped = TRUE, hover = TRUE, na = "")
  
  df_with_bluff <- reactive({
    add_bluff_prob(rv())   # uses whatever Equity/WagerOdds you already compute
  })
  
  
  
  # 15) ── PER-PLAYER HAND COUNT (hands_played)
  hands_played <- reactive({
    rv() %>%
      distinct(HandNumber, Player) %>% 
      count(Player, name = "n_hands")
  })
  
  
  # 16) ── PER-PLAYER SUMMARY METRICS (player_summary)
  player_summary <- reactive({
    df <- df_with_bluff()
    
    # Safety: if there’s no data yet
    if (is.null(df) || nrow(df) == 0) {
      return(tibble(
        Player       = character(),
        avg_equity   = double(),
        avg_odds     = double(),
        avg_EV_raise = double(),
        bluff_rate   = double(),
        n_actions    = integer(),
        n_hands      = integer(),
        vol_frac     = double()
      ))
    }
    
    # Order streets for pot building
    df$StreetOrder <- dplyr::case_when(
      df$Street == "Pre-flop"                 ~ 1L,
      stringr::str_starts(df$Street, "Flop")  ~ 2L,
      stringr::str_starts(df$Street, "Turn")  ~ 3L,
      stringr::str_starts(df$Street, "River") ~ 4L,
      TRUE                                    ~ 5L
    )
    
    # --- EV calculation for bets/raises
    df <- df %>%
      group_by(HandNumber) %>%
      arrange(StreetOrder, .by_group = TRUE) %>%
      mutate(
        Contribution = pmax(0, BetAmount),
        PotBefore    = cumsum(lag(Contribution, default = 0)),
        CostToCall   = if_else(Action %in% c("call", "raise"), BetAmount, 0),
        PotOdds      = if_else(CostToCall > 0, CostToCall / (PotBefore + CostToCall), 0),
        EV_raise     = if_else(Action == "raise", (Equity - 0.5) * BetAmount, NA_real_)
      ) %>%
      ungroup()
    
    # Filter only bet/raise for summary
    at <- df %>% filter(Action %in% c("bet","raise"))
    
    if (nrow(at) == 0) {
      return(tibble(
        Player       = character(),
        avg_equity   = double(),
        avg_odds     = double(),
        avg_EV_raise = double(),
        bluff_rate   = double(),
        n_actions    = integer(),
        n_hands      = integer(),
        vol_frac     = double()
      ))
    }
    
    # 16-1) Aggregate bet/raise metrics per player
    raw <- at %>%
      group_by(Player) %>%
      summarise(
        avg_equity   = mean(Equity,     na.rm = TRUE),
        avg_odds     = mean(WagerOdds,  na.rm = TRUE),
        avg_EV_raise = mean(EV_raise,   na.rm = TRUE),
        bluff_rate   = mean(bluff_prob, na.rm = TRUE),
        n_actions    = n(),
        .groups      = "drop"
      )
    
    # 16-2) How many distinct hands did each player sit in?
    hands <- df %>%
      distinct(HandNumber, Player) %>%
      count(Player, name = "n_hands")
    
    # 16-3) Join & compute volume fraction
    raw %>%
      left_join(hands, by = "Player") %>%
      mutate(
        vol_frac = if_else(n_hands > 0, n_actions / n_hands, 0)
      )
  })
  
  # 17) ── OPTIMALITY RANKING (optimality_summary) 
  optimality_summary <- reactive({
    player_summary() %>%
      # compute raw EV‐edge and keep bluff_rate & vol_frac
      mutate(
        ev_edge    = avg_equity - avg_odds,
        # bonus factor: reward bluffs when EV is +, penalize when EV is –
        bonus      = if_else(
          ev_edge > 0,
          1 + bluff_rate,    # more bluffs → bigger boost
          1 - bluff_rate     # bluffs on –EV hands → hurt your score
        ),
        # final Optimality score uses vol_frac instead of n_actions
        optimality = ev_edge * vol_frac * bonus
      ) %>%
      arrange(desc(optimality))
  })
  
  
  
  # 18) ── DROPDOWN: PLAYER FOCUS (“Summary” + sorted roster) 
  observe({
    ps <- player_summary()
    cols <- if (nrow(ps) > 0) c("Summary", sort(ps$Player)) else "Summary"
    updateSelectInput(
      session,
      "focus_player",
      choices  = cols,
      selected = "Summary"
    )
  })
  
  
  observe({
    ps <- player_summary()
    updateSelectInput(
      session,
      "focus_player",
      choices  = c("Summary", sort(ps$Player)),
      selected = "Summary"
    )
  })
  
  # 19) ── PLAYER AGGRESSION PROFILE PLOT
output$player_profile <- renderPlot({
  ps <- player_summary() %>%
    filter(!is.infinite(avg_odds), !is.infinite(avg_equity))
  req(nrow(ps) > 0)
  
  ## 19-1) Filter out infinities that break the scale
  ggplot(ps, aes(
    x     = avg_odds,
    y     = avg_equity,
    size  = n_actions,
    color = bluff_rate
  )) +
    
    ## 19-2) Bubble chart: Equity vs. Pot-Odds
    geom_abline(
      slope     = 1,
      intercept = 0,
      linetype  = "dashed",
      color     = "grey50",
      size      = 0.5
    ) +
    geom_point(alpha = 0.6) +
    geom_text_repel(
      aes(label = Player),
      size         = 4,
      fontface     = "bold",
      segment.size = 0.3,
      box.padding  = 0.3,
      point.padding= 0.5,
      show.legend  = FALSE
    ) +
    
    ## 19-3) Scales & aesthetics
    scale_size_continuous(range = c(4, 12), name = "# Bets & Raises") +
    scale_color_gradient(low = "skyblue", high = "firebrick", name = "Bluff Rate") +
    labs(
      title = "Player Aggression Profile",
      x     = "Avg. Pot Odds",
      y     = "Avg. Equity"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      legend.box      = "horizontal"
    ) +
    # force both axes to include zero
    expand_limits(x = 0, y = 0)
})

# 20) ── HTML BULLET SUMMARY (“Who’s ahead / behind?”)
  output$profile_summary <- renderUI({
    library(dplyr)
    
    ps <- player_summary() %>%
      mutate(
        ev_score = avg_equity - avg_odds,
        eq_fmt   = sprintf("%.2f", avg_equity),
        od_fmt   = sprintf("%.2f", avg_odds)
      ) %>%
      arrange(desc(ev_score))
    req(nrow(ps) > 0)
    
    # Split into +EV and −EV groups for bullet wording
    positives <- ps %>% filter(ev_score > 0)
    negatives <- ps %>% filter(ev_score <= 0)
    
    bullets <- character()
    
    ## 20-1) Call-out the top +EV player, if any 
    if (nrow(positives) > 0) {
      top <- positives[1, ]
      bullets <- c(bullets,
                   paste0(
                     "<li><b>", top$Player, "</b> is furthest above the line (equity ≈ ",
                     top$eq_fmt, " vs odds ≈ ", top$od_fmt,
                     "), so they’re making the most +EV plays.</li>"
                   )
      )
    }
    
    ## 20-2) List everyone on / below the line 
    if (nrow(negatives) > 0) {
      neg_txt <- negatives %>%
        transmute(
          entry = paste0(
            "<b>", Player, "</b> (≈ ", eq_fmt, " vs ", od_fmt, ")"
          )
        ) %>%
        pull(entry)
      
      bullets <- c(bullets,
                   paste0(
                     "<li>",
                     paste(neg_txt, collapse = ", "),
                     if (nrow(positives) > 0) {
                       " are below the line, so on average their calls/raises are –EV."
                     } else {
                       " are all below the line, so on average their calls/raises are –EV."
                     },
                     "</li>"
                   )
      )
    }
    
    # Build the final “In short…” sentence
    if (nrow(positives) > 0) {
      summary <- paste0(
        "<p><b>In short:</b> ", positives$Player[1],
        " is playing most “safe”. ",
        
        # this if() must return *one* string, so we wrap all of its pieces in paste0():
        if (nrow(negatives) > 0) {
          paste0(
            " You’d want to target ",
            paste0("<b>", negatives$Player, "</b>", collapse = ", "),
            " at the table."
          )
        } else {
          ""
        }
      )
    } else {
      # everyone is -EV
      least_neg <- ps[1, ]
      summary <- paste0(
        "<p><b>In short:</b> No one is +EV; the least −EV player is <b>",
        least_neg$Player,
        "</b>, but everyone is below the line."
      )
    }
    
    HTML(
      paste0(
        "<ul style='padding-left:1em;'>",
        paste(bullets, collapse = ""),
        "</ul>",
        summary
      )
    )
  })
 
  # 21) ── EV-TREND PLOT FOR A SINGLE PLAYER 
  output$ev_trend <- renderPlot({
    df <- df_with_ev(rv()) %>%
      filter(Player == input$focus_player,
             Action %in% c("bet", "call", "raise")) %>%
      mutate(ev_score = coalesce(EV_call, EV_bet, EV_raise))
    
    req(nrow(df) > 0)
    max_abs <- max(abs(df$ev_score), na.rm = TRUE)
    
    ggplot(df, aes(x = HandNumber, y = ev_score, color = Action)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 5, alpha = 0.8) +
      scale_x_continuous(breaks = unique(df$HandNumber)) +
      scale_y_continuous(limits = c(-max_abs, max_abs)) +
      labs(
        title = paste(input$focus_player, "EV over Hands"),
        x     = "Hand Number",
        y     = "EV (per action)",
        color = "Action"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  # 22) ── OPTIMALITY TABLE (renderTable)
  output$opt_table <- renderTable({
    optimality_summary() %>%
      # round our key metrics
      mutate(
        ev_edge      = round(ev_edge, 2),
        bluff_rate   = round(bluff_rate, 2),
        vol_frac     = round(vol_frac, 2)
      ) %>%
      transmute(
        Player,
        `EV Edge`        = sprintf("%.2f", ev_edge),
        `Bet/Raise Per Hand` = sprintf("%.2f", vol_frac),
        `Bluff Rate`     = sprintf("%.2f", bluff_rate),
        Optimality       = sprintf("%.1f", optimality)
      )
  }, striped = TRUE, hover = TRUE, rownames = FALSE)
  
  
  # 23) ── HEADLINE SUMMARY OF OPTIMALITY RANKING
  output$opt_summary <- renderUI({
    summary_df <- optimality_summary()
    top    <- summary_df[1, ]
    bottom <- summary_df[nrow(summary_df), ]
    HTML(paste0(
      "<p><b>Top performer:</b> ", top$Player, 
      " (EV edge ≈ ", sprintf("%.2f", top$ev_edge),
      ", bluff rate ≈ ", sprintf("%.2f", top$bluff_rate),
      ", vol_frac ≈ ", sprintf("%.2f", top$vol_frac),
      ") → score ", sprintf("%.1f", top$optimality), ".</p>",
      "<p><b>Needs work:</b> ", bottom$Player,
      " (EV edge ≈ ", sprintf("%.2f", bottom$ev_edge),
      ", bluff rate ≈ ", sprintf("%.2f", bottom$bluff_rate),
      ", vol_frac ≈ ", sprintf("%.2f", bottom$vol_frac),
      ") → score ", sprintf("%.1f", bottom$optimality), ".</p>"
    ))
  })
  
  
}

shinyApp(ui, server)
