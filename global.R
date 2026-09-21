# ==============================================================================
# global.R
# Fantasy Football Analytics Dashboard — MVP
# Loads packages, pulls/caches nflverse data, and defines shared helper
# functions (fantasy scoring, formatting, team color lookups) used by
# dashboard.Rmd.
# ==============================================================================

suppressPackageStartupMessages({
  library(nflreadr)
  library(nflplotR)
  library(flexdashboard)
  library(shiny)
  library(bslib)
  library(DT)
  library(gt)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

# Most recently completed season. nflreadr exposes a helper for "most recent
# season" that accounts for whether the current season has finished; fall
# back to (current year - 1) if that's unavailable for some reason.
CURRENT_SEASON <- tryCatch(
  nflreadr::most_recent_season(roster = FALSE),
  error = function(e) as.integer(format(Sys.Date(), "%Y")) - 1
)

CACHE_DIR <- "cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

cache_path <- function(name, season) {
  file.path(CACHE_DIR, paste0(name, "_", season, ".rds"))
}

# Generic "load from cache, else pull from nflverse and cache it" helper.
# max_age_days lets you force a refresh periodically (stats update weekly
# during the season); for an MVP we default to a long TTL so the app is fast
# after the first run, and the user can just delete the cache/ folder to
# force a full refresh.
load_cached <- function(name, season, loader_fn, max_age_days = 3) {
  path <- cache_path(name, season)
  if (file.exists(path)) {
    age_days <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
    if (age_days <= max_age_days) {
      return(readRDS(path))
    }
  }
  message(sprintf("Fetching %s for %s from nflverse...", name, season))
  data <- loader_fn()
  saveRDS(data, path)
  data
}

# ------------------------------------------------------------------------------
# Data loaders (cached)
# ------------------------------------------------------------------------------

get_player_stats <- function(season = CURRENT_SEASON) {
  load_cached("player_stats", season, function() {
    nflreadr::load_player_stats(seasons = season, stat_type = "offense")
  })
}

get_rosters <- function(season = CURRENT_SEASON) {
  load_cached("rosters", season, function() {
    nflreadr::load_rosters(seasons = season)
  })
}

get_schedules <- function(season = CURRENT_SEASON) {
  load_cached("schedules", season, function() {
    nflreadr::load_schedules(seasons = season)
  })
}

# Team metadata (colors, logos, abbreviations) doesn't change within a
# season and is tiny, so it's cached with a long TTL regardless of season.
get_teams <- function() {
  load_cached("teams", "static", function() {
    nflreadr::load_teams(current = TRUE)
  }, max_age_days = 30)
}

# ------------------------------------------------------------------------------
# Fantasy scoring
# ------------------------------------------------------------------------------
# Scoring presets. Each is a named list of per-unit point values. The Player
# Explorer / Custom View Builder let the user pick a preset or nudge
# individual weights (kept simple for the MVP: a PPR toggle + preset picker).

SCORING_PRESETS <- list(
  "Standard" = list(
    pass_yd = 1 / 25, pass_td = 4, interception = -2,
    rush_yd = 1 / 10, rush_td = 6,
    rec_yd = 1 / 10, rec_td = 6, reception = 0,
    fumble_lost = -2
  ),
  "Half PPR" = list(
    pass_yd = 1 / 25, pass_td = 4, interception = -2,
    rush_yd = 1 / 10, rush_td = 6,
    rec_yd = 1 / 10, rec_td = 6, reception = 0.5,
    fumble_lost = -2
  ),
  "Full PPR" = list(
    pass_yd = 1 / 25, pass_td = 4, interception = -2,
    rush_yd = 1 / 10, rush_td = 6,
    rec_yd = 1 / 10, rec_td = 6, reception = 1,
    fumble_lost = -2
  )
)

# Maps the scoring weights onto whatever columns load_player_stats() returns.
# nflreadr's offense stat columns typically include: passing_yards,
# passing_tds, interceptions, rushing_yards, rushing_tds, receiving_yards,
# receiving_tds, receptions, sack_fumbles_lost, rushing_fumbles_lost,
# receiving_fumbles_lost. We sum the fumbles-lost columns defensively with
# any() %in% names() checks so this still works if nflverse tweaks column
# names across seasons.
compute_fantasy_points <- function(df, scoring = SCORING_PRESETS[["Standard"]]) {
  safe_col <- function(name) if (name %in% names(df)) df[[name]] else 0

  fumbles_lost <- safe_col("sack_fumbles_lost") +
    safe_col("rushing_fumbles_lost") +
    safe_col("receiving_fumbles_lost")

  df %>%
    mutate(
      fantasy_points = round(
        safe_col("passing_yards")   * scoring$pass_yd +
        safe_col("passing_tds")     * scoring$pass_td +
        safe_col("interceptions")   * scoring$interception +
        safe_col("rushing_yards")   * scoring$rush_yd +
        safe_col("rushing_tds")     * scoring$rush_td +
        safe_col("receiving_yards") * scoring$rec_yd +
        safe_col("receiving_tds")   * scoring$rec_td +
        safe_col("receptions")      * scoring$reception +
        fumbles_lost                * scoring$fumble_lost,
        2
      )
    )
}

# ------------------------------------------------------------------------------
# Team color / logo helpers
# ------------------------------------------------------------------------------

team_lookup <- function(teams_df, abbr) {
  teams_df %>% filter(team_abbr == abbr) %>% slice(1)
}

team_primary_color <- function(teams_df, abbr, fallback = "#3fa7d6") {
  row <- team_lookup(teams_df, abbr)
  if (nrow(row) == 0 || is.na(row$team_color)) return(fallback)
  row$team_color
}

team_logo_url <- function(teams_df, abbr) {
  row <- team_lookup(teams_df, abbr)
  if (nrow(row) == 0) return(NA_character_)
  row$team_logo_espn
}

# ------------------------------------------------------------------------------
# App-wide dark theme (bslib), used inside the Rmd's YAML `theme:` field
# indirectly — this object is also reused for any dynamically-generated
# ggplot/gt styling so colors stay consistent across the app.
# ------------------------------------------------------------------------------

APP_BG        <- "#0f1420"
APP_PANEL_BG  <- "#161c2c"
APP_TEXT      <- "#e7ecf5"
APP_MUTED     <- "#8b93a7"
APP_ACCENT    <- "#3fa7d6"

app_theme <- bslib::bs_theme(
  version = 5,
  bg = APP_BG,
  fg = APP_TEXT,
  primary = APP_ACCENT,
  secondary = APP_MUTED,
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter", wght = 600),
  code_font = bslib::font_google("JetBrains Mono"),
  "border-radius" = "0.9rem",
  "card-bg" = APP_PANEL_BG,
  "body-color" = APP_TEXT
)

# Reusable ggplot theme so any static plots match the dark UI.
gg_dark_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = APP_PANEL_BG, color = NA),
      panel.background = ggplot2::element_rect(fill = APP_PANEL_BG, color = NA),
      panel.grid.major = ggplot2::element_line(color = "#242c40"),
      panel.grid.minor = ggplot2::element_blank(),
      text             = ggplot2::element_text(color = APP_TEXT),
      axis.text        = ggplot2::element_text(color = APP_MUTED),
      legend.background = ggplot2::element_rect(fill = APP_PANEL_BG, color = NA),
      legend.text      = ggplot2::element_text(color = APP_TEXT)
    )
}
