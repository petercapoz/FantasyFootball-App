# ==============================================================================
# global.R
# Fantasy Football Analytics — RB/WR-focused platform
#
# Loads packages, pulls & caches nflverse datasets, builds a metric
# dictionary (directly-sourced vs. calculated vs. unavailable), and defines
# the advanced-metrics/opportunity-score layer used by fantasy_analytics.Rmd.
#
# DATA SOURCE NOTES (read before adding new metrics):
#   - nflreadr::load_player_stats() is the base offensive box score. It
#     already includes target_share, air_yards_share, racr, wopr, dakota,
#     receiving_epa, rushing_epa, receiving_air_yards, receiving_first_downs,
#     rushing_first_downs, receiving_yards_after_catch, carries, headshot_url.
#     These are DIRECTLY SOURCED — nothing is invented here.
#   - nflreadr::load_nextgen_stats(stat_type = "receiving"/"rushing") adds
#     NFL Next Gen Stats: avg_separation, avg_cushion, avg_intended_air_yards,
#     percent_share_of_intended_air_yards, avg_yac, avg_expected_yac,
#     catch_percentage (receiving); efficiency, avg_time_to_los,
#     percent_attempts_gte_eight_defenders, rush_yards_over_expected,
#     rush_yards_over_expected_per_att (rushing). Joined on player_gsis_id.
#   - nflreadr::load_snap_counts() adds Pro-Football-Reference snap share
#     (offense_pct). Joined via the gsis/pfr crosswalk in
#     nflreadr::load_players().
#   - nflreadr::load_pfr_advstats(stat_type = "rush"/"rec") adds PFR's
#     advanced box (yards before/after contact, broken tackles, PFR's own
#     aDOT, drop rate). Also joined via the pfr_id crosswalk.
#   - ROUTES RUN / YARDS PER ROUTE RUN / TARGETS PER ROUTE RUN: nflverse does
#     NOT publish a clean player-week "routes run" column (that lives behind
#     PFF's paid charting product). We do NOT approximate or invent this —
#     see METRIC_DICTIONARY below, where these are explicitly flagged
#     `source = "unavailable"` with a note, per spec. Snap share and targets
#     are offered as separate, clearly-labeled alternatives — never silently
#     substituted.
#   - SEPARATION / CONTESTED TARGETS: avg_separation (NGS) is directly
#     sourced. Play-level "contested target" charting is not in the loaded
#     datasets, so it's flagged unavailable rather than approximated.
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
  library(scales)
})

# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

CURRENT_SEASON <- tryCatch(
  nflreadr::most_recent_season(roster = FALSE),
  error = function(e) as.integer(format(Sys.Date(), "%Y")) - 1
)

CACHE_DIR <- "data_cache"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

cache_path <- function(name, season) file.path(CACHE_DIR, paste0(name, "_", season, ".rds"))

load_cached <- function(name, season, loader_fn, max_age_days = 3) {
  path <- cache_path(name, season)
  if (file.exists(path)) {
    age_days <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
    if (age_days <= max_age_days) return(readRDS(path))
  }
  message(sprintf("Fetching %s for %s from nflverse...", name, season))
  data <- tryCatch(loader_fn(), error = function(e) {
    message(sprintf("  -> failed (%s); returning empty tibble", conditionMessage(e)))
    tibble()
  })
  saveRDS(data, path)
  data
}

# safe division: NA (never Inf/-Inf/NaN) when the denominator is zero/NA
safe_div <- function(num, denom) {
  out <- ifelse(is.na(denom) | denom == 0, NA_real_, num / denom)
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  out
}

# formats a numeric column for display, turning NA into "N/A"
fmt_or_na <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A", format(round(x, digits), nsmall = digits))
}

# Renames only the columns that actually exist in `df`. `mapping` is a named
# character vector, names = new column name, values = source column name
# (same shape dplyr::rename() wants). Any pair whose source column is
# missing is silently dropped instead of erroring — this is what makes the
# PFR/NGS joins below tolerant of a source table having fewer columns than
# expected (e.g. a PFR column name that's drifted between nflreadr
# versions), instead of taking the whole app down.
safe_rename <- function(df, mapping) {
  mapping <- mapping[mapping %in% names(df)]
  if (length(mapping) > 0) df <- dplyr::rename(df, !!!mapping)
  df
}

# ------------------------------------------------------------------------------
# Raw data loaders (cached, one file per dataset per season)
# ------------------------------------------------------------------------------

get_player_stats <- function(season = CURRENT_SEASON) {
  load_cached("player_stats", season, function() {
    nflreadr::load_player_stats(seasons = season, stat_type = "offense")
  })
}

get_rosters <- function(season = CURRENT_SEASON) {
  load_cached("rosters", season, function() nflreadr::load_rosters(seasons = season))
}

get_schedules <- function(season = CURRENT_SEASON) {
  load_cached("schedules", season, function() nflreadr::load_schedules(seasons = season))
}

get_teams <- function() {
  load_cached("teams", "static", function() nflreadr::load_teams(current = TRUE), max_age_days = 30)
}

get_ngs_receiving <- function(season = CURRENT_SEASON) {
  load_cached("ngs_receiving", season, function() {
    nflreadr::load_nextgen_stats(seasons = season, stat_type = "receiving")
  })
}

get_ngs_rushing <- function(season = CURRENT_SEASON) {
  load_cached("ngs_rushing", season, function() {
    nflreadr::load_nextgen_stats(seasons = season, stat_type = "rushing")
  })
}

get_snap_counts <- function(season = CURRENT_SEASON) {
  load_cached("snap_counts", season, function() nflreadr::load_snap_counts(seasons = season))
}

get_pfr_rush <- function(season = CURRENT_SEASON) {
  load_cached("pfr_rush", season, function() {
    nflreadr::load_pfr_advstats(seasons = season, stat_type = "rush", summary_level = "week")
  })
}

get_pfr_rec <- function(season = CURRENT_SEASON) {
  load_cached("pfr_rec", season, function() {
    nflreadr::load_pfr_advstats(seasons = season, stat_type = "rec", summary_level = "week")
  })
}

# Static gsis_id <-> pfr_id crosswalk (also has espn_id, etc.), used to join
# PFR-sourced and snap-count data (keyed on pfr_id) back onto the gsis-keyed
# player_stats table.
get_player_crosswalk <- function() {
  load_cached("player_crosswalk", "static", function() nflreadr::load_players(), max_age_days = 30)
}

# ------------------------------------------------------------------------------
# Fantasy scoring (unchanged from MVP — still applies to whatever slice of
# scored_stats() the rest of the app is looking at)
# ------------------------------------------------------------------------------

SCORING_PRESETS <- list(
  "Standard"  = list(pass_yd = 1/25, pass_td = 4, interception = -2, rush_yd = 1/10, rush_td = 6,
                      rec_yd = 1/10, rec_td = 6, reception = 0,   fumble_lost = -2),
  "Half PPR"  = list(pass_yd = 1/25, pass_td = 4, interception = -2, rush_yd = 1/10, rush_td = 6,
                      rec_yd = 1/10, rec_td = 6, reception = 0.5, fumble_lost = -2),
  "Full PPR"  = list(pass_yd = 1/25, pass_td = 4, interception = -2, rush_yd = 1/10, rush_td = 6,
                      rec_yd = 1/10, rec_td = 6, reception = 1,   fumble_lost = -2)
)

compute_fantasy_points <- function(df, scoring = SCORING_PRESETS[["Standard"]]) {
  safe_col <- function(name) if (name %in% names(df)) df[[name]] else 0
  fumbles_lost <- safe_col("sack_fumbles_lost") + safe_col("rushing_fumbles_lost") + safe_col("receiving_fumbles_lost")
  df %>%
    mutate(fantasy_points = round(
      safe_col("passing_yards")   * scoring$pass_yd +
      safe_col("passing_tds")     * scoring$pass_td +
      safe_col("interceptions")   * scoring$interception +
      safe_col("rushing_yards")   * scoring$rush_yd +
      safe_col("rushing_tds")     * scoring$rush_td +
      safe_col("receiving_yards") * scoring$rec_yd +
      safe_col("receiving_tds")   * scoring$rec_td +
      safe_col("receptions")      * scoring$reception +
      fumbles_lost                * scoring$fumble_lost, 2))
}

# ------------------------------------------------------------------------------
# Advanced metrics layer
# ------------------------------------------------------------------------------
# build_advanced_player_week(season) returns ONE row per player-week with:
#   - every column from load_player_stats() (offense), fantasy-scored
#   - NGS receiving/rushing columns, left-joined on player_gsis_id + week
#   - snap counts (offense_pct), left-joined via the pfr_id crosswalk
#   - PFR rush/rec advanced columns, left-joined via the pfr_id crosswalk
#   - calculated ratio metrics (see METRIC_DICTIONARY "calculated" rows)
# Any column that isn't found in its source table is simply absent (NA) for
# that row rather than fabricated — downstream UI checks `%in% names(df)`
# before offering a metric.

build_advanced_player_week <- function(season, scoring = SCORING_PRESETS[["Standard"]]) {

  base <- get_player_stats(season) %>%
    filter(season_type == "REG" | is.na(season_type)) %>%
    compute_fantasy_points(scoring)

  # load_player_stats() names the team column `recent_team`, not `team` —
  # normalize it here once so every downstream tab/join can just use `team`.
  if ("recent_team" %in% names(base) && !"team" %in% names(base)) {
    base <- base %>% mutate(team = recent_team)
  }

  crosswalk <- get_player_crosswalk() %>%
    select(any_of(c("gsis_id", "pfr_id"))) %>%
    filter(!is.na(gsis_id), !is.na(pfr_id)) %>%
    distinct(gsis_id, .keep_all = TRUE)

  # --- NGS receiving/rushing (already keyed by gsis id under player_gsis_id)
  ngs_rec <- get_ngs_receiving(season)
  if (nrow(ngs_rec) > 0 && "player_gsis_id" %in% names(ngs_rec)) {
    ngs_rec <- ngs_rec %>%
      filter(week > 0) %>%  # week == 0 rows are season-to-date summaries
      select(any_of(c(
        "player_gsis_id", "week", "avg_cushion", "avg_separation",
        "avg_intended_air_yards", "percent_share_of_intended_air_yards",
        "avg_yac", "avg_expected_yac", "avg_yac_above_expectation",
        "catch_percentage"
      ))) %>%
      safe_rename(c(gsis_id = "player_gsis_id"))
  } else {
    ngs_rec <- tibble(gsis_id = character(), week = integer())
  }

  ngs_rush <- get_ngs_rushing(season)
  if (nrow(ngs_rush) > 0 && "player_gsis_id" %in% names(ngs_rush)) {
    ngs_rush <- ngs_rush %>%
      filter(week > 0) %>%
      select(any_of(c(
        "player_gsis_id", "week", "efficiency", "avg_time_to_los",
        "percent_attempts_gte_eight_defenders", "rush_yards_over_expected",
        "rush_yards_over_expected_per_att", "rush_pct_over_expected"
      ))) %>%
      safe_rename(c(gsis_id = "player_gsis_id"))
  } else {
    ngs_rush <- tibble(gsis_id = character(), week = integer())
  }

  # --- Snap counts (keyed on pfr_player_id) -> crosswalk to gsis_id
  snaps <- get_snap_counts(season)
  if (nrow(snaps) > 0 && "pfr_player_id" %in% names(snaps)) {
    snaps <- snaps %>%
      select(any_of(c("pfr_player_id", "week", "offense_snaps", "offense_pct"))) %>%
      safe_rename(c(pfr_id = "pfr_player_id")) %>%
      left_join(crosswalk, by = "pfr_id") %>%
      filter(!is.na(gsis_id)) %>%
      select(-pfr_id)
  } else {
    snaps <- tibble(gsis_id = character(), week = integer())
  }

  # --- PFR advanced rushing (yards before/after contact, broken tackles)
  pfr_rush <- get_pfr_rush(season)
  if (nrow(pfr_rush) > 0 && "pfr_id" %in% names(pfr_rush)) {
    pfr_rush <- pfr_rush %>%
      select(any_of(c(
        "pfr_id", "week", "ybc", "ybc_att", "yac", "yac_att", "brk_tkl", "att_br"
      ))) %>%
      safe_rename(c(rush_ybc = "ybc", rush_ybc_att = "ybc_att", rush_yac = "yac",
                    rush_yac_att = "yac_att", rush_brk_tkl = "brk_tkl",
                    rush_att_per_brk_tkl = "att_br")) %>%
      left_join(crosswalk, by = "pfr_id") %>%
      filter(!is.na(gsis_id)) %>%
      select(-pfr_id)
  } else {
    pfr_rush <- tibble(gsis_id = character(), week = integer())
  }

  # --- PFR advanced receiving (aDOT, yards before/after catch, broken tackles, drops)
  pfr_rec <- get_pfr_rec(season)
  if (nrow(pfr_rec) > 0 && "pfr_id" %in% names(pfr_rec)) {
    pfr_rec <- pfr_rec %>%
      select(any_of(c(
        "pfr_id", "week", "ybc", "ybc_r", "yac", "yac_r", "adot",
        "brk_tkl", "rec_br", "drop", "drop_percent"
      ))) %>%
      safe_rename(c(rec_ybc = "ybc", rec_ybc_per_rec = "ybc_r", rec_yac = "yac",
                    rec_yac_per_rec = "yac_r", pfr_adot = "adot", rec_brk_tkl = "brk_tkl",
                    rec_per_brk_tkl = "rec_br", drops = "drop", drop_pct = "drop_percent")) %>%
      left_join(crosswalk, by = "pfr_id") %>%
      filter(!is.na(gsis_id)) %>%
      select(-pfr_id)
  } else {
    pfr_rec <- tibble(gsis_id = character(), week = integer())
  }

  out <- base %>%
    rename(gsis_id = player_id) %>%
    left_join(ngs_rec,  by = c("gsis_id", "week")) %>%
    left_join(ngs_rush, by = c("gsis_id", "week")) %>%
    left_join(snaps,    by = c("gsis_id", "week")) %>%
    left_join(pfr_rush, by = c("gsis_id", "week")) %>%
    left_join(pfr_rec,  by = c("gsis_id", "week")) %>%
    rename(player_id = gsis_id)

  # --- Calculated ratio metrics (NA-safe; never Inf/NaN)
  out <- out %>%
    mutate(
      yards_per_carry        = safe_div(rushing_yards, carries),
      yards_per_reception    = safe_div(receiving_yards, receptions),
      catch_rate_calc        = safe_div(receptions, targets),
      adot_calc              = safe_div(receiving_air_yards, targets),
      epa_per_target          = safe_div(receiving_epa, targets),
      epa_per_rush             = safe_div(rushing_epa, carries),
      fantasy_points_per_target = safe_div(fantasy_points, targets),
      fantasy_points_per_opportunity = safe_div(fantasy_points, carries + targets),
      opportunities           = carries + targets,
      rush_ybc_per_att        = safe_div(rush_ybc, carries),
      rush_yac_per_att        = safe_div(rush_yac, carries)
    )

  out
}

# Season-to-date aggregation of the player-week table above, for KPI cards,
# rankings, and any "season total" view. Ratio metrics are recomputed from
# summed numerators/denominators (not averaged week-to-week) so they stay
# mathematically consistent; NGS/PFR rate columns (already per-game rates)
# are averaged instead, weighted equally by game.
build_advanced_player_season <- function(season, scoring = SCORING_PRESETS[["Standard"]],
                                          week_min = 1, week_max = 18) {
  pw <- build_advanced_player_week(season, scoring) %>%
    filter(week >= week_min, week <= week_max)

  pw %>%
    group_by(player_id, player_display_name, position, position_group,
              team, headshot_url) %>%
    summarise(
      games = n_distinct(week),
      fantasy_points   = sum(fantasy_points, na.rm = TRUE),
      carries          = sum(carries, na.rm = TRUE),
      rushing_yards    = sum(rushing_yards, na.rm = TRUE),
      rushing_tds      = sum(rushing_tds, na.rm = TRUE),
      rushing_epa      = sum(rushing_epa, na.rm = TRUE),
      rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE),
      targets          = sum(targets, na.rm = TRUE),
      receptions       = sum(receptions, na.rm = TRUE),
      receiving_yards  = sum(receiving_yards, na.rm = TRUE),
      receiving_tds    = sum(receiving_tds, na.rm = TRUE),
      receiving_epa    = sum(receiving_epa, na.rm = TRUE),
      receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE),
      receiving_first_downs = sum(receiving_first_downs, na.rm = TRUE),
      target_share     = mean(target_share, na.rm = TRUE),
      air_yards_share  = mean(air_yards_share, na.rm = TRUE),
      wopr             = mean(wopr, na.rm = TRUE),
      racr             = mean(racr, na.rm = TRUE),
      avg_separation   = mean(avg_separation, na.rm = TRUE),
      avg_cushion      = mean(avg_cushion, na.rm = TRUE),
      avg_intended_air_yards = mean(avg_intended_air_yards, na.rm = TRUE),
      avg_yac          = mean(avg_yac, na.rm = TRUE),
      avg_expected_yac = mean(avg_expected_yac, na.rm = TRUE),
      offense_pct      = mean(offense_pct, na.rm = TRUE),
      rush_ybc         = sum(rush_ybc, na.rm = TRUE),
      rush_yac         = sum(rush_yac, na.rm = TRUE),
      rush_brk_tkl     = sum(rush_brk_tkl, na.rm = TRUE),
      rec_brk_tkl      = sum(rec_brk_tkl, na.rm = TRUE),
      pfr_adot         = mean(pfr_adot, na.rm = TRUE),
      drops            = sum(drops, na.rm = TRUE),
      efficiency       = mean(efficiency, na.rm = TRUE),
      rush_yards_over_expected = sum(rush_yards_over_expected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x))) %>%
    mutate(
      opportunities            = carries + targets,
      yards_per_carry          = safe_div(rushing_yards, carries),
      yards_per_reception      = safe_div(receiving_yards, receptions),
      catch_rate               = safe_div(receptions, targets),
      adot_calc                = safe_div(receiving_air_yards, targets),
      epa_per_target             = safe_div(receiving_epa, targets),
      epa_per_rush               = safe_div(rushing_epa, carries),
      fantasy_points_per_game     = safe_div(fantasy_points, games),
      fantasy_points_per_target   = safe_div(fantasy_points, targets),
      fantasy_points_per_carry    = safe_div(fantasy_points, carries),
      fantasy_points_per_opportunity = safe_div(fantasy_points, opportunities),
      rush_ybc_per_att          = safe_div(rush_ybc, carries),
      rush_yac_per_att          = safe_div(rush_yac, carries)
    )
}

# ------------------------------------------------------------------------------
# Opportunity scores (transparent, component-based — never a black box)
# ------------------------------------------------------------------------------
# Both scores are 0-100, min-max scaled *within the currently filtered player
# pool* (so they're relative to who's being compared, e.g. "RBs with >=20
# opportunities this season"), and are a simple, disclosed weighted average
# of the components below. The component values themselves are always shown
# alongside the score in the UI — this is a scoring rubric, not a model.

minmax01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) return(rep(NA_real_, length(x)))
  (x - rng[1]) / diff(rng)
}

# Components (all directly sourced or calculated, each individually
# inspectable in the UI):
#   rushing attempt share (carries as a share of the pool's max carries),
#   target share (direct from player_stats),
#   red-zone/goal-line usage -> NOT separately available at player-week
#     granularity in the loaded datasets (would require play-by-play
#     filtering by yardline, which this MVP's cached tables don't include),
#     so it is OMITTED from the score rather than approximated, and the UI
#     labels the weight breakdown accordingly.
#   snap share (offense_pct, from PFR snap counts)
RB_OPPORTUNITY_WEIGHTS <- c(carry_share = 0.40, target_share = 0.25, snap_share = 0.35)
WR_OPPORTUNITY_WEIGHTS <- c(target_share = 0.45, air_yards_share = 0.30, snap_share = 0.25)

compute_rb_opportunity_score <- function(df) {
  df %>%
    mutate(
      .carry_share = minmax01(carries),
      .target_share = minmax01(coalesce(target_share, 0)),
      .snap_share = minmax01(coalesce(offense_pct, NA_real_)),
      rb_opportunity_score = round(100 * (
        RB_OPPORTUNITY_WEIGHTS["carry_share"]  * coalesce(.carry_share, 0) +
        RB_OPPORTUNITY_WEIGHTS["target_share"] * coalesce(.target_share, 0) +
        RB_OPPORTUNITY_WEIGHTS["snap_share"]   * coalesce(.snap_share, 0)
      ), 1)
    ) %>%
    select(-.carry_share, -.target_share, -.snap_share)
}

compute_wr_opportunity_score <- function(df) {
  df %>%
    mutate(
      .target_share = minmax01(coalesce(target_share, 0)),
      .air_yards_share = minmax01(coalesce(air_yards_share, 0)),
      .snap_share = minmax01(coalesce(offense_pct, NA_real_)),
      wr_opportunity_score = round(100 * (
        WR_OPPORTUNITY_WEIGHTS["target_share"]    * coalesce(.target_share, 0) +
        WR_OPPORTUNITY_WEIGHTS["air_yards_share"] * coalesce(.air_yards_share, 0) +
        WR_OPPORTUNITY_WEIGHTS["snap_share"]      * coalesce(.snap_share, 0)
      ), 1)
    ) %>%
    select(-.target_share, -.air_yards_share, -.snap_share)
}

# ------------------------------------------------------------------------------
# Metric dictionary — the single source of truth the UI reads from to build
# every dropdown. `source` is one of "direct", "calculated", "unavailable".
# Friendly `label` is what the UI shows; `column` is the real field name in
# build_advanced_player_week()/build_advanced_player_season(). Metrics with
# source == "unavailable" are never offered in a chart dropdown — they exist
# in this table purely so the Metric Definitions / Advanced Metrics tab can
# tell the user honestly what nflverse does not provide here, instead of
# silently omitting them.
# ------------------------------------------------------------------------------

METRIC_DICTIONARY <- tribble(
  ~key,                          ~label,                              ~category,           ~source,       ~column,                      ~positions,   ~note,
  "fantasy_points",              "Fantasy Points",                    "Fantasy",            "calculated",  "fantasy_points",             "ALL",        "From the selected scoring preset.",
  "fantasy_points_per_target",   "Fantasy Points / Target",           "Fantasy",            "calculated",  "fantasy_points_per_target",  "RB,WR,TE",   "fantasy_points / targets.",
  "fantasy_points_per_carry",    "Fantasy Points / Carry",            "Fantasy",            "calculated",  "fantasy_points_per_carry",   "RB",         "fantasy_points / carries.",
  "fantasy_points_per_opportunity","Fantasy Points / Opportunity",    "Fantasy",            "calculated",  "fantasy_points_per_opportunity","RB,WR",   "fantasy_points / (carries + targets).",
  "fantasy_points_per_route",    "Fantasy Points / Route",            "Fantasy",            "unavailable", NA_character_,                 "RB,WR,TE",   "Requires routes run — not published by nflverse (see Routes Run note).",
  "carries",                     "Carries",                           "Volume",             "direct",      "carries",                    "RB",         "load_player_stats().",
  "targets",                     "Targets",                           "Volume",             "direct",      "targets",                    "RB,WR,TE",   "load_player_stats().",
  "receptions",                  "Receptions",                        "Volume",             "direct",      "receptions",                 "RB,WR,TE",   "load_player_stats().",
  "opportunities",               "Opportunities (Carries+Targets)",   "Volume",             "calculated",  "opportunities",              "RB,WR",      "carries + targets.",
  "offense_pct",                 "Snap Share",                        "Volume",             "direct",      "offense_pct",                "ALL",        "PFR snap counts via load_snap_counts(), joined by pfr_id.",
  "target_share",                "Target Share",                      "Volume",             "direct",      "target_share",               "RB,WR,TE",   "load_player_stats().",
  "air_yards_share",             "Air Yard Share",                    "Volume",             "direct",      "air_yards_share",            "WR,TE",      "load_player_stats().",
  "routes_run",                  "Routes Run",                        "Volume",             "unavailable", NA_character_,                 "RB,WR,TE",   "Not published in the loaded nflverse tables (PFF-charted, not open data). Use Snap Share as the closest available volume proxy — shown separately, never substituted silently.",
  "targets_per_route_run",       "Targets Per Route Run",             "Efficiency",         "unavailable", NA_character_,                 "RB,WR,TE",   "Depends on Routes Run (unavailable).",
  "receptions_per_route_run",    "Receptions Per Route Run",          "Efficiency",         "unavailable", NA_character_,                 "RB,WR,TE",   "Depends on Routes Run (unavailable).",
  "yards_per_route_run",         "Yards Per Route Run",               "Efficiency",         "unavailable", NA_character_,                 "RB,WR,TE",   "Depends on Routes Run (unavailable).",
  "yards_per_carry",             "Yards Per Carry",                   "Efficiency",         "calculated",  "yards_per_carry",            "RB",         "rushing_yards / carries.",
  "yards_per_reception",         "Yards Per Reception",               "Efficiency",         "calculated",  "yards_per_reception",        "RB,WR,TE",   "receiving_yards / receptions.",
  "catch_rate",                  "Catch Rate",                        "Efficiency",         "calculated",  "catch_rate",                 "RB,WR,TE",   "receptions / targets.",
  "epa_per_rush",                 "EPA / Rush",                        "Efficiency",         "calculated",  "epa_per_rush",                "RB",         "rushing_epa / carries.",
  "epa_per_target",                "EPA / Target",                      "Efficiency",         "calculated",  "epa_per_target",              "RB,WR,TE",   "receiving_epa / targets.",
  "racr",                        "Receiver Air Conversion Ratio",     "Efficiency",         "direct",      "racr",                       "WR,TE",      "load_player_stats(); receiving_yards / receiving_air_yards.",
  "wopr",                        "Weighted Opportunity Rating",       "Efficiency",         "direct",      "wopr",                       "WR,TE",      "load_player_stats(); 1.5*target_share + 0.7*air_yards_share.",
  "rushing_yards",                "Rushing Yards",                    "Rushing Profile",    "direct",      "rushing_yards",              "RB",         "load_player_stats().",
  "rushing_tds",                  "Rushing TDs",                      "Rushing Profile",    "direct",      "rushing_tds",                "RB",         "load_player_stats().",
  "rushing_epa",                  "Rushing EPA",                       "Rushing Profile",    "direct",      "rushing_epa",                "RB",         "load_player_stats(): total EPA on rush attempts.",
  "rushing_first_downs",          "Rushing First Downs",               "Rushing Profile",    "direct",      "rushing_first_downs",        "RB",         "load_player_stats().",
  "efficiency",                   "NGS Rush Efficiency",               "Rushing Profile",    "direct",      "efficiency",                 "RB",         "load_nextgen_stats(stat_type='rushing'): distance traveled per yard gained.",
  "rush_yards_over_expected",      "Rush Yards Over Expected",          "Rushing Profile",    "direct",      "rush_yards_over_expected",   "RB",         "load_nextgen_stats(stat_type='rushing').",
  "rush_ybc_per_att",              "Yards Before Contact / Att",        "Rushing Profile",    "calculated",  "rush_ybc_per_att",           "RB",         "PFR advanced rushing (ybc) / carries.",
  "rush_yac_per_att",              "Yards After Contact / Att",         "Rushing Profile",    "calculated",  "rush_yac_per_att",           "RB",         "PFR advanced rushing (yac) / carries.",
  "rush_brk_tkl",                  "Broken Tackles (Rush)",             "Rushing Profile",    "direct",      "rush_brk_tkl",                "RB",         "load_pfr_advstats(stat_type='rush'), joined via pfr_id.",
  "receiving_air_yards",           "Air Yards",                         "Receiving Profile",  "direct",      "receiving_air_yards",         "WR,TE,RB",   "load_player_stats().",
  "adot_calc",                     "Average Depth of Target (calc.)",   "Receiving Profile",  "calculated",  "adot_calc",                   "WR,TE,RB",   "receiving_air_yards / targets.",
  "avg_intended_air_yards",        "Average Depth of Target (NGS)",     "Receiving Profile",  "direct",      "avg_intended_air_yards",      "WR,TE",      "load_nextgen_stats(stat_type='receiving').",
  "avg_separation",                "Separation",                        "Receiving Profile",  "direct",      "avg_separation",              "WR,TE",      "load_nextgen_stats(stat_type='receiving'): yards from nearest defender at catch/incompletion.",
  "avg_cushion",                   "Cushion",                            "Receiving Profile",  "direct",      "avg_cushion",                 "WR,TE",      "load_nextgen_stats(stat_type='receiving'): yards from defender at snap.",
  "avg_yac",                       "Yards After Catch (avg.)",          "Receiving Profile",  "direct",      "avg_yac",                     "WR,TE,RB",   "load_nextgen_stats(stat_type='receiving').",
  "contested_targets",             "Contested Targets",                 "Receiving Profile",  "unavailable", NA_character_,                 "WR,TE",      "Not present in the loaded PFR/NGS tables at player-week level.",
  "red_zone_targets",              "Red-Zone Targets",                  "Receiving Profile",  "unavailable", NA_character_,                 "WR,TE",      "Requires play-by-play yardline filtering, out of scope for this MVP's cached tables.",
  "drops",                         "Drops",                             "Receiving Profile",  "direct",      "drops",                       "WR,TE,RB",   "load_pfr_advstats(stat_type='rec'), joined via pfr_id.",
  "rec_brk_tkl",                   "Broken Tackles (Rec)",              "Receiving Profile",  "direct",      "rec_brk_tkl",                 "WR,TE,RB",   "load_pfr_advstats(stat_type='rec'), joined via pfr_id."
)

# Convenience: metrics actually safe to plot right now (direct + calculated),
# restricted to columns that exist in a given data frame.
plottable_metrics <- function(df) {
  avail <- METRIC_DICTIONARY %>% filter(source != "unavailable", column %in% names(df))
  setNames(avail$column, avail$label)
}

# ------------------------------------------------------------------------------
# Team color / logo helpers
# ------------------------------------------------------------------------------

team_lookup <- function(teams_df, abbr) teams_df %>% filter(team_abbr == abbr) %>% slice(1)

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
# App-wide dark theme
# ------------------------------------------------------------------------------

APP_BG        <- "#0f1420"
APP_PANEL_BG  <- "#161c2c"
APP_TEXT      <- "#e7ecf5"
APP_MUTED     <- "#8b93a7"
APP_ACCENT    <- "#3fa7d6"

app_theme <- bslib::bs_theme(
  version = 5, bg = APP_BG, fg = APP_TEXT, primary = APP_ACCENT, secondary = APP_MUTED,
  base_font = bslib::font_google("Inter"), heading_font = bslib::font_google("Inter", wght = 600),
  code_font = bslib::font_google("JetBrains Mono"),
  "border-radius" = "0.9rem", "card-bg" = APP_PANEL_BG, "body-color" = APP_TEXT
)

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

# ------------------------------------------------------------------------------
# Plotly scatter helper — ENFORCES markers-only so a scatter view can never
# accidentally render as a connected line (the bug this rebuild fixes).
# Always pass type="scatter", mode="markers" explicitly; never "lines" or
# "lines+markers" here. Line charts must go through plotly_time_series()
# below instead, which is the only helper allowed to use a line mode.
# ------------------------------------------------------------------------------

plotly_scatter <- function(data, x, y, color = NULL, hover_text) {
  # plotly's discrete color/legend mapping (color = ~var) builds a named
  # vector internally and jsonlite emits a harmless future-deprecation
  # NOTE ("Input to asJSON(keep_vec_names=TRUE) is a named vector...") every
  # time. It doesn't affect the rendered chart — suppressed here so it
  # doesn't spam the console on every scatter render.
  suppressWarnings(plot_ly(
    data = data, x = as.formula(paste0("~", x)), y = as.formula(paste0("~", y)),
    type = "scatter", mode = "markers",
    color = if (!is.null(color)) as.formula(paste0("~", color)) else NULL,
    text = hover_text, hoverinfo = "text",
    marker = list(size = 10, opacity = 0.85, line = list(width = 1, color = APP_PANEL_BG))
  ))
}

plotly_time_series <- function(data, x, y, hover_text) {
  suppressWarnings(plot_ly(
    data = data, x = as.formula(paste0("~", x)), y = as.formula(paste0("~", y)),
    type = "scatter", mode = "lines+markers",
    line = list(color = APP_ACCENT), marker = list(color = APP_ACCENT, size = 6),
    text = hover_text, hoverinfo = "text"
  ))
}

plotly_dark_layout <- function(p, xtitle = NULL, ytitle = NULL, images = NULL) {
  p %>% layout(
    paper_bgcolor = APP_PANEL_BG, plot_bgcolor = APP_PANEL_BG, font = list(color = APP_TEXT),
    xaxis = list(title = xtitle, gridcolor = "#242c40"),
    yaxis = list(title = ytitle, gridcolor = "#242c40"),
    images = images
  )
}