# Fantasy Football Analytics Dashboard (MVP)

An R Shiny + flexdashboard app for exploring NFL player and team fantasy
production, built on the [nflverse](https://nflverse.nflverse.com/)
ecosystem (`nflreadr` for data, `nflplotR` for team logos/colors).

## Files

| File | Purpose |
|---|---|
| `dashboard.Rmd` | The app itself — flexdashboard YAML header with `runtime: shiny`, four tabs, and a persistent sidebar. |
| `global.R` | Loads packages, pulls & caches nflverse data, and defines the fantasy-scoring and team-color/logo helper functions. Sourced by `dashboard.Rmd`. |
| `styles.css` | Dark/modern visual polish layered on top of the `darkly` bootswatch theme. |
| `cache/` | Created automatically on first run — `.rds` snapshots of nflverse pulls so the app doesn't re-download every session. |

## Running it locally

```r
install.packages(c(
  "nflreadr", "nflplotR", "flexdashboard", "shiny", "bslib",
  "DT", "gt", "plotly", "dplyr", "tidyr", "scales", "purrr"
))

rmarkdown::run("dashboard.Rmd")
```

`rmarkdown::run()` (not `rmarkdown::render()`) is required because the
dashboard uses `runtime: shiny` — it's a live app, not a static document.

The first run will pull the most recent completed season's player stats,
rosters, schedules, and team metadata from nflverse's data releases and
cache them as `.rds` files in `cache/`. Subsequent launches reuse the
cache (see **Refreshing data** below) so startup is fast.

## Data sources

All data comes from `nflreadr`, which reads nflverse's published data
releases (no scraping, no API keys required):

- **`nflreadr::load_player_stats()`** — weekly per-player offensive stat
  lines (passing/rushing/receiving yards & TDs, receptions, targets,
  interceptions, fumbles lost, etc.). This is the source for everything
  scored into fantasy points.
- **`nflreadr::load_rosters()`** — player-to-team mapping and biographical
  metadata for the selected season.
- **`nflreadr::load_schedules()`** — game-by-game matchups, used to look
  up each player's weekly opponent (for the game-log chart's opponent
  logos) and to compute "points allowed" on the Team Comparison tab.
- **`nflreadr::load_teams()`** — team abbreviations, primary/secondary
  colors, and logo URLs, used throughout for team-colored charts and
  `nflplotR`-rendered logos.

### Caching

`global.R`'s `load_cached()` helper checks `cache/<name>_<season>.rds`
before hitting nflverse. Player/roster/schedule pulls are considered
fresh for 3 days (stats update during the season); team metadata is
cached for 30 days since it rarely changes. To force a full refresh,
either delete the `cache/` folder or reduce `max_age_days` in
`global.R`.

## How fantasy scoring works

`global.R` defines three scoring presets (`SCORING_PRESETS`), selectable
from the sidebar:

| Stat | Standard | Half PPR | Full PPR |
|---|---|---|---|
| Passing yards | 1 pt / 25 yds | same | same |
| Passing TD | 4 | 4 | 4 |
| Interception | −2 | −2 | −2 |
| Rushing yards | 1 pt / 10 yds | same | same |
| Rushing TD | 6 | 6 | 6 |
| Receiving yards | 1 pt / 10 yds | same | same |
| Receiving TD | 6 | 6 | 6 |
| Reception | 0 | 0.5 | 1 |
| Fumble lost | −2 | −2 | −2 |

`compute_fantasy_points()` applies whichever preset is selected to every
row returned by `load_player_stats()` for the chosen season/week range,
producing a `fantasy_points` column that every tab reads from. Presets
are plain named lists, so adding a custom scoring scheme is a matter of
adding another entry to `SCORING_PRESETS` in `global.R`.

## Tabs

- **Overview** — value boxes (top scorer, biggest riser across the
  selected week range, weeks in view, players tracked) plus a `gt` table
  of the top 15 performers with team logos rendered inline.
- **Player Explorer** — a searchable/filterable `DT` table of all players
  matching the position/team filters; clicking a row populates a `plotly`
  weekly game-log chart with the opponent's team logo plotted at each
  point.
- **Team Comparison** — pick two or more teams and compare total fantasy
  points scored vs. points allowed to opponents, with team logos as the
  axis labels (via `nflplotR::element_nfl_logo()`) and bars colored in
  each team's primary color.
- **Custom View Builder** — pick a metric, a grouping (team / position /
  player), a chart type (bar/line/scatter), and an optional position
  filter; the resulting `plotly` chart renders live. **Save current
  view** stores the metric/grouping/chart/filter combination in a
  `reactiveValues` list for the rest of the session, and the dropdown
  that appears lets you flip back to any saved view (session-only —
  nothing persists after the app is closed, which is fine for this MVP).

## Known MVP limitations / next steps

- Data currently scopes to one season at a time (current or two prior,
  selectable in the sidebar) — no cross-season historical trends yet, by
  design per the MVP scope.
- Saved custom views are session-only (`reactiveValues`), not written to
  disk or a database — reopening the app clears them. A simple next step
  would be `saveRDS`-ing `saved_views$views` to a per-user file.
- Player headshots aren't wired up (team logos are); `nflreadr` roster
  data includes headshot URLs if that's wanted next.
- The opponent-logo overlay on the Player Explorer game-log chart uses
  plotly's `images` layout (rather than converting an `nflplotR` ggplot
  via `ggplotly()`), since logo grobs don't always survive that
  conversion cleanly — worth revisiting if a fully ggplot-driven pipeline
  is preferred later.
