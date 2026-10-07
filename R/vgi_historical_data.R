#' Get Full Historical Data for a Game
#'
#' Retrieve the complete daily history for a single Steam game and split it
#' into tidy per-metric time series.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) reads the v4
#'   `/historical-data` endpoint filtered to the game, which covers every day
#'   since the game was first tracked, world-wide. `"v3"` reads the per-game
#'   v3 `/historical-data/games/{steamAppId}` endpoint.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list with element `steam_app_id` plus one tibble (or `NULL` when
#'   the API has no data for that metric) for each of:
#' \describe{
#'   \item{revenue}{`date`, `revenue` (cumulative total), `daily_revenue`;
#'     v4 `premiumRevenue*` fields}
#'   \item{units_sold}{`date`, `units_sold` (cumulative total), `daily_units`;
#'     v4 `unitsOwned*` fields}
#'   \item{concurrent_players}{`date`, `ccu_avg`, `ccu_median`, `ccu_max`, `ccu_min`}
#'   \item{active_players}{`date`, `dau`, `mau`}
#'   \item{reviews}{`date`, `positive`, `negative`, `positive_change`, `negative_change`}
#'   \item{wishlists}{`date`, `wishlists`, `wishlists_change`}
#'   \item{followers}{`date`, `followers`, `followers_change`}
#'   \item{price_history}{`date`, `price_initial`, `price_final`}
#' }
#'   The `daily` element holds the raw flat daily table with every metric
#'   column returned by the API (snake_case).
#'
#' @details
#' For a single-date snapshot across many games (including the v4
#' multi-platform, country and region breakdowns) use
#' [vgi_historical_data_by_date()].
#'
#' @export
#' @examples
#' \dontrun{
#' hist <- vgi_historical_data(4019220)
#' hist$concurrent_players
#' hist$revenue
#' }
vgi_historical_data <- function(steam_app_id,
                               version = c("v4", "v3"),
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")

  version <- .vgi_api_version(match.arg(version))
  rows <- .vgi_game_series(steam_app_id, version, "historical-data/games/%s",
                           auth_token = auth_token, headers = headers)

  empty <- .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    revenue = NULL, unitsSold = NULL, concurrentPlayers = NULL,
    activePlayers = NULL, reviews = NULL, wishlists = NULL,
    followers = NULL, priceHistory = NULL, daily = NULL
  ))
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"date" %in% names(rows)) return(empty)

  rows <- rows[!is.na(rows$date), , drop = FALSE]
  rows <- rows[order(rows$date), , drop = FALSE]

  make_ts <- function(cols) {
    out <- tibble::tibble(date = as.Date(rows$date))
    for (nm in names(cols)) out[[nm]] <- as.numeric(.vgi_col(rows, cols[[nm]]))
    out
  }
  null_if_empty <- function(df) {
    if (nrow(df) == 0 || all(is.na(as.matrix(df[, -1, drop = FALSE])))) NULL else df
  }

  .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    revenue = null_if_empty(make_ts(list(revenue = c("premiumRevenueTotal", "revenueTotal"),
                                       dailyRevenue = c("premiumRevenueChange", "revenueChange")))),
    unitsSold = null_if_empty(make_ts(list(unitsSold = c("unitsOwnedTotal", "unitsSoldTotal"),
                                         dailyUnits = c("unitsOwnedChange", "unitsSoldChange")))),
    concurrentPlayers = null_if_empty(make_ts(list(ccuAvg = "ccuAvg", ccuMedian = "ccuMedian",
                                                   ccuMax = "ccuMax", ccuMin = "ccuMin"))),
    activePlayers = null_if_empty(make_ts(list(dau = "dau", mau = "mau"))),
    reviews = null_if_empty(make_ts(list(positive = "positiveReviewsTotal", negative = "negativeReviewsTotal",
                                         positiveChange = "positiveReviewsChange",
                                         negativeChange = "negativeReviewsChange"))),
    wishlists = null_if_empty(make_ts(list(wishlists = "wishlistsTotal", wishlistsChange = "wishlistsChange"))),
    followers = null_if_empty(make_ts(list(followers = "followersTotal", followersChange = "followersChange"))),
    priceHistory = null_if_empty(make_ts(list(priceInitial = "priceInitial", priceFinal = "priceFinal"))),
    daily = .vgi_clean_names(tibble::as_tibble(rows))
  ))
}
