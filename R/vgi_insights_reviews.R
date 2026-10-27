#' Get Review History for a Game
#'
#' Retrieve the daily Steam review history for a single game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `steam_app_id`, `date`,
#'   `positive`, `negative` (cumulative totals), `total`, `positive_ratio`,
#'   `positive_change` and `negative_change` (reviews added that day).
#'
#' @export
#' @examples
#' \dontrun{
#' reviews <- vgi_insights_reviews(steam_app_id = 4019220)
#' tail(reviews[, c("date", "positive", "negative", "positive_ratio")])
#' }
vgi_insights_reviews <- function(steam_app_id,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")

  rows <- .vgi_v4_game_history(steam_app_id, auth_token = auth_token, headers = headers)
  if (nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(), date = as.Date(character()),
      positive = integer(), negative = integer(), total = integer(),
      positiveRatio = numeric(), positiveChange = integer(), negativeChange = integer()
    )))
  }

  positive <- as.integer(.vgi_col(rows, "positiveReviewsTotal"))
  negative <- as.integer(.vgi_col(rows, "negativeReviewsTotal"))
  total <- positive + negative
  out <- tibble::tibble(
    steamAppId = as.integer(steam_app_id),
    date = as.Date(rows$date),
    positive = positive,
    negative = negative,
    total = total,
    positiveRatio = ifelse(!is.na(total) & total > 0, positive / total, NA_real_),
    positiveChange = as.integer(.vgi_col(rows, "positiveReviewsChange")),
    negativeChange = as.integer(.vgi_col(rows, "negativeReviewsChange"))
  )
  .vgi_clean_names(out[order(out$date), , drop = FALSE])
}
