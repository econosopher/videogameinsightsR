#' Get Price History Data for a Game
#'
#' Retrieve the Steam price-change periods for a game: the Steam rows of
#' [vgi_price_history()], which also covers PlayStation and Xbox prices and
#' VGI-id or slug lookup.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param currency Character. Optional ISO currency code (e.g. "USD", "EUR").
#'   When omitted, price periods for every currency are returned.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{currency}{Character. The requested currency, or "ALL"}
#'   \item{price_changes}{Tibble with columns `currency`, `price_initial`,
#'     `price_final`, `first_date`, `last_date` (NA for the current period),
#'     newest period first}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' usd <- vgi_insights_price_history(4019220, currency = "USD")
#' usd$price_changes
#'
#' all_prices <- vgi_insights_price_history(4019220)
#' table(all_prices$price_changes$currency)
#' }
vgi_insights_price_history <- function(steam_app_id,
                                       currency = NULL,
                                       auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                       headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")
  if (!is.null(currency) && (!is.character(currency) || nchar(currency) == 0)) {
    stop("currency must be a non-empty character string")
  }

  v4 <- vgi_price_history(steam_app_id = steam_app_id, currency = currency,
                          auth_token = auth_token, headers = headers)
  v4 <- v4[v4$platform == "steam", , drop = FALSE]
  changes <- tibble::tibble(
    currency = v4$currency, priceInitial = v4$price_initial, priceFinal = v4$price_final,
    firstDate = v4$first_date, lastDate = v4$last_date
  )
  changes <- .vgi_sort_price_changes(changes)

  .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    currency = if (is.null(currency)) "ALL" else toupper(currency),
    priceChanges = changes
  ))
}

# Currency A-Z, newest period first within a currency.
.vgi_sort_price_changes <- function(df) {
  if (nrow(df) == 0) return(df)
  df[order(df$currency, df$firstDate, decreasing = c(FALSE, TRUE), method = "radix"), , drop = FALSE]
}
