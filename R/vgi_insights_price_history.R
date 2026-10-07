#' Get Price History Data for a Game
#'
#' Retrieve the Steam price-change periods for a game. By default this reads
#' the v4 `/price-history` endpoint (Steam rows only); `version = "v3"` reads
#' the v3 `/commercial-performance/price-history/games/{steamAppId}[/{currency}]`
#' endpoints. For PlayStation / Xbox prices and VGI-id or slug lookup see
#' [vgi_price_history()].
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param currency Character. Optional ISO currency code (e.g. "USD", "EUR").
#'   When omitted, price periods for every currency are returned.
#' @param version API generation, `"v4"` (default) or `"v3"`. USD history
#'   goes back to the end of 2014 and other currencies to 2022-04-14 in both.
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
                                       headers = list(),
                                       version = c("v4", "v3")) {

  validate_numeric(steam_app_id, "steam_app_id")
  if (!is.null(currency) && (!is.character(currency) || nchar(currency) == 0)) {
    stop("currency must be a non-empty character string")
  }
  version <- .vgi_api_version(match.arg(version))

  if (version == "v4") {
    v4 <- vgi_price_history(steam_app_id = steam_app_id, currency = currency,
                            auth_token = auth_token, headers = headers)
    v4 <- v4[v4$platform == "steam", , drop = FALSE]
    changes <- tibble::tibble(
      currency = v4$currency, priceInitial = v4$price_initial, priceFinal = v4$price_final,
      firstDate = v4$first_date, lastDate = v4$last_date
    )
    changes <- .vgi_sort_price_changes(changes)
  } else {
    endpoint <- sprintf("commercial-performance/price-history/games/%s", as.integer(steam_app_id))
    if (!is.null(currency)) endpoint <- paste0(endpoint, "/", toupper(currency))
    result <- make_api_request(
      endpoint = endpoint,
      auth_token = auth_token,
      method = "GET",
      headers = headers,
      version = "v3"
    )
    changes <- .vgi_price_changes_from_v3(result, currency)
  }

  .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    currency = if (is.null(currency)) "ALL" else toupper(currency),
    priceChanges = changes
  ))
}

.vgi_empty_price_changes <- function() {
  tibble::tibble(
    currency = character(), priceInitial = numeric(), priceFinal = numeric(),
    firstDate = as.Date(character()), lastDate = as.Date(character())
  )
}

.vgi_price_change_rows <- function(df, currency) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  tibble::tibble(
    currency = as.character(currency),
    priceInitial = as.numeric(.vgi_col(df, "priceInitial")),
    priceFinal = as.numeric(.vgi_col(df, "priceFinal")),
    firstDate = as.Date(.vgi_col(df, "firstDate", NA_character_)),
    lastDate = as.Date(.vgi_col(df, "lastDate", NA_character_))
  )
}

# Normalise either v3 price-history response shape into one long tibble.
.vgi_price_changes_from_v3 <- function(result, currency = NULL) {
  out <- .vgi_empty_price_changes()
  if (!is.list(result)) return(out)
  if (!is.null(currency) || "priceChanges" %in% names(result)) {
    rows <- .vgi_price_change_rows(result$priceChanges, result$currency %||% currency %||% NA_character_)
    if (!is.null(rows)) out <- rows
  } else if (is.data.frame(result$price) && nrow(result$price) > 0) {
    per_currency <- lapply(seq_len(nrow(result$price)), function(i) {
      .vgi_price_change_rows(result$price$priceChanges[[i]], result$price$currency[i])
    })
    per_currency <- per_currency[!vapply(per_currency, is.null, logical(1))]
    if (length(per_currency) > 0) out <- dplyr::bind_rows(per_currency)
  }
  .vgi_sort_price_changes(out)
}

# Currency A-Z, newest period first within a currency.
.vgi_sort_price_changes <- function(df) {
  if (nrow(df) == 0) return(df)
  df[order(df$currency, df$firstDate, decreasing = c(FALSE, TRUE), method = "radix"), , drop = FALSE]
}
