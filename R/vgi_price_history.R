#' Get Price History for a Game (v4, multi-platform)
#'
#' Retrieve price-change periods for a game from the v4 `/price-history`
#' endpoint. Exactly one of `steam_app_id`, `vgi_id` or `slug` identifies the
#' game. For the Steam-only v3 endpoint see [vgi_insights_price_history()].
#'
#' @param steam_app_id Integer. Steam App ID of the game.
#' @param vgi_id Integer. VGI internal game ID.
#' @param slug Character. VGI game slug (e.g. "dressmaker").
#' @param currency Character vector. Optional ISO currency codes to keep
#'   (filtering happens client-side; the API returns every currency).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per price period and columns
#'   `platform`, `external_id`, `currency`, `price_initial`, `price_final`,
#'   `first_date`, `last_date` (NA for the current period).
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_price_history(steam_app_id = 4019220, currency = "USD")
#' vgi_price_history(slug = "dressmaker")
#' }
vgi_price_history <- function(steam_app_id = NULL,
                              vgi_id = NULL,
                              slug = NULL,
                              currency = NULL,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  qp <- .vgi_single_game_query(steam_app_id, vgi_id, slug)

  rows <- make_api_request(
    endpoint = "price-history",
    query_params = qp,
    auth_token = auth_token,
    method = "GET",
    headers = headers,
    version = "v4"
  )

  empty <- .vgi_clean_names(tibble::tibble(
    platform = character(), externalId = character(), currency = character(),
    priceInitial = numeric(), priceFinal = numeric(),
    firstDate = as.Date(character()), lastDate = as.Date(character())
  ))
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"priceChanges" %in% names(rows)) return(empty)

  per_row <- lapply(seq_len(nrow(rows)), function(i) {
    changes <- rows$priceChanges[[i]]
    if (!is.data.frame(changes) || nrow(changes) == 0) return(NULL)
    tibble::tibble(
      platform = as.character(rows$platform[i] %||% NA_character_),
      externalId = as.character(rows$externalId[i] %||% NA_character_),
      currency = as.character(rows$currency[i] %||% NA_character_),
      priceInitial = as.numeric(.vgi_col(changes, "priceInitial")),
      priceFinal = as.numeric(.vgi_col(changes, "priceFinal")),
      firstDate = as.Date(.vgi_col(changes, "firstDate", NA_character_)),
      lastDate = as.Date(.vgi_col(changes, "lastDate", NA_character_))
    )
  })
  per_row <- per_row[!vapply(per_row, is.null, logical(1))]
  if (length(per_row) == 0) return(empty)
  out <- dplyr::bind_rows(per_row)
  if (!is.null(currency)) out <- out[out$currency %in% toupper(currency), , drop = FALSE]
  .vgi_clean_names(out)
}

# Query parameters for the v4 single-game endpoints (player-overlap, price-history).
.vgi_single_game_query <- function(steam_app_id = NULL, vgi_id = NULL, slug = NULL) {
  supplied <- c(!is.null(steam_app_id), !is.null(vgi_id), !is.null(slug))
  if (sum(supplied) != 1) {
    stop("Supply exactly one of steam_app_id, vgi_id or slug.")
  }
  if (!is.null(steam_app_id)) {
    validate_numeric(steam_app_id, "steam_app_id")
    return(list(steamAppId = as.character(as.integer(steam_app_id))))
  }
  if (!is.null(vgi_id)) {
    validate_numeric(vgi_id, "vgi_id")
    return(list(vgiId = as.character(as.integer(vgi_id))))
  }
  if (!is.character(slug) || length(slug) != 1 || !nzchar(slug)) {
    stop("slug must be a single non-empty character string")
  }
  list(slug = slug)
}
