# Simple helpers: fetch peak CCU for multiple games in one call (by names or IDs)

#' Get peak concurrent players (CCU) for multiple Steam app IDs
#'
#' @param steam_app_ids Integer vector of Steam app IDs
#' @return A tibble with columns: name, steamAppId, peak_ccu, peak_date
#' @export
vgi_peak_ccu_by_ids <- function(steam_app_ids) {
  if (is.null(steam_app_ids) || length(steam_app_ids) == 0) {
    return(.vgi_clean_names(tibble::tibble(name = character(), steamAppId = integer(), peak_ccu = numeric(), peak_date = as.Date(character()))))
  }
  rows <- lapply(as.integer(steam_app_ids), function(id) {
    ts <- tryCatch(vgi_insights_ccu(id), error = function(e) NULL)
    nm <- tryCatch({
      meta <- vgi_game_metadata(id)
      if (is.data.frame(meta) && nrow(meta) > 0 && "name" %in% names(meta)) meta$name[1] else as.character(id)
    }, error = function(e) as.character(id))
    if (is.null(ts) || !is.data.frame(ts) || nrow(ts) == 0) {
      return(tibble::tibble(name = nm, steamAppId = id, peak_ccu = NA_real_, peak_date = as.Date(NA)))
    }
    value_col <- intersect(names(ts), c("ccu", "concurrent", "peakConcurrent"))
    date_col <- intersect(names(ts), c("date", "timestamp"))
    if (length(value_col) == 0 || length(date_col) == 0) {
      return(tibble::tibble(name = nm, steamAppId = id, peak_ccu = NA_real_, peak_date = as.Date(NA)))
    }
    vals <- suppressWarnings(as.numeric(ts[[value_col[1]]]))
    dates <- as.Date(ts[[date_col[1]]])
    idx <- which.max(vals)
    tibble::tibble(name = nm, steamAppId = id, peak_ccu = vals[idx], peak_date = dates[idx])
  })
  .vgi_clean_names(dplyr::bind_rows(rows))
}

#' Get peak concurrent players (CCU) for multiple games by names
#'
#' Resolves each game name to a Steam app ID using `vgi_search_games(title, limit = 5)`
#' with a simple exact/startsWith prioritization, then fetches peak CCU via
#' `vgi_insights_ccu()` per resolved ID. No all-games endpoints are used.
#'
#' @param game_names Character vector of game names (e.g., "THE FINALS", "Halo Infinite")
#' @return A tibble with columns: input_name, name, steamAppId, peak_ccu, peak_date
#' @export
vgi_peak_ccu_by_names <- function(game_names) {
  if (is.null(game_names) || length(game_names) == 0) {
    return(.vgi_clean_names(tibble::tibble(input_name = character(), name = character(), steamAppId = integer(), peak_ccu = numeric(), peak_date = as.Date(character()))))
  }
  resolve_one <- function(title) {
    res <- tryCatch(vgi_search_games(title, limit = 5), error = function(e) NULL)
    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) return(NA_integer_)
    res$score <- ifelse(tolower(res$name) == tolower(title), 2,
                   ifelse(startsWith(tolower(res$name), tolower(title)), 1, 0))
    res <- res[order(res$score, decreasing = TRUE), , drop = FALSE]
    as.integer(res$steamAppId[1] %||% res$id[1] %||% NA_integer_)
  }
  ids <- vapply(game_names, resolve_one, integer(1))
  peak_df <- vgi_peak_ccu_by_ids(ids)
  peak_df$input_name <- game_names
  .vgi_clean_names(peak_df[, c("input_name", "name", "steamAppId", "peak_ccu", "peak_date")])
}


