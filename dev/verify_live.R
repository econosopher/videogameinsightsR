# Live verification against the real API for a known title (Dressmaker,
# Steam 4019220). Requires VGI_AUTH_TOKEN in the environment. Prints a short,
# non-secret summary of v3 and v4 results fetched through the package.
suppressMessages(devtools::load_all(quiet = TRUE))
options(vgi.request_cache_ttl = 0)
id <- 4019220
fmt <- function(x) format(x, big.mark = ",")
last <- function(df, col) df[[col]][max(which(!is.na(df[[col]])))]
last_date <- function(df, col) df$date[max(which(!is.na(df[[col]])))]

meta <- vgi_game_metadata(id)
cat(sprintf("v3 metadata: %s | released %s | price $%.2f | publisher %s (%d) | %d steam tags\n",
            meta$name, meta$release_date, meta$price, meta$publisher_name, meta$publisher_id,
            length(meta$steam_tags[[1]])))
rk <- vgi_game_rankings(steam_app_id = id)
cat(sprintf("v3 rankings: revenue rank %d | units rank %d | reviews rank %d\n",
            rk$total_revenue_rank, rk$total_units_sold_rank, rk$positive_reviews_rank))
for (v in c("v3", "v4")) {
  u <- vgi_insights_units(id, version = v)
  r <- vgi_insights_revenue(id, version = v)
  ccu <- vgi_insights_ccu(id, version = v)$player_history
  w <- vgi_insights_wishlists(id, version = v)$wishlist_changes
  rv <- vgi_insights_reviews(id, version = v)
  cat(sprintf("%s units: %d rows (%s..%s) | total %s on %s\n", v, nrow(u), min(u$date), max(u$date),
              fmt(last(u, "units_sold_total")), last_date(u, "units_sold_total")))
  cat(sprintf("%s revenue: total $%s on %s\n", v, fmt(last(r, "revenue_total")), last_date(r, "revenue_total")))
  cat(sprintf("%s ccu: %d rows | peak %s on %s | duplicate dates %d\n", v, nrow(ccu),
              fmt(max(ccu$max, na.rm = TRUE)), ccu$date[which.max(ccu$max)], sum(duplicated(ccu$date))))
  cat(sprintf("%s wishlists: latest total %s on %s\n", v, fmt(last(w, "wishlists_total")), last_date(w, "wishlists_total")))
  cat(sprintf("%s reviews: %s positive / %s negative on %s\n", v, fmt(last(rv, "positive")),
              fmt(last(rv, "negative")), last_date(rv, "positive")))
}
o3 <- vgi_player_overlap(id, limit = 3)$player_overlaps
cat(sprintf("v3 overlap: top %s\n", paste(sprintf("%d (%.1f%% units)", o3$steam_app_id, o3$units_sold_overlap_percentage), collapse = ", ")))
o3all <- vgi_player_overlap(id, limit = 5000)$player_overlaps
cat(sprintf("v3 overlap rows (limit 5000): %d\n", nrow(o3all)))
g <- vgi_games_metadata(steam_app_ids = id)
cat(sprintf("v4 metadata: vgi_id %d | slug %s | platforms %s | price_steam %.2f | steam_app_id %d\n",
            g$vgi_id, g$slug, paste(g$platforms[[1]], collapse = ","), g$price_steam, g$steam_app_id))
o4 <- vgi_player_overlap(slug = "dressmaker", limit = 3)
cat(sprintf("v4 overlap: steam_app_id %d | vgi_id %d | %d informative overlap rows\n",
            o4$steam_app_id, o4$vgi_id, nrow(o4$player_overlaps)))
s <- vgi_historical_data_by_date("2026-09-28", steam_app_ids = id)
cat(sprintf("v4 snapshot 2026-09-28: ccu_max %d | dau %d | wishlists %s\n", s$ccu_max, s$dau, fmt(s$wishlists_total)))
