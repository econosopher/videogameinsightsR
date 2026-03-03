devtools::load_all(".")

HELLDIVERS_STEAM_ID <- 553850
pass_count <- 0
fail_count <- 0
warn_count <- 0

record <- function(status, msg = "") {
  if (status == "PASS") {
    pass_count <<- pass_count + 1
    cat("  PASS\n\n")
  } else if (status == "WARN") {
    warn_count <<- warn_count + 1
    cat("  WARNING:", msg, "\n\n")
  } else {
    fail_count <<- fail_count + 1
    cat("  FAIL:", msg, "\n\n")
  }
}

cat("=== HELLDIVERS 2 LIVE INTEGRATION TESTS ===\n")
cat("Steam App ID:", HELLDIVERS_STEAM_ID, "\n\n")

# TEST 1: Game Metadata
cat("--- TEST 1: vgi_game_metadata ---\n")
meta <- tryCatch(vgi_game_metadata(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(meta, "error")) {
  record("FAIL", meta$message)
} else if (is.data.frame(meta) && nrow(meta) > 0) {
  cat("  Name:", as.character(meta$name), "\n")
  cat("  Price:", meta$price, "\n")
  cat("  Release:", as.character(meta$releaseDate), "\n")
  cat("  Publisher:", as.character(meta$publisherName), "\n")
  cat("  Developer:", as.character(meta$developerName), "\n")
  record("PASS")
} else {
  record("FAIL", "empty data frame returned")
}

# TEST 2: DAU/MAU (full history)
cat("--- TEST 2: vgi_insights_dau_mau ---\n")
dau_mau <- tryCatch(vgi_insights_dau_mau(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(dau_mau, "error")) {
  record("FAIL", dau_mau$message)
} else {
  ph <- dau_mau$playerHistory
  cat("  playerHistory rows:", nrow(ph), "\n")
  if (nrow(ph) > 0) {
    non_zero <- ph[ph$dau > 0 | ph$mau > 0, , drop = FALSE]
    cat("  Non-zero rows:", nrow(non_zero), "\n")
    if (nrow(non_zero) > 0) {
      latest <- non_zero[nrow(non_zero), , drop = FALSE]
      cat("  Latest non-zero: date=", as.character(latest$date), " dau=", latest$dau, " mau=", latest$mau, "\n")
    }
    record("PASS")
  } else {
    record("WARN", "0 rows")
  }
}

# TEST 3: Active Players by Date (filtered to Helldivers)
cat("--- TEST 3: vgi_active_players_by_date (2025-01-15) ---\n")
active <- tryCatch(
  vgi_active_players_by_date("2025-01-15", steam_app_ids = HELLDIVERS_STEAM_ID),
  error = function(e) e
)
if (inherits(active, "error")) {
  record("FAIL", active$message)
} else if (nrow(active) > 0) {
  cat("  Rows:", nrow(active), "\n")
  cat("  DAU:", active$dau[1], " MAU:", active$mau[1], "\n")
  record("PASS")
} else {
  record("WARN", "0 rows")
}

# TEST 4: Price History (USD)
cat("--- TEST 4: vgi_insights_price_history (USD) ---\n")
price_usd <- tryCatch(vgi_insights_price_history(HELLDIVERS_STEAM_ID, currency = "USD"), error = function(e) e)
if (inherits(price_usd, "error")) {
  record("FAIL", price_usd$message)
} else {
  cat("  priceChanges rows:", nrow(price_usd$priceChanges), "\n")
  if (nrow(price_usd$priceChanges) > 0) {
    cat("  Current price: $", price_usd$priceChanges$priceFinal[1], "\n")
    record("PASS")
  } else {
    record("WARN", "no price changes found")
  }
}

# TEST 5: Price History (no currency filter)
cat("--- TEST 5: vgi_insights_price_history (all) ---\n")
price_all <- tryCatch(vgi_insights_price_history(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(price_all, "error")) {
  record("FAIL", price_all$message)
} else {
  cat("  priceChanges rows:", nrow(price_all$priceChanges), "\n")
  record("PASS")
}

# TEST 6: Historical Data (comprehensive)
cat("--- TEST 6: vgi_historical_data ---\n")
hist_data <- tryCatch(vgi_historical_data(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(hist_data, "error")) {
  record("FAIL", hist_data$message)
} else {
  for (nm in c("revenue", "unitsSold", "concurrentPlayers", "activePlayers",
               "reviews", "wishlists", "followers", "priceHistory")) {
    cmp <- hist_data[[nm]]
    if (is.data.frame(cmp)) {
      cat(" ", nm, ":", nrow(cmp), "rows\n")
    } else {
      cat(" ", nm, ": NULL\n")
    }
  }
  has_data <- sum(sapply(c("revenue", "unitsSold", "concurrentPlayers", "activePlayers"),
                          function(x) !is.null(hist_data[[x]])))
  if (has_data > 0) record("PASS") else record("FAIL", "all components NULL")
}

# TEST 7: Concurrent Players by Date
cat("--- TEST 7: vgi_concurrent_players_by_date ---\n")
ccu <- tryCatch(
  vgi_concurrent_players_by_date(date = "2025-01-15", steam_app_ids = HELLDIVERS_STEAM_ID),
  error = function(e) e
)
if (inherits(ccu, "error")) {
  record("FAIL", ccu$message)
} else if (nrow(ccu) > 0) {
  cat("  Rows:", nrow(ccu), "\n")
  cat("  Peak:", ccu$peakConcurrent[1], " Avg:", ccu$avgConcurrent[1], "\n")
  record("PASS")
} else {
  record("WARN", "0 rows")
}

# TEST 8: Revenue by Date
cat("--- TEST 8: vgi_revenue_by_date ---\n")
rev_data <- tryCatch(
  vgi_revenue_by_date(date = "2025-01-15", steam_app_ids = HELLDIVERS_STEAM_ID),
  error = function(e) e
)
if (inherits(rev_data, "error")) {
  record("FAIL", rev_data$message)
} else if (nrow(rev_data) > 0) {
  cat("  Revenue total:", rev_data$revenue[1], "\n")
  cat("  Daily revenue:", rev_data$dailyRevenue[1], "\n")
  record("PASS")
} else {
  record("WARN", "0 rows")
}

# TEST 9: Units Sold by Date
cat("--- TEST 9: vgi_units_sold_by_date ---\n")
units <- tryCatch(
  vgi_units_sold_by_date(date = "2025-01-15", steam_app_ids = HELLDIVERS_STEAM_ID),
  error = function(e) e
)
if (inherits(units, "error")) {
  record("FAIL", units$message)
} else if (nrow(units) > 0) {
  cat("  Units sold total:", units$unitsSold[1], "\n")
  record("PASS")
} else {
  record("WARN", "0 rows")
}

# TEST 10: Playtime
cat("--- TEST 10: vgi_insights_playtime ---\n")
playtime <- tryCatch(vgi_insights_playtime(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(playtime, "error")) {
  record("FAIL", playtime$message)
} else {
  cat("  Avg playtime:", playtime$avgPlaytime, "min\n")
  cat("  Median playtime:", playtime$medianPlaytime, "min\n")
  cat("  Rank:", playtime$avgPlaytimeRank, "\n")
  cat("  Ranges:", nrow(playtime$playtimeRanges), "bins\n")
  record("PASS")
}

# TEST 11: Player Regions
cat("--- TEST 11: vgi_insights_player_regions ---\n")
regions <- tryCatch(vgi_insights_player_regions(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(regions, "error")) {
  record("FAIL", regions$message)
} else {
  cat("  Regions:", nrow(regions$regions), "\n")
  if (nrow(regions$regions) > 0) {
    cat("  Top region:", regions$regions$regionName[1], "(", regions$regions$percentage[1], "%)\n")
  }
  record("PASS")
}

# TEST 12: Top Countries
cat("--- TEST 12: vgi_top_countries ---\n")
countries <- tryCatch(vgi_top_countries(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(countries, "error")) {
  record("FAIL", countries$message)
} else {
  cat("  Countries:", nrow(countries), "\n")
  if (nrow(countries) > 0) {
    cat("  Top:", countries$countryName[1], "(", countries$percentage[1], "%)\n")
  }
  record("PASS")
}

# TEST 13: Player Overlap
cat("--- TEST 13: vgi_player_overlap ---\n")
overlap <- tryCatch(vgi_player_overlap(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(overlap, "error")) {
  record("FAIL", overlap$message)
} else {
  cat("  Overlaps:", nrow(overlap$playerOverlaps), "\n")
  record("PASS")
}

# TEST 14: Entitlements
cat("--- TEST 14: vgi_insights_entitlements ---\n")
entitlements <- tryCatch(vgi_insights_entitlements(HELLDIVERS_STEAM_ID), error = function(e) e)
if (inherits(entitlements, "error")) {
  record("FAIL", entitlements$message)
} else {
  cat("  Rows:", nrow(entitlements), "\n")
  if (nrow(entitlements) > 0) {
    cat("  Latest total:", entitlements$entitlementsTotal[nrow(entitlements)], "\n")
  }
  record("PASS")
}

# TEST 15: Wishlists by Date
cat("--- TEST 15: vgi_wishlists_by_date ---\n")
wl <- tryCatch(
  vgi_wishlists_by_date(date = "2025-01-15", steam_app_ids = HELLDIVERS_STEAM_ID),
  error = function(e) e
)
if (inherits(wl, "error")) {
  record("FAIL", wl$message)
} else if (nrow(wl) > 0) {
  cat("  Wishlists:", wl$wishlists[1], "\n")
  record("PASS")
} else {
  record("WARN", "0 rows")
}

cat("\n=== RESULTS ===\n")
cat("PASS:", pass_count, " WARN:", warn_count, " FAIL:", fail_count, "\n")
cat("Total:", pass_count + warn_count + fail_count, "\n")
if (fail_count == 0) cat("ALL TESTS PASSED\n")
