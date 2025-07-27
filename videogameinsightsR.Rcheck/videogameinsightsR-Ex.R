pkgname <- "videogameinsightsR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "videogameinsightsR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('videogameinsightsR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("vgi_active_players_by_date")
### * vgi_active_players_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_active_players_by_date
### Title: Get Active Players Data by Date
### Aliases: vgi_active_players_by_date

### ** Examples

## Not run: 
##D # Get active player data
##D active_data <- vgi_active_players_by_date("2024-01-15")
##D 
##D # Top 20 games by DAU
##D top_dau <- head(active_data, 20)
##D cat("Top 20 games by daily active users:\n")
##D print(top_dau[, c("steamAppId", "dau", "mau", "dauMauRatio")])
##D 
##D # Find games with excellent retention (DAU/MAU > 25%)
##D high_retention <- active_data[active_data$dauMauRatio > 0.25 & 
##D                              active_data$dau > 1000, ]
##D cat("Games with >25% DAU/MAU ratio:", nrow(high_retention), "\n")
##D print(head(high_retention[order(-high_retention$dauMauRatio), ], 10))
##D 
##D # Compare with concurrent player data
##D ccu_data <- vgi_concurrent_players_by_date("2024-01-15")
##D engagement <- merge(active_data, ccu_data, by = "steamAppId")
##D 
##D # Calculate concurrent-to-DAU ratio (session intensity)
##D engagement$ccu_dau_ratio <- engagement$peakConcurrent / (engagement$dau + 1)
##D 
##D # Games with high session intensity (many concurrent per daily user)
##D high_intensity <- engagement[engagement$ccu_dau_ratio > 0.3 & 
##D                             engagement$dau > 1000, ]
##D cat("High session intensity games:", nrow(high_intensity), "\n")
##D 
##D # Analyze retention tiers
##D active_data$retention_tier <- cut(active_data$dauMauRatio,
##D                                   breaks = c(0, 0.1, 0.2, 0.3, 0.5, 1),
##D                                   labels = c("Poor", "Below Avg", "Good", 
##D                                             "Excellent", "Outstanding"))
##D 
##D retention_summary <- table(active_data$retention_tier[active_data$dau > 100])
##D barplot(retention_summary,
##D         main = "Games by Retention Tier (DAU > 100)",
##D         xlab = "Retention Tier",
##D         ylab = "Number of Games",
##D         col = rainbow(5))
##D 
##D # Monthly trend analysis
##D month_ago <- as.Date("2024-01-15") - 30
##D active_prev <- vgi_active_players_by_date(as.character(month_ago))
##D 
##D trend <- merge(active_data, active_prev,
##D               by = "steamAppId",
##D               suffixes = c("_now", "_prev"))
##D 
##D # Calculate monthly growth
##D trend$dau_growth <- ((trend$dau_now - trend$dau_prev) / 
##D                     (trend$dau_prev + 1)) * 100
##D trend$mau_growth <- ((trend$mau_now - trend$mau_prev) / 
##D                     (trend$mau_prev + 1)) * 100
##D 
##D # Find rapidly growing games
##D growing <- trend[trend$dau_growth > 50 & trend$dau_now > 1000, ]
##D cat("Games with >50% DAU growth:", nrow(growing), "\n")
##D 
##D # Identify games losing players
##D declining <- trend[trend$mau_growth < -20 & trend$mau_prev > 5000, ]
##D cat("Games losing >20% MAU:", nrow(declining), "\n")
##D print(head(declining[order(declining$mau_growth), 
##D            c("steamAppId", "mau_prev", "mau_now", "mau_growth")]))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_active_players_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_developer_games")
### * vgi_all_developer_games

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_developer_games
### Title: Get All Developer Game IDs
### Aliases: vgi_all_developer_games

### ** Examples

## Not run: 
##D # Get all developer game mappings
##D dev_games <- vgi_all_developer_games()
##D 
##D # Find most prolific developers
##D top_devs <- head(dev_games[order(-dev_games$gameCount), ], 20)
##D cat("Top 20 most prolific developers:\n")
##D print(top_devs[, c("developerId", "gameCount")])
##D 
##D # Get developer names for context
##D dev_list <- vgi_developer_list()
##D top_devs_named <- merge(top_devs, dev_list, 
##D                        by.x = "developerId", by.y = "id")
##D print(top_devs_named[, c("name", "gameCount")])
##D 
##D # Analyze developer portfolio sizes
##D hist(dev_games$gameCount[dev_games$gameCount <= 50],
##D      breaks = 50,
##D      main = "Distribution of Developer Portfolio Sizes",
##D      xlab = "Number of Games",
##D      col = "lightblue")
##D 
##D # Find single-game developers
##D single_game_devs <- dev_games[dev_games$gameCount == 1, ]
##D cat("Developers with only one game:", nrow(single_game_devs), "\n")
##D cat("Percentage of single-game developers:", 
##D     round(nrow(single_game_devs) / nrow(dev_games) * 100, 1), "%\n")
##D 
##D # Analyze specific developer's portfolio
##D valve_id <- 8  # Example: Valve's ID
##D valve_games <- dev_games$gameIds[dev_games$developerId == valve_id][[1]]
##D if (length(valve_games) > 0) {
##D   cat("Valve has", length(valve_games), "games\n")
##D   
##D   # Get metadata for all Valve games
##D   valve_metadata <- vgi_game_metadata_batch(valve_games)
##D   print(valve_metadata[, c("name", "releaseDate")])
##D }
##D 
##D # Find developers with similar portfolio sizes
##D target_size <- 10
##D similar_devs <- dev_games[dev_games$gameCount >= target_size - 2 & 
##D                          dev_games$gameCount <= target_size + 2, ]
##D cat("Developers with 8-12 games:", nrow(similar_devs), "\n")
##D 
##D # Calculate total games in database
##D total_games <- sum(dev_games$gameCount)
##D unique_games <- length(unique(unlist(dev_games$gameIds)))
##D cat("Total developer-game relationships:", total_games, "\n")
##D cat("Unique games:", unique_games, "\n")
##D cat("Average developers per game:", round(total_games / unique_games, 2), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_developer_games", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_games_metadata")
### * vgi_all_games_metadata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_games_metadata
### Title: Get Metadata for All Games
### Aliases: vgi_all_games_metadata

### ** Examples

## Not run: 
##D # Get first 1000 games
##D games_metadata <- vgi_all_games_metadata(limit = 1000)
##D 
##D # Basic statistics
##D cat("Total games:", nrow(games_metadata), "\n")
##D cat("Date range:", min(games_metadata$releaseDate), "to", 
##D     max(games_metadata$releaseDate), "\n")
##D 
##D # Price analysis
##D price_stats <- summary(games_metadata$price)
##D print(price_stats)
##D 
##D # Free vs paid games
##D free_games <- sum(games_metadata$price == 0)
##D cat("Free games:", free_games, 
##D     "(", round(free_games/nrow(games_metadata)*100, 1), "%)\n")
##D 
##D # Genre analysis
##D all_genres <- unlist(games_metadata$genres)
##D genre_counts <- sort(table(all_genres), decreasing = TRUE)
##D cat("Top 10 genres:\n")
##D print(head(genre_counts, 10))
##D 
##D # Tag analysis for trends
##D all_tags <- unlist(games_metadata$tags)
##D tag_counts <- sort(table(all_tags), decreasing = TRUE)
##D cat("Top 20 tags:\n")
##D print(head(tag_counts, 20))
##D 
##D # Release patterns by year
##D games_metadata$year <- format(as.Date(games_metadata$releaseDate), "%Y")
##D yearly_releases <- table(games_metadata$year)
##D 
##D barplot(yearly_releases[names(yearly_releases) >= "2015"],
##D         main = "Games Released per Year",
##D         xlab = "Year",
##D         ylab = "Number of Games",
##D         las = 2,
##D         col = "steelblue")
##D 
##D # Developer analysis
##D dev_counts <- sort(table(games_metadata$developer), decreasing = TRUE)
##D cat("Most prolific developers:\n")
##D print(head(dev_counts, 10))
##D 
##D # Price tier analysis
##D games_metadata$price_tier <- cut(games_metadata$price,
##D                                  breaks = c(-0.01, 0, 9.99, 19.99, 
##D                                            39.99, 59.99, Inf),
##D                                  labels = c("Free", "<$10", "$10-20", 
##D                                            "$20-40", "$40-60", ">$60"))
##D 
##D tier_dist <- table(games_metadata$price_tier)
##D pie(tier_dist,
##D     main = "Games by Price Tier",
##D     col = rainbow(length(tier_dist)))
##D 
##D # Find games by specific criteria
##D # Recent indie games
##D recent_indie <- games_metadata[
##D   games_metadata$year >= "2023" & 
##D   sapply(games_metadata$tags, function(t) "Indie" %in% t) &
##D   games_metadata$price < 30,
##D ]
##D cat("Recent indie games under $30:", nrow(recent_indie), "\n")
##D 
##D # Export for external analysis
##D # write.csv(games_metadata, "all_games_metadata.csv", row.names = FALSE)
##D 
##D # Paginated retrieval for large datasets
##D # all_games <- list()
##D # offset <- 0
##D # repeat {
##D #   batch <- vgi_all_games_metadata(limit = 1000, offset = offset)
##D #   if (nrow(batch) == 0) break
##D #   all_games[[length(all_games) + 1]] <- batch
##D #   offset <- offset + 1000
##D #   cat("Retrieved", offset, "games...\n")
##D # }
##D # all_games_df <- do.call(rbind, all_games)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_games_metadata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_games_player_overlap")
### * vgi_all_games_player_overlap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_games_player_overlap
### Title: Get Player Overlap Data for All Games
### Aliases: vgi_all_games_player_overlap

### ** Examples

## Not run: 
##D # Get player overlap data for all games
##D overlap_data <- vgi_all_games_player_overlap()
##D 
##D # Find games with highest overlap percentages
##D high_overlap <- overlap_data[overlap_data$topOverlapPct > 50, ]
##D cat("Games with >50% overlap with another game:", nrow(high_overlap), "\n")
##D 
##D # These are likely sequels, expansions, or very similar games
##D print(head(high_overlap[, c("steamAppId", "topOverlapGame", "topOverlapPct")]))
##D 
##D # Analyze overlap patterns
##D hist(overlap_data$topOverlapPct,
##D      breaks = 50,
##D      main = "Distribution of Maximum Player Overlap",
##D      xlab = "Top Overlap Percentage",
##D      col = "lightcoral")
##D 
##D # Find gaming clusters (mutual high overlap)
##D # Check if game A's top overlap is game B, and vice versa
##D mutual_overlaps <- overlap_data[
##D   mapply(function(id, top_id) {
##D     if (is.na(top_id)) return(FALSE)
##D     overlap_data$topOverlapGame[overlap_data$steamAppId == top_id] == id
##D   }, overlap_data$steamAppId, overlap_data$topOverlapGame),
##D ]
##D cat("Games with mutual top overlap:", nrow(mutual_overlaps), "\n")
##D 
##D # Extract detailed overlap data for analysis
##D # Find games that overlap with a specific game
##D target_game <- 730  # Example: Counter-Strike 2
##D games_overlapping_target <- overlap_data[
##D   sapply(overlap_data$topOverlaps, function(overlaps) {
##D     if (is.null(overlaps)) return(FALSE)
##D     target_game %in% overlaps$steamAppId
##D   }),
##D ]
##D cat("Games with significant overlap with game", target_game, ":", 
##D     nrow(games_overlapping_target), "\n")
##D 
##D # Build a gaming ecosystem map
##D # Extract all overlap relationships
##D all_overlaps <- do.call(rbind, lapply(seq_len(nrow(overlap_data)), function(i) {
##D   game_id <- overlap_data$steamAppId[i]
##D   overlaps <- overlap_data$topOverlaps[[i]]
##D   if (is.null(overlaps) || nrow(overlaps) == 0) return(NULL)
##D   
##D   data.frame(
##D     from = game_id,
##D     to = overlaps$steamAppId[1:min(5, nrow(overlaps))],
##D     overlap_pct = overlaps$overlapPercentage[1:min(5, nrow(overlaps))]
##D   )
##D }))
##D 
##D # Find most connected games (hubs in the network)
##D connection_counts <- table(c(all_overlaps$from, all_overlaps$to))
##D hubs <- head(sort(connection_counts, decreasing = TRUE), 20)
##D cat("Most connected games (appear in many overlaps):\n")
##D print(hubs)
##D 
##D # Genre affinity analysis (would need genre data)
##D # Games with high overlap likely share genres
##D # Could cluster games based on overlap patterns
##D 
##D # Find isolated games (low overlap with any other game)
##D isolated_games <- overlap_data[overlap_data$topOverlapPct < 5 | 
##D                                is.na(overlap_data$topOverlapPct), ]
##D cat("Games with <5% overlap (unique/niche):", nrow(isolated_games), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_games_player_overlap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_games_playtime")
### * vgi_all_games_playtime

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_games_playtime
### Title: Get Playtime Data for All Games
### Aliases: vgi_all_games_playtime

### ** Examples

## Not run: 
##D # Get playtime data for all games
##D playtime_data <- vgi_all_games_playtime()
##D 
##D # Top 20 most played games by average playtime
##D top_played <- head(playtime_data, 20)
##D cat("Top 20 games by average playtime:\n")
##D print(top_played[, c("steamAppId", "avgPlaytime", "medianPlaytime")])
##D 
##D # Find games with high engagement
##D high_engagement <- playtime_data[playtime_data$avgPlaytime > 100, ]
##D cat("Games with >100 hours average playtime:", nrow(high_engagement), "\n")
##D 
##D # Analyze playtime distribution
##D hist(log10(playtime_data$avgPlaytime + 1),
##D      breaks = 40,
##D      main = "Distribution of Average Playtime (log scale)",
##D      xlab = "Log10(Avg Playtime + 1)",
##D      col = "orange")
##D 
##D # Compare average vs median to find games with dedicated players
##D playtime_data$avg_median_ratio <- playtime_data$avgPlaytime / 
##D                                   (playtime_data$medianPlaytime + 0.1)
##D 
##D # Games where average is much higher than median (cult followings)
##D cult_games <- playtime_data[playtime_data$avg_median_ratio > 5 & 
##D                            playtime_data$avgPlaytime > 20, ]
##D cat("Games with cult followings (avg >> median):", nrow(cult_games), "\n")
##D 
##D # Combine with revenue data for value analysis
##D revenue_data <- vgi_revenue_by_date(Sys.Date() - 1)
##D value_analysis <- merge(playtime_data, revenue_data, by = "steamAppId")
##D 
##D # Calculate hours per dollar (value metric)
##D units_data <- vgi_units_sold_by_date(Sys.Date() - 1)
##D value_analysis <- merge(value_analysis, units_data, by = "steamAppId")
##D value_analysis$avg_price <- value_analysis$revenue / 
##D                            (value_analysis$unitsSold + 1)
##D value_analysis$hours_per_dollar <- value_analysis$avgPlaytime / 
##D                                    (value_analysis$avg_price + 0.01)
##D 
##D # Best value games (high playtime, reasonable price)
##D best_value <- value_analysis[value_analysis$hours_per_dollar > 2 & 
##D                             value_analysis$avg_price < 60 &
##D                             value_analysis$unitsSold > 10000, ]
##D best_value <- head(best_value[order(-best_value$hours_per_dollar), ], 20)
##D cat("Best value games (hours per dollar):\n")
##D print(best_value[, c("steamAppId", "avgPlaytime", "avg_price", 
##D                     "hours_per_dollar")])
##D 
##D # Genre analysis (would need genre data)
##D # Multiplayer games typically have higher playtime
##D likely_multiplayer <- playtime_data[playtime_data$avgPlaytime > 50 & 
##D                                    playtime_data$medianPlaytime > 20, ]
##D cat("Likely multiplayer/service games:", nrow(likely_multiplayer), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_games_playtime", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_games_regions")
### * vgi_all_games_regions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_games_regions
### Title: Get Regional Distribution Data for All Games
### Aliases: vgi_all_games_regions

### ** Examples

## Not run: 
##D # Get regional data for all games
##D regions_data <- vgi_all_games_regions()
##D 
##D # Find games by dominant region
##D dominant_regions <- table(regions_data$dominantRegion)
##D print(dominant_regions)
##D 
##D # Visualize regional distribution
##D pie(dominant_regions,
##D     main = "Games by Dominant Region",
##D     col = rainbow(length(dominant_regions)))
##D 
##D # Find globally balanced games
##D regions_data$max_region_pct <- pmax(regions_data$northAmerica,
##D                                     regions_data$europe,
##D                                     regions_data$asia,
##D                                     regions_data$southAmerica,
##D                                     regions_data$oceania,
##D                                     regions_data$africa,
##D                                     regions_data$middleEast)
##D 
##D balanced_games <- regions_data[regions_data$max_region_pct < 40, ]
##D cat("Games with no region >40%:", nrow(balanced_games), "\n")
##D 
##D # Compare Western vs Eastern games
##D regions_data$western_pct <- regions_data$northAmerica + 
##D                            regions_data$europe + 
##D                            regions_data$oceania
##D regions_data$eastern_pct <- regions_data$asia + 
##D                            regions_data$middleEast
##D 
##D western_games <- regions_data[regions_data$western_pct > 70, ]
##D eastern_games <- regions_data[regions_data$eastern_pct > 70, ]
##D 
##D cat("Western-dominated games (>70%):", nrow(western_games), "\n")
##D cat("Eastern-dominated games (>70%):", nrow(eastern_games), "\n")
##D 
##D # Analyze emerging markets
##D emerging_markets <- regions_data$southAmerica + 
##D                    regions_data$africa + 
##D                    regions_data$middleEast
##D 
##D emerging_focused <- regions_data[emerging_markets > 30, ]
##D cat("Games with >30% from emerging markets:", nrow(emerging_focused), "\n")
##D 
##D # Create regional profile heatmap (requires additional packages)
##D # library(ggplot2)
##D # library(reshape2)
##D # 
##D # top_100 <- head(regions_data, 100)
##D # regions_matrix <- top_100[, c("steamAppId", "northAmerica", "europe", 
##D #                              "asia", "southAmerica", "oceania", 
##D #                              "africa", "middleEast")]
##D # regions_long <- melt(regions_matrix, id.vars = "steamAppId")
##D # 
##D # ggplot(regions_long, aes(x = variable, y = steamAppId, fill = value)) +
##D #   geom_tile() +
##D #   scale_fill_gradient(low = "white", high = "darkblue") +
##D #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
##D #   labs(title = "Regional Distribution Heatmap (Top 100 Games)",
##D #        x = "Region", y = "Game", fill = "Player %")
##D 
##D # Find region-specific genres (would need genre data)
##D # Asia-focused games might be more likely to be MMOs or mobile ports
##D asia_focused <- regions_data[regions_data$asia > 50, ]
##D cat("Asia-focused games (>50% Asian players):", nrow(asia_focused), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_games_regions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_games_top_countries")
### * vgi_all_games_top_countries

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_games_top_countries
### Title: Get Top Countries Data for All Games
### Aliases: vgi_all_games_top_countries

### ** Examples

## Not run: 
##D # Get top countries for all games
##D countries_data <- vgi_all_games_top_countries()
##D 
##D # Find games dominated by specific countries
##D us_dominated <- countries_data[countries_data$topCountry == "US" & 
##D                               countries_data$topCountryPct > 50, ]
##D cat("Games where >50% of players are from US:", nrow(us_dominated), "\n")
##D 
##D # Find globally diverse games
##D global_games <- countries_data[countries_data$topCountryPct < 20 & 
##D                               countries_data$countryCount > 50, ]
##D cat("Globally diverse games (<20% from any country):", nrow(global_games), "\n")
##D 
##D # Analyze regional preferences
##D top_countries_summary <- table(countries_data$topCountry)
##D top_10_countries <- head(sort(top_countries_summary, decreasing = TRUE), 10)
##D 
##D barplot(top_10_countries,
##D         main = "Countries Most Often #1 in Games",
##D         xlab = "Country",
##D         ylab = "Number of Games Where #1",
##D         las = 2,
##D         col = "steelblue")
##D 
##D # Extract detailed country data for a specific game
##D game_id <- 730  # Example game
##D game_countries <- countries_data$topCountries[
##D   countries_data$steamAppId == game_id][[1]]
##D if (!is.null(game_countries)) {
##D   print(head(game_countries, 10))
##D }
##D 
##D # Find games popular in specific regions
##D # Extract games where China is in top 3
##D china_popular <- countries_data[sapply(countries_data$topCountries, 
##D   function(tc) {
##D     if (is.null(tc) || nrow(tc) < 3) return(FALSE)
##D     "CN" %in% tc$country[1:3]
##D   }), ]
##D cat("Games where China is in top 3 countries:", nrow(china_popular), "\n")
##D 
##D # Calculate market concentration
##D countries_data$top3_concentration <- sapply(countries_data$topCountries,
##D   function(tc) {
##D     if (is.null(tc) || nrow(tc) < 3) return(NA)
##D     sum(tc$percentage[1:3])
##D   })
##D 
##D # Games with highest geographic concentration
##D concentrated <- countries_data[!is.na(countries_data$top3_concentration) & 
##D                               countries_data$top3_concentration > 70, ]
##D cat("Games where top 3 countries >70% of players:", nrow(concentrated), "\n")
##D 
##D # Regional gaming hours analysis
##D # Games popular in Asia vs Americas vs Europe
##D asia_countries <- c("CN", "JP", "KR", "TW", "HK", "SG", "TH", "ID")
##D americas_countries <- c("US", "CA", "BR", "MX", "AR", "CL", "CO")
##D europe_countries <- c("DE", "FR", "GB", "IT", "ES", "PL", "NL", "SE")
##D 
##D countries_data$asia_pct <- sapply(countries_data$topCountries,
##D   function(tc) {
##D     if (is.null(tc)) return(0)
##D     sum(tc$percentage[tc$country %in% asia_countries])
##D   })
##D 
##D asia_focused <- countries_data[countries_data$asia_pct > 50, ]
##D cat("Games with >50% Asian players:", nrow(asia_focused), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_games_top_countries", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_games_wishlist_countries")
### * vgi_all_games_wishlist_countries

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_games_wishlist_countries
### Title: Get Top Wishlist Countries Data for All Games
### Aliases: vgi_all_games_wishlist_countries

### ** Examples

## Not run: 
##D # Get wishlist country data for all games
##D wishlist_countries <- vgi_all_games_wishlist_countries()
##D 
##D # Compare with player country data
##D player_countries <- vgi_all_games_top_countries()
##D 
##D # Merge to analyze wishlist vs player patterns
##D geo_comparison <- merge(wishlist_countries, player_countries,
##D                        by = "steamAppId",
##D                        suffixes = c("_wishlist", "_player"))
##D 
##D # Find games where wishlist and player geography differ
##D geo_comparison$country_match <- 
##D   geo_comparison$topWishlistCountry == geo_comparison$topCountry
##D 
##D mismatched <- geo_comparison[!geo_comparison$country_match, ]
##D cat("Games where top wishlist country != top player country:", 
##D     nrow(mismatched), "\n")
##D 
##D # Analyze wishlist concentration
##D hist(wishlist_countries$topWishlistCountryPct,
##D      breaks = 30,
##D      main = "Wishlist Geographic Concentration",
##D      xlab = "Top Country Wishlist %",
##D      col = "darkgreen")
##D 
##D # Find games with global wishlist appeal
##D global_wishlist <- wishlist_countries[
##D   wishlist_countries$topWishlistCountryPct < 25 & 
##D   wishlist_countries$wishlistCountryCount > 50, ]
##D cat("Games with global wishlist distribution:", nrow(global_wishlist), "\n")
##D 
##D # Regional wishlist patterns
##D wishlist_top_countries <- table(wishlist_countries$topWishlistCountry)
##D player_top_countries <- table(player_countries$topCountry)
##D 
##D # Compare which countries dominate wishlists vs players
##D country_comparison <- merge(
##D   data.frame(country = names(wishlist_top_countries),
##D              wishlist_games = as.numeric(wishlist_top_countries)),
##D   data.frame(country = names(player_top_countries),
##D              player_games = as.numeric(player_top_countries)),
##D   by = "country", all = TRUE
##D )
##D country_comparison[is.na(country_comparison)] <- 0
##D country_comparison$wishlist_player_ratio <- 
##D   country_comparison$wishlist_games / (country_comparison$player_games + 1)
##D 
##D # Countries that wishlist more than they play
##D high_wishlist_countries <- country_comparison[
##D   country_comparison$wishlist_player_ratio > 1.5 & 
##D   country_comparison$wishlist_games > 10, ]
##D cat("Countries that wishlist disproportionately:\n")
##D print(high_wishlist_countries[order(-high_wishlist_countries$wishlist_player_ratio), ])
##D 
##D # Extract detailed data for emerging markets
##D emerging_markets <- c("BR", "IN", "MX", "TR", "PL")
##D 
##D emerging_wishlist_share <- sapply(wishlist_countries$topWishlistCountries,
##D   function(countries) {
##D     if (is.null(countries)) return(0)
##D     sum(countries$percentage[countries$country %in% emerging_markets])
##D   })
##D 
##D high_emerging <- wishlist_countries[emerging_wishlist_share > 30, ]
##D cat("Games with >30% wishlists from emerging markets:", nrow(high_emerging), "\n")
##D 
##D # Wishlist velocity by region (would need time series data)
##D # Could identify which regions are gaining momentum
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_games_wishlist_countries", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_all_publisher_games")
### * vgi_all_publisher_games

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_all_publisher_games
### Title: Get All Publisher Game IDs
### Aliases: vgi_all_publisher_games

### ** Examples

## Not run: 
##D # Get all publisher game mappings
##D pub_games <- vgi_all_publisher_games()
##D 
##D # Find largest publishers by catalog size
##D top_pubs <- head(pub_games[order(-pub_games$gameCount), ], 20)
##D cat("Top 20 largest publishers:\n")
##D print(top_pubs[, c("publisherId", "gameCount")])
##D 
##D # Get publisher names for context
##D pub_list <- vgi_publisher_list()
##D top_pubs_named <- merge(top_pubs, pub_list, 
##D                        by.x = "publisherId", by.y = "id")
##D print(top_pubs_named[, c("name", "gameCount")])
##D 
##D # Compare with developer portfolios
##D dev_games <- vgi_all_developer_games()
##D cat("Average games per developer:", round(mean(dev_games$gameCount), 1), "\n")
##D cat("Average games per publisher:", round(mean(pub_games$gameCount), 1), "\n")
##D 
##D # Find mega-publishers (>100 games)
##D mega_pubs <- pub_games[pub_games$gameCount > 100, ]
##D cat("Publishers with >100 games:", nrow(mega_pubs), "\n")
##D 
##D # Analyze publisher concentration
##D total_pub_games <- sum(pub_games$gameCount)
##D top_10_pub_games <- sum(head(pub_games$gameCount, 10))
##D concentration <- (top_10_pub_games / total_pub_games) * 100
##D cat(sprintf("Top 10 publishers control %.1f%% of published games\n", 
##D             concentration))
##D 
##D # Find publishers that also develop
##D dev_list <- vgi_developer_list()
##D pub_dev_overlap <- intersect(pub_list$name, dev_list$name)
##D cat("Companies that both publish and develop:", 
##D     length(pub_dev_overlap), "\n")
##D 
##D # Analyze specific publisher's portfolio
##D ea_id <- 1  # Example: Electronic Arts
##D ea_games <- pub_games$gameIds[pub_games$publisherId == ea_id][[1]]
##D if (length(ea_games) > 0) {
##D   cat("EA has published", length(ea_games), "games\n")
##D   
##D   # Get recent EA releases
##D   ea_metadata <- vgi_game_metadata_batch(ea_games)
##D   recent_ea <- ea_metadata[as.Date(ea_metadata$releaseDate) > 
##D                           as.Date("2023-01-01"), ]
##D   cat("EA games released since 2023:", nrow(recent_ea), "\n")
##D }
##D 
##D # Distribution of publisher sizes
##D pub_size_dist <- table(cut(pub_games$gameCount,
##D                           breaks = c(1, 2, 5, 10, 20, 50, 100, Inf),
##D                           labels = c("1", "2-4", "5-9", "10-19", 
##D                                     "20-49", "50-99", "100+"),
##D                           right = FALSE))
##D 
##D barplot(pub_size_dist,
##D         main = "Distribution of Publishers by Portfolio Size",
##D         xlab = "Number of Games Published",
##D         ylab = "Number of Publishers",
##D         col = "purple")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_all_publisher_games", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_concurrent_players_by_date")
### * vgi_concurrent_players_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_concurrent_players_by_date
### Title: Get Concurrent Players Data by Date
### Aliases: vgi_concurrent_players_by_date

### ** Examples

## Not run: 
##D # Get concurrent player data
##D ccu_data <- vgi_concurrent_players_by_date("2024-01-15")
##D 
##D # Top 20 games by concurrent players
##D top_ccu <- head(ccu_data, 20)
##D cat("Top 20 games by peak concurrent players:\n")
##D print(top_ccu[, c("steamAppId", "peakConcurrent", "avgConcurrent")])
##D 
##D # Calculate peak-to-average ratio (indicates play pattern)
##D ccu_data$peak_avg_ratio <- ccu_data$peakConcurrent / 
##D                           (ccu_data$avgConcurrent + 1)
##D 
##D # Games with spiky play patterns (high peak/avg ratio)
##D spiky_games <- ccu_data[ccu_data$peak_avg_ratio > 3 & 
##D                         ccu_data$peakConcurrent > 1000, ]
##D cat("Games with concentrated play times:", nrow(spiky_games), "\n")
##D 
##D # Compare weekend vs weekday
##D is_weekend <- weekdays(as.Date("2024-01-15")) %in% c("Saturday", "Sunday")
##D if (!is_weekend) {
##D   # Get previous weekend data
##D   last_saturday <- as.Date("2024-01-15") - 
##D                   ((as.numeric(format(as.Date("2024-01-15"), "%w")) + 1) %% 7)
##D   weekend_ccu <- vgi_concurrent_players_by_date(as.character(last_saturday))
##D   
##D   comparison <- merge(ccu_data, weekend_ccu,
##D                      by = "steamAppId",
##D                      suffixes = c("_weekday", "_weekend"))
##D   
##D   # Calculate weekend boost
##D   comparison$weekend_boost <- (comparison$peakConcurrent_weekend - 
##D                               comparison$peakConcurrent_weekday) / 
##D                              comparison$peakConcurrent_weekday * 100
##D   
##D   # Games with biggest weekend boost
##D   weekend_games <- comparison[comparison$weekend_boost > 50 & 
##D                              comparison$peakConcurrent_weekday > 500, ]
##D   cat("Games with >50% weekend boost:", nrow(weekend_games), "\n")
##D }
##D 
##D # Analyze player concentration
##D total_players <- sum(ccu_data$peakConcurrent)
##D top_10_players <- sum(head(ccu_data$peakConcurrent, 10))
##D concentration <- (top_10_players / total_players) * 100
##D 
##D cat(sprintf("Top 10 games account for %.1f%% of all concurrent players\n", 
##D             concentration))
##D 
##D # Distribution analysis
##D hist(log10(ccu_data$peakConcurrent + 1),
##D      breaks = 40,
##D      main = "Distribution of Peak Concurrent Players (log scale)",
##D      xlab = "Log10(Peak CCU + 1)",
##D      col = "orange")
##D 
##D # Find multiplayer vs single-player patterns
##D # (Multiplayer games typically have higher avg/peak ratios)
##D ccu_data$avg_peak_ratio <- ccu_data$avgConcurrent / 
##D                           (ccu_data$peakConcurrent + 1)
##D likely_multiplayer <- ccu_data[ccu_data$avg_peak_ratio > 0.6 & 
##D                               ccu_data$peakConcurrent > 1000, ]
##D cat("Likely multiplayer games (high avg/peak):", nrow(likely_multiplayer), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_concurrent_players_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_developer_games")
### * vgi_developer_games

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_developer_games
### Title: Get Game IDs by Developer
### Aliases: vgi_developer_games

### ** Examples

## Not run: 
##D # Get all game IDs for a developer
##D game_ids <- vgi_developer_games(company_id = 24569)
##D 
##D # Count games by this developer
##D cat("Total games:", length(game_ids), "\n")
##D 
##D # Get detailed info for the first 5 games
##D if (length(game_ids) > 0) {
##D   first_five <- head(game_ids, 5)
##D   game_details <- lapply(first_five, vgi_game_metadata)
##D   
##D   # Display game names
##D   for (game in game_details) {
##D     cat(game$name, "(", game$steamAppId, ")\n")
##D   }
##D }
##D 
##D # Check if developer made a specific game
##D target_game_id <- 730
##D if (target_game_id %in% game_ids) {
##D   cat("Yes, this developer made game", target_game_id, "\n")
##D }
##D 
##D # Analyze developer's recent releases
##D if (length(game_ids) > 0) {
##D   # Get metadata for all games
##D   all_games <- vgi_game_metadata_batch(game_ids)
##D   
##D   # Find games released in last 2 years
##D   recent_games <- all_games[
##D     as.Date(all_games$releaseDate) > Sys.Date() - 730,
##D   ]
##D   
##D   cat("Games released in last 2 years:", nrow(recent_games), "\n")
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_developer_games", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_developer_info")
### * vgi_developer_info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_developer_info
### Title: Get Developer Information
### Aliases: vgi_developer_info

### ** Examples

## Not run: 
##D # Get information about a developer
##D dev_info <- vgi_developer_info(company_id = 24569)
##D 
##D # Display developer details
##D cat("Developer:", dev_info$name, "\n")
##D cat("Founded:", dev_info$foundedDate, "\n")
##D cat("Total Games:", dev_info$totalGames, "\n")
##D cat("Active Games:", dev_info$activeGames, "\n")
##D 
##D # Check developer size
##D if (!is.null(dev_info$employeeCount)) {
##D   if (dev_info$employeeCount < 10) {
##D     cat("This is an indie developer\n")
##D   } else if (dev_info$employeeCount < 100) {
##D     cat("This is a mid-sized developer\n")
##D   } else {
##D     cat("This is a large developer\n")
##D   }
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_developer_info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_developer_list")
### * vgi_developer_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_developer_list
### Title: Get Complete Developer List
### Aliases: vgi_developer_list

### ** Examples

## Not run: 
##D # Get all developers
##D all_devs <- vgi_developer_list()
##D cat("Total developers:", nrow(all_devs), "\n")
##D 
##D # Search for developers by name
##D valve_devs <- all_devs[grep("Valve", all_devs$name, ignore.case = TRUE), ]
##D print(valve_devs)
##D 
##D # Find developer ID by exact name
##D dev_id <- all_devs$id[all_devs$name == "Valve Corporation"]
##D if (length(dev_id) > 0) {
##D   cat("Valve Corporation ID:", dev_id, "\n")
##D   
##D   # Get more info about this developer
##D   valve_info <- vgi_developer_info(dev_id)
##D   print(valve_info)
##D }
##D 
##D # Analyze developer names
##D # Find developers with "Studios" in name
##D studios <- all_devs[grep("Studios", all_devs$name), ]
##D cat("Developers with 'Studios':", nrow(studios), "\n")
##D 
##D # Find indie developers (often individual names or small teams)
##D short_names <- all_devs[nchar(all_devs$name) < 15, ]
##D cat("Developers with short names:", nrow(short_names), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_developer_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_followers_by_date")
### * vgi_followers_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_followers_by_date
### Title: Get Followers Data by Date
### Aliases: vgi_followers_by_date

### ** Examples

## Not run: 
##D # Get follower data for a specific date
##D followers <- vgi_followers_by_date("2024-01-15")
##D 
##D # Top 20 most followed games
##D top_followed <- head(followers, 20)
##D print(top_followed)
##D 
##D # Compare with wishlist data to find engagement patterns
##D wishlists <- vgi_wishlists_by_date("2024-01-15")
##D engagement <- merge(followers, wishlists, by = "steamAppId")
##D engagement$follow_to_wishlist_ratio <- 
##D   engagement$followerCount / (engagement$wishlistCount + 1)
##D 
##D # Games with high follower/wishlist ratio (strong community)
##D high_engagement <- engagement[engagement$follow_to_wishlist_ratio > 2 & 
##D                               engagement$followerCount > 10000, ]
##D cat("High community engagement games:", nrow(high_engagement), "\n")
##D 
##D # Monthly follower growth analysis
##D month_ago <- as.Date("2024-01-15") - 30
##D followers_prev <- vgi_followers_by_date(as.character(month_ago))
##D 
##D growth <- merge(followers, followers_prev,
##D                by = "steamAppId",
##D                suffixes = c("_now", "_prev"))
##D growth$monthly_change <- growth$followerCount_now - growth$followerCount_prev
##D growth$monthly_pct <- (growth$monthly_change / growth$followerCount_prev) * 100
##D 
##D # Fastest growing communities (min 5000 followers)
##D qualified <- growth[growth$followerCount_prev >= 5000, ]
##D fastest <- head(qualified[order(-qualified$monthly_pct), ], 20)
##D 
##D cat("Fastest growing communities (>5000 followers):\n")
##D print(fastest[, c("steamAppId", "followerCount_now", 
##D                  "monthly_change", "monthly_pct")])
##D 
##D # Find games losing followers
##D declining <- growth[growth$monthly_change < -500, ]
##D cat("Games losing 500+ followers this month:", nrow(declining), "\n")
##D 
##D # Analyze follower distribution by tier
##D follower_tiers <- cut(followers$followerCount,
##D                      breaks = c(0, 1000, 10000, 50000, 100000, Inf),
##D                      labels = c("<1K", "1K-10K", "10K-50K", "50K-100K", ">100K"))
##D tier_summary <- table(follower_tiers)
##D 
##D barplot(tier_summary,
##D         main = "Distribution of Games by Follower Count",
##D         xlab = "Follower Tier",
##D         ylab = "Number of Games",
##D         col = "skyblue")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_followers_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_game_list")
### * vgi_game_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_game_list
### Title: Get Complete Game List
### Aliases: vgi_game_list

### ** Examples

## Not run: 
##D # Get all games
##D all_games <- vgi_game_list()
##D cat("Total games in database:", nrow(all_games), "\n")
##D 
##D # Search for games by name
##D cs_games <- all_games[grep("Counter-Strike", all_games$name, ignore.case = TRUE), ]
##D print(cs_games)
##D 
##D # Find game ID by exact name
##D game_id <- all_games$id[all_games$name == "Counter-Strike 2"]
##D if (length(game_id) > 0) {
##D   cat("Counter-Strike 2 ID:", game_id, "\n")
##D }
##D 
##D # Get random sample of games for analysis
##D sample_games <- all_games[sample(nrow(all_games), 10), ]
##D print(sample_games)
##D 
##D # Cache results for future use
##D saveRDS(all_games, "vgi_game_list_cache.rds")
##D # Later: all_games <- readRDS("vgi_game_list_cache.rds")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_game_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_game_metadata")
### * vgi_game_metadata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_game_metadata
### Title: Get Game Metadata from Video Game Insights
### Aliases: vgi_game_metadata

### ** Examples

## Not run: 
##D # Ensure the VGI_AUTH_TOKEN environment variable is set
##D # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
##D 
##D # Get metadata for Valheim (Steam App ID: 892970)
##D valheim_data <- vgi_game_metadata(892970)
##D print(valheim_data)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_game_metadata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_game_metadata_batch")
### * vgi_game_metadata_batch

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_game_metadata_batch
### Title: Get Batch Game Metadata from Video Game Insights
### Aliases: vgi_game_metadata_batch

### ** Examples

## Not run: 
##D # Ensure the VGI_AUTH_TOKEN environment variable is set
##D # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
##D 
##D # Get metadata for multiple games
##D game_ids <- c(892970, 1245620, 105600) # Valheim, Elden Ring, Terraria
##D games_data <- vgi_game_metadata_batch(game_ids)
##D print(games_data)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_game_metadata_batch", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_game_rankings")
### * vgi_game_rankings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_game_rankings
### Title: Get Game Rankings
### Aliases: vgi_game_rankings

### ** Examples

## Not run: 
##D # Get all game rankings
##D rankings <- vgi_game_rankings()
##D 
##D # Find top 10 games by revenue
##D top_revenue <- head(rankings[order(rankings$totalRevenueRank), ], 10)
##D print(top_revenue[, c("steamAppId", "totalRevenueRank", "totalRevenuePrct")])
##D 
##D # Find games that rank well across multiple metrics
##D # (top 100 in both revenue and reviews)
##D top_overall <- rankings[
##D   rankings$totalRevenueRank <= 100 & 
##D   rankings$positiveReviewsRank <= 100, 
##D ]
##D print(paste("Games in top 100 for both revenue and reviews:", nrow(top_overall)))
##D 
##D # Identify trending games (high recent sales relative to total)
##D rankings$trending_score <- rankings$totalUnitsSoldRank / rankings$yesterdayUnitsSoldRank
##D trending <- head(rankings[order(rankings$trending_score, decreasing = TRUE), ], 20)
##D 
##D # Create a scatter plot of revenue vs reviews rankings
##D plot(rankings$totalRevenueRank, rankings$positiveReviewsRank,
##D      pch = 19, col = rgb(0, 0, 1, 0.1),
##D      xlab = "Revenue Rank", ylab = "Reviews Rank",
##D      main = "Game Rankings: Revenue vs Reviews")
##D abline(a = 0, b = 1, col = "red", lty = 2)
##D 
##D # Find hidden gems (great reviews but lower revenue)
##D hidden_gems <- rankings[
##D   rankings$positiveReviewsRank <= 50 & 
##D   rankings$totalRevenueRank > 200,
##D ]
##D print(paste("Hidden gems found:", nrow(hidden_gems)))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_game_rankings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_historical_data")
### * vgi_historical_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_historical_data
### Title: Get Historical Game Data
### Aliases: vgi_historical_data

### ** Examples

## Not run: 
##D # Get all historical data for a game
##D historical <- vgi_historical_data(steam_app_id = 730)
##D 
##D # Plot revenue over time
##D if (!is.null(historical$revenue)) {
##D   plot(as.Date(historical$revenue$date), historical$revenue$revenue,
##D        type = "l", col = "green", lwd = 2,
##D        xlab = "Date", ylab = "Revenue ($)",
##D        main = "Revenue Over Time")
##D }
##D 
##D # Analyze review sentiment over time
##D if (!is.null(historical$reviews)) {
##D   historical$reviews$positiveRatio <- historical$reviews$positive / 
##D     (historical$reviews$positive + historical$reviews$negative)
##D   
##D   plot(as.Date(historical$reviews$date), historical$reviews$positiveRatio,
##D        type = "l", col = "blue", lwd = 2,
##D        xlab = "Date", ylab = "Positive Review Ratio",
##D        main = "Review Sentiment Over Time")
##D   abline(h = 0.7, col = "green", lty = 2)
##D   abline(h = 0.5, col = "orange", lty = 2)
##D }
##D 
##D # Correlate price changes with sales
##D if (!is.null(historical$priceHistory) && !is.null(historical$unitsSold)) {
##D   # Find USD prices
##D   usd_prices <- historical$priceHistory[historical$priceHistory$currency == "USD", ]
##D   
##D   # Match dates between price and units sold
##D   matched_data <- merge(usd_prices, historical$unitsSold, 
##D                        by = "date", all = FALSE)
##D   
##D   if (nrow(matched_data) > 0) {
##D     plot(matched_data$price, matched_data$unitsSold,
##D          pch = 19, col = "darkblue",
##D          xlab = "Price (USD)", ylab = "Units Sold",
##D          main = "Price vs. Sales Correlation")
##D   }
##D }
##D 
##D # Calculate growth metrics
##D if (!is.null(historical$followers)) {
##D   n <- nrow(historical$followers)
##D   if (n > 30) {
##D     growth_30d <- (historical$followers$followers[n] - 
##D                    historical$followers$followers[n-30]) / 
##D                   historical$followers$followers[n-30] * 100
##D     cat("30-day follower growth:", round(growth_30d, 1), "%\n")
##D   }
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_historical_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_ccu")
### * vgi_insights_ccu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_ccu
### Title: Get Concurrent Users (CCU) Data
### Aliases: vgi_insights_ccu

### ** Examples

## Not run: 
##D # Get CCU data for Counter-Strike 2
##D ccu_data <- vgi_insights_ccu(steam_app_id = 730)
##D 
##D # Plot max concurrent players over time
##D plot(ccu_data$playerHistory$date, ccu_data$playerHistory$max,
##D      type = "l",
##D      main = "Peak Concurrent Players",
##D      xlab = "Date",
##D      ylab = "Players")
##D 
##D # Calculate average peak CCU
##D avg_peak <- mean(ccu_data$playerHistory$max, na.rm = TRUE)
##D print(paste("Average peak CCU:", round(avg_peak)))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_ccu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_dau_mau")
### * vgi_insights_dau_mau

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_dau_mau
### Title: Get Daily and Monthly Active Users Data
### Aliases: vgi_insights_dau_mau

### ** Examples

## Not run: 
##D # Get DAU/MAU data for a game
##D active_players <- vgi_insights_dau_mau(steam_app_id = 730)
##D 
##D # Calculate DAU/MAU ratios
##D active_players$playerHistory$dau_mau_ratio <- 
##D   active_players$playerHistory$dau / active_players$playerHistory$mau
##D 
##D # Calculate average DAU/MAU ratio
##D avg_ratio <- mean(active_players$playerHistory$dau_mau_ratio, na.rm = TRUE)
##D print(paste("Average DAU/MAU ratio:", round(avg_ratio, 3)))
##D 
##D # Plot DAU and MAU over time
##D par(mfrow = c(2, 1))
##D plot(active_players$playerHistory$date, active_players$playerHistory$dau, 
##D      type = "l", col = "blue",
##D      main = "Daily Active Users", 
##D      xlab = "Date", ylab = "DAU")
##D plot(active_players$playerHistory$date, active_players$playerHistory$mau, 
##D      type = "l", col = "red",
##D      main = "Monthly Active Users", 
##D      xlab = "Date", ylab = "MAU")
##D 
##D # Analyze retention trends
##D plot(active_players$playerHistory$date, 
##D      active_players$playerHistory$dau_mau_ratio,
##D      type = "l", ylim = c(0, 1),
##D      main = "Player Retention (DAU/MAU Ratio)",
##D      xlab = "Date", ylab = "DAU/MAU Ratio")
##D abline(h = 0.3, col = "gray", lty = 2)  # Industry average
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_dau_mau", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_followers")
### * vgi_insights_followers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_followers
### Title: Get Follower Data for a Game
### Aliases: vgi_insights_followers

### ** Examples

## Not run: 
##D # Get follower data for a game
##D followers <- vgi_insights_followers(steam_app_id = 892970)
##D 
##D # Display current followers
##D current_followers <- tail(followers$followersChange, 1)$followersTotal
##D print(paste("Current followers:", format(current_followers, big.mark = ",")))
##D 
##D # Calculate growth rate
##D if (nrow(followers$followersChange) >= 7) {
##D   week_ago <- followers$followersChange[nrow(followers$followersChange) - 6, ]
##D   weekly_growth <- current_followers - week_ago$followersTotal
##D   print(paste("Weekly growth:", format(weekly_growth, big.mark = ",")))
##D }
##D 
##D # Plot follower growth
##D plot(followers$followersChange$date, followers$followersChange$followersTotal,
##D      type = "l", col = "darkgreen", lwd = 2,
##D      main = "Follower Growth Over Time",
##D      xlab = "Date", ylab = "Total Followers")
##D 
##D # Add daily changes as bars
##D par(new = TRUE)
##D barplot(followers$followersChange$followersChange,
##D         col = ifelse(followers$followersChange$followersChange > 0, 
##D                      "lightgreen", "lightcoral"),
##D         border = NA, axes = FALSE, xlab = "", ylab = "")
##D axis(4)
##D mtext("Daily Change", side = 4, line = 3)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_followers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_player_regions")
### * vgi_insights_player_regions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_player_regions
### Title: Get Player Geographic Distribution Data
### Aliases: vgi_insights_player_regions

### ** Examples

## Not run: 
##D # Get player regions for a game
##D regions <- vgi_insights_player_regions(steam_app_id = 730)
##D 
##D # Display regions sorted by rank
##D print(regions$regions)
##D 
##D # Find the top region
##D top_region <- regions$regions[regions$regions$rank == 1, ]
##D print(paste("Top region:", top_region$regionName, 
##D             "with", round(top_region$percentage, 1), "% of players"))
##D 
##D # Create a horizontal bar chart of regions
##D par(mar = c(5, 8, 4, 2))  # Increase left margin for region names
##D barplot(regions$regions$percentage,
##D         names.arg = regions$regions$regionName,
##D         horiz = TRUE,
##D         las = 1,
##D         main = "Player Distribution by Region",
##D         xlab = "Percentage of Players",
##D         col = rainbow(nrow(regions$regions), alpha = 0.8))
##D 
##D # Check geographic diversity
##D top_3_percentage <- sum(head(regions$regions$percentage, 3))
##D print(paste("Top 3 regions account for", 
##D             round(top_3_percentage, 1), "% of players"))
##D             
##D # Identify potential server locations
##D major_regions <- regions$regions[regions$regions$percentage > 10, ]
##D print(paste("Regions with >10% of players:", 
##D             paste(major_regions$regionName, collapse = ", ")))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_player_regions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_playtime")
### * vgi_insights_playtime

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_playtime
### Title: Get Playtime Statistics for a Game
### Aliases: vgi_insights_playtime

### ** Examples

## Not run: 
##D # Get playtime statistics for a game
##D playtime <- vgi_insights_playtime(steam_app_id = 730)
##D 
##D # Display overall statistics
##D print(paste("Average playtime:", round(playtime$avgPlaytime / 60, 1), "hours"))
##D print(paste("Median playtime:", round(playtime$medianPlaytime / 60, 1), "hours"))
##D print(paste("Playtime rank:", playtime$avgPlaytimeRank))
##D print(paste("Better than", round(100 - playtime$avgPlaytimePrct, 1), "% of games"))
##D 
##D # Visualize playtime distribution
##D if (nrow(playtime$playtimeRanges) > 0) {
##D   barplot(playtime$playtimeRanges$percentage,
##D           names.arg = playtime$playtimeRanges$range,
##D           main = "Player Playtime Distribution",
##D           xlab = "Hours Played",
##D           ylab = "Percentage of Players",
##D           col = "steelblue",
##D           las = 2)
##D }
##D 
##D # Calculate engaged players (20+ hours)
##D engaged_ranges <- c("20-50", "50-100", "100-200", "200-500", "500+")
##D engaged_players <- sum(playtime$playtimeRanges$percentage[
##D   playtime$playtimeRanges$range %in% engaged_ranges
##D ], na.rm = TRUE)
##D print(paste("Engaged players (20+ hours):", round(engaged_players, 1), "%"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_playtime", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_price_history")
### * vgi_insights_price_history

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_price_history
### Title: Get Price History Data for a Game
### Aliases: vgi_insights_price_history

### ** Examples

## Not run: 
##D # Get price history for a game in USD
##D usd_history <- vgi_insights_price_history(
##D   steam_app_id = 730,
##D   currency = "USD"
##D )
##D 
##D # Calculate discount percentage for each price period
##D if (nrow(usd_history$priceChanges) > 0) {
##D   usd_history$priceChanges$discount_pct <- 
##D     round((1 - usd_history$priceChanges$priceFinal / 
##D            usd_history$priceChanges$priceInitial) * 100, 1)
##D }
##D 
##D # Get price history for all currencies
##D all_prices <- vgi_insights_price_history(steam_app_id = 730)
##D 
##D # Find all currencies where the game is available
##D currencies <- sapply(all_prices$price, function(x) x$currency)
##D print(paste("Available in", length(currencies), "currencies"))
##D 
##D # Identify sales periods (where priceFinal < priceInitial)
##D sales <- usd_history$priceChanges[
##D   usd_history$priceChanges$priceFinal < usd_history$priceChanges$priceInitial, 
##D ]
##D print(paste("Number of sale periods:", nrow(sales)))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_price_history", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_revenue")
### * vgi_insights_revenue

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_revenue
### Title: Get Revenue Insights from Video Game Insights
### Aliases: vgi_insights_revenue

### ** Examples

## Not run: 
##D # Get revenue history for a game
##D revenue_data <- vgi_insights_revenue(steam_app_id = 892970)
##D 
##D # Plot revenue changes over time
##D plot(revenue_data$date, revenue_data$revenueChange,
##D      type = "l",
##D      main = "Revenue Changes Over Time",
##D      xlab = "Date", 
##D      ylab = "Revenue Change")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_revenue", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_reviews")
### * vgi_insights_reviews

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_reviews
### Title: Get Review Analytics from Video Game Insights
### Aliases: vgi_insights_reviews

### ** Examples

## Not run: 
##D # Get review history for a game
##D reviews <- vgi_insights_reviews(steam_app_id = 892970)
##D 
##D # Calculate current overall rating
##D latest <- tail(reviews, 1)
##D rating <- latest$positiveReviewsTotal / 
##D           (latest$positiveReviewsTotal + latest$negativeReviewsTotal) * 100
##D print(paste("Overall rating:", round(rating, 1), "%"))
##D 
##D # Plot review trends
##D plot(reviews$date, reviews$positiveReviewsChange, 
##D      type = "l", col = "green",
##D      main = "Daily Review Trends",
##D      xlab = "Date", ylab = "New Reviews")
##D lines(reviews$date, reviews$negativeReviewsChange, col = "red")
##D legend("topright", c("Positive", "Negative"), 
##D        col = c("green", "red"), lty = 1)
##D 
##D # Find review bombs (days with unusual negative reviews)
##D avg_negative <- mean(reviews$negativeReviewsChange, na.rm = TRUE)
##D sd_negative <- sd(reviews$negativeReviewsChange, na.rm = TRUE)
##D review_bombs <- reviews[reviews$negativeReviewsChange > avg_negative + 2*sd_negative, ]
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_reviews", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_units")
### * vgi_insights_units

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_units
### Title: Get Units Sold Data for a Game
### Aliases: vgi_insights_units

### ** Examples

## Not run: 
##D # Get units sold history for a game
##D units_data <- vgi_insights_units(steam_app_id = 730)
##D 
##D # Plot cumulative units sold over time
##D plot(units_data$date, units_data$unitsSoldTotal, 
##D      type = "l", main = "Total Units Sold Over Time",
##D      xlab = "Date", ylab = "Total Units")
##D 
##D # Calculate daily sales for recent period
##D recent_data <- tail(units_data, 30)
##D daily_sales <- mean(recent_data$unitsSoldChange, na.rm = TRUE)
##D print(paste("Average daily sales (last 30 days):", round(daily_sales)))
##D 
##D # Find peak sales day
##D peak_day <- units_data[which.max(units_data$unitsSoldChange), ]
##D print(paste("Peak sales:", peak_day$unitsSoldChange, "on", peak_day$date))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_units", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_insights_wishlists")
### * vgi_insights_wishlists

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_insights_wishlists
### Title: Get Wishlist Data for a Game
### Aliases: vgi_insights_wishlists

### ** Examples

## Not run: 
##D # Get wishlist data for a game
##D wishlists <- vgi_insights_wishlists(steam_app_id = 892970)
##D 
##D # Calculate total wishlists and recent trend
##D current_wishlists <- tail(wishlists$wishlistChanges, 1)$wishlistsTotal
##D print(paste("Current wishlists:", format(current_wishlists, big.mark = ",")))
##D 
##D # Calculate 30-day growth
##D if (nrow(wishlists$wishlistChanges) >= 30) {
##D   thirty_days_ago <- wishlists$wishlistChanges[nrow(wishlists$wishlistChanges) - 29, ]
##D   growth <- current_wishlists - thirty_days_ago$wishlistsTotal
##D   growth_pct <- (growth / thirty_days_ago$wishlistsTotal) * 100
##D   print(paste("30-day growth:", format(growth, big.mark = ","), 
##D               sprintf("(%.1f%%)", growth_pct)))
##D }
##D 
##D # Plot wishlist trend
##D plot(wishlists$wishlistChanges$date, wishlists$wishlistChanges$wishlistsTotal,
##D      type = "l", col = "blue", lwd = 2,
##D      main = "Wishlist Trend Over Time",
##D      xlab = "Date", ylab = "Total Wishlists")
##D 
##D # Identify major wishlist spikes
##D avg_change <- mean(abs(wishlists$wishlistChanges$wishlistsChange), na.rm = TRUE)
##D spikes <- wishlists$wishlistChanges[
##D   wishlists$wishlistChanges$wishlistsChange > avg_change * 3, 
##D ]
##D if (nrow(spikes) > 0) {
##D   print("Major wishlist spikes detected on:")
##D   print(spikes[, c("date", "wishlistsChange")])
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_insights_wishlists", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_player_overlap")
### * vgi_player_overlap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_player_overlap
### Title: Get Player Overlap Data
### Aliases: vgi_player_overlap

### ** Examples

## Not run: 
##D # Get player overlap for a game
##D overlap <- vgi_player_overlap(steam_app_id = 892970, limit = 20)
##D 
##D # Find games with strongest overlap
##D strong_overlap <- overlap$playerOverlaps[
##D   overlap$playerOverlaps$unitsSoldOverlapIndex > 2.0,
##D ]
##D print(paste("Games with strong overlap:", nrow(strong_overlap)))
##D 
##D # Analyze competitor landscape
##D competitors <- head(overlap$playerOverlaps[
##D   order(-overlap$playerOverlaps$unitsSoldOverlapPercentage),
##D ], 5)
##D print("Top 5 competitors by player overlap:")
##D print(competitors[, c("steamAppId", "unitsSoldOverlapPercentage", 
##D                       "unitsSoldOverlapIndex")])
##D 
##D # Find games where overlap players are highly engaged
##D engaged_overlap <- overlap$playerOverlaps[
##D   overlap$playerOverlaps$medianPlaytime > 50,
##D ]
##D print(paste("Games where overlap players spend 50+ hours:", 
##D             nrow(engaged_overlap)))
##D 
##D # Calculate total addressable market from overlap
##D total_overlap_players <- sum(overlap$playerOverlaps$unitsSoldOverlap)
##D print(paste("Total unique players across all overlaps:", 
##D             format(total_overlap_players, big.mark = ",")))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_player_overlap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_publisher_games")
### * vgi_publisher_games

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_publisher_games
### Title: Get Game IDs by Publisher
### Aliases: vgi_publisher_games

### ** Examples

## Not run: 
##D # Get all game IDs for a publisher
##D game_ids <- vgi_publisher_games(company_id = 13190)
##D 
##D # Count games by this publisher
##D cat("Total games published:", length(game_ids), "\n")
##D 
##D # Get detailed info for the first 5 games
##D if (length(game_ids) > 0) {
##D   first_five <- head(game_ids, 5)
##D   game_details <- lapply(first_five, vgi_game_metadata)
##D   
##D   # Display game names
##D   for (game in game_details) {
##D     cat(game$name, "(", game$steamAppId, ")\n")
##D   }
##D }
##D 
##D # Analyze publisher's portfolio
##D if (length(game_ids) > 0) {
##D   # Get metadata for all games
##D   all_games <- vgi_game_metadata_batch(game_ids)
##D   
##D   # Group by genre
##D   genre_counts <- table(unlist(lapply(all_games$genres, function(x) x)))
##D   print("Publisher's genre distribution:")
##D   print(sort(genre_counts, decreasing = TRUE))
##D }
##D 
##D # Find publisher's most successful games
##D if (length(game_ids) > 10) {
##D   # Get revenue data for top games
##D   revenues <- lapply(head(game_ids, 10), function(id) {
##D     tryCatch({
##D       rev_data <- vgi_insights_revenue(id)
##D       list(id = id, revenue = rev_data$revenueTotal)
##D     }, error = function(e) NULL)
##D   })
##D   
##D   # Filter out NULLs and sort by revenue
##D   revenues <- revenues[!sapply(revenues, is.null)]
##D   revenues <- revenues[order(sapply(revenues, function(x) x$revenue), 
##D                             decreasing = TRUE)]
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_publisher_games", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_publisher_info")
### * vgi_publisher_info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_publisher_info
### Title: Get Publisher Information
### Aliases: vgi_publisher_info

### ** Examples

## Not run: 
##D # Get information about a publisher
##D pub_info <- vgi_publisher_info(company_id = 13190)
##D 
##D # Display publisher details
##D cat("Publisher:", pub_info$name, "\n")
##D cat("Founded:", pub_info$foundedDate, "\n")
##D cat("Total Games Published:", pub_info$totalGames, "\n")
##D cat("Currently Active Games:", pub_info$activeGames, "\n")
##D 
##D # Analyze publisher size
##D if (!is.null(pub_info$employeeCount)) {
##D   if (pub_info$employeeCount > 1000) {
##D     cat("This is a major publisher\n")
##D   } else if (pub_info$employeeCount > 100) {
##D     cat("This is a mid-sized publisher\n")
##D   } else {
##D     cat("This is a boutique publisher\n")
##D   }
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_publisher_info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_publisher_list")
### * vgi_publisher_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_publisher_list
### Title: Get Complete Publisher List
### Aliases: vgi_publisher_list

### ** Examples

## Not run: 
##D # Get all publishers
##D all_pubs <- vgi_publisher_list()
##D cat("Total publishers:", nrow(all_pubs), "\n")
##D 
##D # Search for major publishers
##D ea_pubs <- all_pubs[grep("Electronic Arts", all_pubs$name, ignore.case = TRUE), ]
##D print(ea_pubs)
##D 
##D # Find publisher ID by exact name
##D pub_id <- all_pubs$id[all_pubs$name == "Ubisoft"]
##D if (length(pub_id) > 0) {
##D   cat("Ubisoft ID:", pub_id, "\n")
##D   
##D   # Get more info about this publisher
##D   ubi_info <- vgi_publisher_info(pub_id)
##D   print(ubi_info)
##D }
##D 
##D # Analyze publisher ecosystem
##D # Find self-published developers
##D all_devs <- vgi_developer_list()
##D self_published <- intersect(all_pubs$name, all_devs$name)
##D cat("Companies that both develop and publish:", length(self_published), "\n")
##D 
##D # Find publishers with "Games" in name
##D games_pubs <- all_pubs[grep("Games", all_pubs$name), ]
##D cat("Publishers with 'Games' in name:", nrow(games_pubs), "\n")
##D 
##D # Export for analysis
##D write.csv(all_pubs, "vgi_publishers.csv", row.names = FALSE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_publisher_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_revenue_by_date")
### * vgi_revenue_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_revenue_by_date
### Title: Get Revenue Data by Date
### Aliases: vgi_revenue_by_date

### ** Examples

## Not run: 
##D # Get revenue data for a specific date
##D revenue_data <- vgi_revenue_by_date("2024-01-15")
##D 
##D # Top 20 highest-grossing games
##D top_revenue <- head(revenue_data, 20)
##D cat("Top 20 games by revenue:\n")
##D print(top_revenue[, c("steamAppId", "revenue", "dailyRevenue")])
##D 
##D # Format revenue for display
##D top_revenue$revenue_millions <- round(top_revenue$revenue / 1000000, 2)
##D cat("Top game revenue: $", top_revenue$revenue_millions[1], "M\n", sep = "")
##D 
##D # Calculate market concentration
##D total_revenue <- sum(revenue_data$revenue)
##D top_10_revenue <- sum(head(revenue_data$revenue, 10))
##D concentration <- (top_10_revenue / total_revenue) * 100
##D cat(sprintf("Top 10 games represent %.1f%% of total revenue\n", concentration))
##D 
##D # Daily revenue leaders
##D prev_date <- as.Date("2024-01-15") - 1
##D revenue_prev <- vgi_revenue_by_date(as.character(prev_date))
##D 
##D daily_rev <- merge(revenue_data, revenue_prev,
##D                   by = "steamAppId",
##D                   suffixes = c("_today", "_yesterday"))
##D daily_rev$revenue_today <- daily_rev$revenue_today - daily_rev$revenue_yesterday
##D 
##D # Games with highest daily revenue
##D top_daily_rev <- head(daily_rev[order(-daily_rev$revenue_today), ], 20)
##D cat("Top daily revenue generators:\n")
##D top_daily_rev$daily_rev_k <- round(top_daily_rev$revenue_today / 1000, 1)
##D print(top_daily_rev[, c("steamAppId", "daily_rev_k")])
##D 
##D # Revenue distribution analysis
##D revenue_tiers <- cut(revenue_data$revenue,
##D                     breaks = c(0, 10000, 100000, 1000000, 10000000, Inf),
##D                     labels = c("<$10K", "$10K-100K", "$100K-1M", 
##D                               "$1M-10M", ">$10M"))
##D tier_summary <- table(revenue_tiers)
##D 
##D barplot(tier_summary,
##D         main = "Games by Revenue Tier",
##D         xlab = "Revenue Tier",
##D         ylab = "Number of Games",
##D         col = "gold")
##D 
##D # Year-over-year growth analysis
##D last_year <- as.Date("2024-01-15") - 365
##D revenue_ly <- vgi_revenue_by_date(as.character(last_year))
##D 
##D yoy <- merge(revenue_data, revenue_ly,
##D             by = "steamAppId",
##D             suffixes = c("_now", "_lastyear"))
##D yoy$yoy_growth <- ((yoy$revenue_now - yoy$revenue_lastyear) / 
##D                   yoy$revenue_lastyear) * 100
##D 
##D # Fastest growing games by revenue
##D min_revenue <- 100000  # Only games with substantial revenue
##D qualified <- yoy[yoy$revenue_lastyear >= min_revenue, ]
##D fastest_growing <- head(qualified[order(-qualified$yoy_growth), ], 20)
##D 
##D cat("Fastest growing games (YoY revenue):\n")
##D print(fastest_growing[, c("steamAppId", "revenue_now", "yoy_growth")])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_revenue_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_reviews_by_date")
### * vgi_reviews_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_reviews_by_date
### Title: Get Reviews Data by Date
### Aliases: vgi_reviews_by_date

### ** Examples

## Not run: 
##D # Get reviews for a specific date
##D reviews_data <- vgi_reviews_by_date("2024-01-15")
##D 
##D # Find games with most reviews on this date
##D top_reviewed <- head(reviews_data[order(-reviews_data$totalReviews), ], 20)
##D print(top_reviewed)
##D 
##D # Find games with best review ratios (min 100 reviews)
##D qualified <- reviews_data[reviews_data$totalReviews >= 100, ]
##D best_rated <- head(qualified[order(-qualified$positiveRatio), ], 20)
##D cat("Best rated games with 100+ reviews:\n")
##D print(best_rated[, c("steamAppId", "positiveRatio", "totalReviews")])
##D 
##D # Analyze review distribution
##D hist(reviews_data$positiveRatio[reviews_data$totalReviews >= 10],
##D      breaks = 20,
##D      main = "Distribution of Review Scores",
##D      xlab = "Positive Review Ratio",
##D      col = "lightblue")
##D abline(v = 0.7, col = "green", lwd = 2, lty = 2)
##D abline(v = 0.5, col = "orange", lwd = 2, lty = 2)
##D 
##D # Compare with previous date
##D prev_date <- as.Date("2024-01-15") - 1
##D prev_reviews <- vgi_reviews_by_date(as.character(prev_date))
##D 
##D # Merge to find daily changes
##D comparison <- merge(reviews_data, prev_reviews, 
##D                    by = "steamAppId", 
##D                    suffixes = c("_today", "_yesterday"))
##D 
##D # Calculate daily review additions
##D comparison$new_reviews <- comparison$totalReviews_today - comparison$totalReviews_yesterday
##D comparison$ratio_change <- comparison$positiveRatio_today - comparison$positiveRatio_yesterday
##D 
##D # Find games with biggest review changes
##D biggest_changes <- head(comparison[order(-abs(comparison$new_reviews)), ], 10)
##D cat("Games with most review activity:\n")
##D print(biggest_changes[, c("steamAppId", "new_reviews", "ratio_change")])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_reviews_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_search_games")
### * vgi_search_games

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_search_games
### Title: Search Games in Video Game Insights
### Aliases: vgi_search_games

### ** Examples

## Not run: 
##D # Ensure the VGI_AUTH_TOKEN environment variable is set
##D # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
##D 
##D # Search for games with "valve" in the title
##D valve_games <- vgi_search_games("valve")
##D print(valve_games)
##D 
##D # Search with more results
##D rpg_games <- vgi_search_games("rpg", limit = 50)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_search_games", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_steam_market_data")
### * vgi_steam_market_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_steam_market_data
### Title: Get Steam Market Data Analytics
### Aliases: vgi_steam_market_data

### ** Examples

## Not run: 
##D # Get Steam market analytics
##D market_data <- vgi_steam_market_data()
##D 
##D # Display key market metrics
##D cat("Total Steam Games:", format(market_data$totalGames, big.mark = ","), "\n")
##D cat("Total Revenue: $", format(market_data$totalRevenue, big.mark = ","), "\n")
##D cat("Average Game Price: $", round(market_data$averagePrice, 2), "\n")
##D cat("Games Released Last 30 Days:", market_data$gamesReleasedLast30Days, "\n")
##D 
##D # Analyze top genres
##D if (!is.null(market_data$topGenres)) {
##D   print("Top 5 Genres by Game Count:")
##D   print(head(market_data$topGenres, 5))
##D }
##D 
##D # Examine price distribution
##D if (!is.null(market_data$priceDistribution)) {
##D   barplot(market_data$priceDistribution$count,
##D           names.arg = market_data$priceDistribution$priceRange,
##D           main = "Steam Games Price Distribution",
##D           xlab = "Price Range",
##D           ylab = "Number of Games",
##D           col = "steelblue")
##D }
##D 
##D # Calculate market concentration
##D top_10_percent_games <- market_data$totalGames * 0.1
##D cat("The top 10% of games (", round(top_10_percent_games), 
##D     " games) likely generate 80%+ of revenue\n", sep = "")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_steam_market_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_top_countries")
### * vgi_top_countries

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_top_countries
### Title: Get Top Countries by Player Count
### Aliases: vgi_top_countries

### ** Examples

## Not run: 
##D # Get top countries for a game
##D top_countries <- vgi_top_countries(steam_app_id = 730)
##D 
##D # Display top 10 countries
##D head(top_countries, 10)
##D 
##D # Calculate cumulative percentage
##D top_countries$cumulative_pct <- cumsum(top_countries$percentage)
##D 
##D # Find how many countries make up 80% of players
##D countries_80pct <- which(top_countries$cumulative_pct >= 80)[1]
##D cat("80% of players come from top", countries_80pct, "countries\n")
##D 
##D # Create a bar chart of top 10 countries
##D top10 <- head(top_countries, 10)
##D barplot(top10$percentage, 
##D         names.arg = top10$countryName,
##D         las = 2,
##D         main = "Top 10 Countries by Player %",
##D         ylab = "Percentage of Players",
##D         col = "steelblue")
##D 
##D # Check for specific regions
##D eu_countries <- c("DE", "FR", "GB", "IT", "ES", "PL", "NL", "SE", "BE", "AT")
##D eu_players <- sum(top_countries$percentage[top_countries$country %in% eu_countries])
##D cat("EU player percentage:", round(eu_players, 1), "%\n")
##D 
##D # Identify emerging markets
##D emerging <- top_countries[top_countries$rank > 10 & top_countries$percentage > 1, ]
##D cat("Emerging markets (rank >10 but >1%):", nrow(emerging), "\n")
##D print(emerging)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_top_countries", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_top_games")
### * vgi_top_games

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_top_games
### Title: Get Top Games from Video Game Insights
### Aliases: vgi_top_games

### ** Examples

## Not run: 
##D # Ensure the VGI_AUTH_TOKEN environment variable is set
##D # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
##D 
##D # Get top 10 games by revenue
##D top_revenue <- vgi_top_games("revenue", limit = 10)
##D print(top_revenue)
##D 
##D # Get top Steam games by CCU for a specific date range
##D top_ccu_steam <- vgi_top_games(
##D   metric = "ccu",
##D   platform = "steam",
##D   start_date = "2024-01-01",
##D   end_date = "2024-01-31",
##D   limit = 50
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_top_games", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_top_wishlist_countries")
### * vgi_top_wishlist_countries

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_top_wishlist_countries
### Title: Get Top Countries by Wishlist Count
### Aliases: vgi_top_wishlist_countries

### ** Examples

## Not run: 
##D # Get top wishlist countries for a game
##D wishlist_countries <- vgi_top_wishlist_countries(steam_app_id = 892970)
##D 
##D # Display top 10 countries
##D head(wishlist_countries, 10)
##D 
##D # Compare with actual player distribution
##D player_countries <- vgi_top_countries(steam_app_id = 892970)
##D 
##D # Merge to compare wishlist vs player percentages
##D comparison <- merge(wishlist_countries, player_countries, 
##D                    by = "country", suffixes = c("_wishlist", "_player"))
##D 
##D # Calculate conversion potential
##D comparison$conversion_rate <- comparison$percentage_player / comparison$percentage_wishlist
##D comparison <- comparison[order(comparison$conversion_rate), ]
##D 
##D # Find underperforming countries (high wishlist, low players)
##D underperforming <- comparison[comparison$conversion_rate < 0.5, ]
##D cat("Countries with low wishlist conversion:\n")
##D print(underperforming[, c("countryName_wishlist", "percentage_wishlist", 
##D                          "percentage_player", "conversion_rate")])
##D 
##D # Calculate regional interest
##D asia_countries <- c("CN", "JP", "KR", "TW", "HK", "SG", "TH", "ID", "MY", "PH")
##D asia_wishlist_pct <- sum(wishlist_countries$percentage[
##D   wishlist_countries$country %in% asia_countries])
##D cat("Asia wishlist percentage:", round(asia_wishlist_pct, 1), "%\n")
##D 
##D # Visualize top 10 wishlist countries
##D top10 <- head(wishlist_countries, 10)
##D barplot(top10$percentage, 
##D         names.arg = top10$countryName,
##D         las = 2,
##D         main = "Top 10 Countries by Wishlist %",
##D         ylab = "Percentage of Wishlists",
##D         col = "darkgreen")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_top_wishlist_countries", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_units_sold_by_date")
### * vgi_units_sold_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_units_sold_by_date
### Title: Get Units Sold Data by Date
### Aliases: vgi_units_sold_by_date

### ** Examples

## Not run: 
##D # Get units sold data for a specific date
##D units_data <- vgi_units_sold_by_date("2024-01-15")
##D 
##D # Top 20 best-selling games
##D top_sellers <- head(units_data, 20)
##D cat("Top 20 best-selling games:\n")
##D print(top_sellers[, c("steamAppId", "unitsSold", "dailyUnits")])
##D 
##D # Calculate previous day's data for daily sales
##D prev_date <- as.Date("2024-01-15") - 1
##D units_prev <- vgi_units_sold_by_date(as.character(prev_date))
##D 
##D # Merge to calculate exact daily sales
##D daily_sales <- merge(units_data, units_prev,
##D                     by = "steamAppId",
##D                     suffixes = c("_today", "_yesterday"))
##D daily_sales$units_sold_today <- daily_sales$unitsSold_today - 
##D                                 daily_sales$unitsSold_yesterday
##D 
##D # Find games with highest daily sales
##D top_daily <- head(daily_sales[order(-daily_sales$units_sold_today), ], 20)
##D cat("Top 20 games by daily sales:\n")
##D print(top_daily[, c("steamAppId", "units_sold_today")])
##D 
##D # Analyze sales distribution
##D hist(log10(units_data$unitsSold + 1),
##D      breaks = 40,
##D      main = "Distribution of Total Units Sold (log scale)",
##D      xlab = "Log10(Units Sold + 1)",
##D      col = "darkgreen")
##D 
##D # Sales velocity analysis
##D units_data$sales_per_day <- units_data$unitsSold / 
##D   as.numeric(as.Date("2024-01-15") - as.Date("2020-01-01"))
##D 
##D # Games with sustained high sales velocity
##D high_velocity <- units_data[units_data$sales_per_day > 100 & 
##D                            units_data$unitsSold > 100000, ]
##D cat("Games averaging >100 sales/day:", nrow(high_velocity), "\n")
##D 
##D # Compare with revenue data for average price calculation
##D revenue_data <- vgi_revenue_by_date("2024-01-15")
##D pricing <- merge(units_data, revenue_data, by = "steamAppId")
##D pricing$avg_price <- pricing$revenue / (pricing$unitsSold + 1)
##D 
##D # Find premium-priced successful games
##D premium_games <- pricing[pricing$avg_price > 40 & 
##D                         pricing$unitsSold > 50000, ]
##D cat("Premium games (>$40) with >50k sales:", nrow(premium_games), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_units_sold_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("vgi_wishlists_by_date")
### * vgi_wishlists_by_date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: vgi_wishlists_by_date
### Title: Get Wishlists Data by Date
### Aliases: vgi_wishlists_by_date

### ** Examples

## Not run: 
##D # Get wishlist data for a specific date
##D wishlists <- vgi_wishlists_by_date("2024-01-15")
##D 
##D # Top 20 most wishlisted games
##D top_wishlisted <- head(wishlists, 20)
##D print(top_wishlisted)
##D 
##D # Track weekly wishlist changes
##D week_ago <- as.Date("2024-01-15") - 7
##D wishlists_prev <- vgi_wishlists_by_date(as.character(week_ago))
##D 
##D # Calculate weekly growth
##D growth <- merge(wishlists, wishlists_prev, 
##D                by = "steamAppId", 
##D                suffixes = c("_now", "_prev"))
##D growth$weekly_change <- growth$wishlistCount_now - growth$wishlistCount_prev
##D growth$weekly_pct <- (growth$weekly_change / growth$wishlistCount_prev) * 100
##D 
##D # Find fastest growing games
##D min_wishlists <- 1000  # Only games with substantial wishlists
##D qualified <- growth[growth$wishlistCount_prev >= min_wishlists, ]
##D fastest_growing <- head(qualified[order(-qualified$weekly_pct), ], 20)
##D 
##D cat("Fastest growing games (>1000 wishlists):\n")
##D print(fastest_growing[, c("steamAppId", "wishlistCount_now", 
##D                          "weekly_change", "weekly_pct")])
##D 
##D # Analyze wishlist distribution
##D hist(log10(wishlists$wishlistCount + 1),
##D      breaks = 30,
##D      main = "Distribution of Wishlist Counts (log scale)",
##D      xlab = "Log10(Wishlist Count + 1)",
##D      col = "lightgreen")
##D 
##D # Find games losing wishlists
##D declining <- growth[growth$weekly_change < -100, ]
##D cat("Games losing 100+ wishlists this week:", nrow(declining), "\n")
##D 
##D # Seasonal analysis (if you have historical data)
##D # Compare with same date last year
##D last_year <- as.Date("2024-01-15") - 365
##D wishlists_ly <- vgi_wishlists_by_date(as.character(last_year))
##D 
##D yoy <- merge(wishlists, wishlists_ly,
##D             by = "steamAppId",
##D             suffixes = c("_now", "_lastyear"))
##D yoy$yoy_growth <- ((yoy$wishlistCount_now - yoy$wishlistCount_lastyear) / 
##D                   yoy$wishlistCount_lastyear) * 100
##D 
##D # Find games with massive YoY growth
##D breakout <- yoy[yoy$yoy_growth > 1000 & yoy$wishlistCount_now > 10000, ]
##D cat("Breakout games (>1000% YoY growth, >10k wishlists):", nrow(breakout), "\n")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("vgi_wishlists_by_date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
