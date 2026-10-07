# VGI API coverage audit (2026-10-07)

Source of truth: the OpenAPI documents served by Sensor Tower at
`https://app.sensortower.com/vgi/api/v3/api-docs/` and
`https://app.sensortower.com/vgi/api/v4/api-docs/` (copies in `dev/specs/`).
Live base URLs: `https://vginsights.com/api/v3` and `/v4` (301 to
`app.sensortower.com/vgi/api`). Auth header: `api-key`.

Pagination: v3 list endpoints take `offset`/`limit` and return bare arrays;
v4 endpoints take `cursor`/`limit` and return `{ nextCursor, results }`.
v4 is multi-platform (`platform` + `externalId`, `vgiId`, `slug`); v4 has no
commercial-performance, engagement, reception or interest-level endpoints.

Per-game functions and versions. Commit 24f08e3 moved the per-game time
series (`vgi_historical_data()`, `vgi_insights_{ccu,dau_mau,revenue,units,
reviews,wishlists,followers,price_history}()`) and the per-game player
insights (`vgi_insights_playtime()`, `vgi_insights_player_regions()`,
`vgi_top_countries()`, `vgi_top_wishlist_countries()`) to v4. They stay on v4
by default and gain `version = "v3"` for the per-game v3 endpoints:

- v4 `/historical-data?steamAppIds=<id>` (no `date`) returns the full daily
  history (Dressmaker: 385 world-wide rows from 2025-09-17) in one response.
  With identifier filters the API ignores `cursor` and repeats the same page
  and `nextCursor`; 0.1.1 followed that cursor once and returned every row
  twice (768 CCU rows for 384 days). `vgi_insights_units()` and
  `vgi_insights_revenue()` also errored in 0.1.1. Both are fixed.
- v3 per-game endpoints are all live (200). Their series start nearer release
  (Dressmaker CCU from 2026-09-21) and can be one day fresher. Figures agree
  with v4 for the same date (units 2026-10-06: 283,970 in both).
- Exception: `vgi_player_overlap()` keeps v3 as default. v4 `/player-overlap`
  lists overlapping games but returns only null percentages (checked for
  Steam 4019220, 730, 892970), while v3 returns counts, percentages and
  indices.

v4 field renames handled: `premiumRevenue*` (was `revenue*`), `unitsOwned*`
(was `unitsSold*`); the deprecated names are used only as a fallback.

## v3 endpoints -> package functions

| v3 endpoint | Function(s) | Status |
|---|---|---|
| `GET /analytics/steam-market-data` | `vgi_steam_market_data(version = "v3")` | implemented |
| `GET /commercial-performance/price-history/games/{steamAppId}` | `vgi_insights_price_history(id, version = "v3")` | implemented (v4 default uses `/price-history`) |
| `GET /commercial-performance/price-history/games/{steamAppId}/{currency}` | `vgi_insights_price_history(id, currency, version = "v3")` | implemented |
| `GET /commercial-performance/revenue/games/{steamAppId}` | `vgi_insights_revenue(version = "v3")` | implemented |
| `GET /commercial-performance/revenue/{date}` | `vgi_snapshot_by_date(metric = "revenue")` | new |
| `GET /commercial-performance/units-sold/games/{steamAppId}` | `vgi_insights_units(version = "v3")` (+ deprecated `vgi_insights_entitlements()`) | implemented |
| `GET /commercial-performance/units-sold/{date}` | `vgi_snapshot_by_date("units-sold")` | new |
| `GET /developers` | `vgi_developers()` | new |
| `GET /developers/developer-list` | `vgi_developer_list(version = "v3")` | new |
| `GET /developers/game-ids` | `vgi_all_developer_games(version = "v3")` | new |
| `GET /developers/{companyId}` | `vgi_developer_info()` | restored (v4 version returned VGI ids) |
| `GET /developers/{companyId}/game-ids` | `vgi_developer_games()` | restored |
| `GET /engagement/active-players/games/{steamAppId}` | `vgi_insights_dau_mau(version = "v3")` | implemented |
| `GET /engagement/active-players/{date}` | `vgi_snapshot_by_date("active-players")` | new |
| `GET /engagement/concurrent-players/games/{steamAppId}` | `vgi_insights_ccu(version = "v3")` | implemented |
| `GET /engagement/concurrent-players/{date}` | `vgi_snapshot_by_date("concurrent-players")` | new |
| `GET /games/game-list` | `vgi_game_list(version = "v3")` | implemented |
| `GET /games/metadata` | `vgi_all_games_metadata()` | restored (offset/limit semantics) |
| `GET /games/rankings` | `vgi_game_rankings()`, `vgi_top_games()` | restored (was recomputed from snapshot) |
| `GET /games/{steamAppId}/metadata` | `vgi_game_metadata()` | restored |
| `GET /games/{steamAppId}/rankings` | `vgi_game_rankings(steam_app_id = )` | new |
| `GET /historical-data/games/{steamAppId}` | `vgi_historical_data(version = "v3")` | implemented |
| `GET /historical-data/{date}` | `vgi_snapshot_by_date("historical-data")` | new |
| `GET /interest-level/followers/games/{steamAppId}` | `vgi_insights_followers(version = "v3")` | implemented |
| `GET /interest-level/followers/{date}` | `vgi_snapshot_by_date("followers")` | new |
| `GET /interest-level/wishlists/games/{steamAppId}` | `vgi_insights_wishlists(version = "v3")` | implemented |
| `GET /interest-level/wishlists/{date}` | `vgi_snapshot_by_date("wishlists")` | new |
| `GET /player-insights/games/player-overlap` | `vgi_all_games_player_overlap()` | restored (v3; v4 endpoint is single-game) |
| `GET /player-insights/games/playtime` | — (v4 `vgi_all_games_playtime()` covers the multi-game case) | not wrapped |
| `GET /player-insights/games/regions` | — (v4 `vgi_all_games_regions()`) | not wrapped |
| `GET /player-insights/games/top-countries` | — (v4 `vgi_all_games_top_countries()`) | not wrapped |
| `GET /player-insights/games/top-wishlist-countries` | — (v4 `vgi_all_games_wishlist_countries()`) | not wrapped |
| `GET /player-insights/games/{steamAppId}/player-overlap` | `vgi_player_overlap()` | restored (default; v4 returns null figures) |
| `GET /player-insights/games/{steamAppId}/playtime` | `vgi_insights_playtime(version = "v3")` | implemented |
| `GET /player-insights/games/{steamAppId}/regions` | `vgi_insights_player_regions(version = "v3")`, `vgi_top_regions(version = "v3")` | implemented |
| `GET /player-insights/games/{steamAppId}/top-countries` | `vgi_top_countries(version = "v3")` | implemented |
| `GET /player-insights/games/{steamAppId}/top-wishlist-countries` | `vgi_top_wishlist_countries(version = "v3")` | implemented |
| `GET /publishers` | `vgi_publishers()` | new |
| `GET /publishers/game-ids` | `vgi_all_publisher_games(version = "v3")` | new |
| `GET /publishers/publisher-list` | `vgi_publisher_list(version = "v3")` | new |
| `GET /publishers/{companyId}` | `vgi_publisher_info()` | restored |
| `GET /publishers/{companyId}/game-ids` | `vgi_publisher_games()` | restored |
| `GET /reception/reviews/games/{steamAppId}` | `vgi_insights_reviews(version = "v3")` | implemented |
| `GET /reception/reviews/{date}` | `vgi_snapshot_by_date("reviews")` | new |

The four unwrapped v3 catalogue endpoints (`/player-insights/games/{playtime,
regions,top-countries,top-wishlist-countries}`) are superseded by their v4
equivalents, which add identifier filters and cursor paging; they were left
out deliberately.

## v4 endpoints -> package functions

| v4 endpoint | Function(s) | Status |
|---|---|---|
| `GET /companies/developers` | `vgi_developers_overview()` | implemented (+ `all_pages`) |
| `GET /companies/developers/game-ids` | `vgi_all_developer_games()` | implemented (+ filters, `all_pages`) |
| `GET /companies/developers/list` | `vgi_developer_list()` | implemented (+ filters, `all_pages`) |
| `GET /companies/publishers` | `vgi_publishers_overview()` | implemented (+ `all_pages`) |
| `GET /companies/publishers/game-ids` | `vgi_all_publisher_games()` | implemented (+ filters, `all_pages`) |
| `GET /companies/publishers/list` | `vgi_publisher_list()` | implemented (+ filters, `all_pages`) |
| `GET /games/game-list` | `vgi_game_list()` | implemented (adds `vgi_id`) |
| `GET /games/metadata` | `vgi_games_metadata()` | new (steamAppIds / vgiIds / slugs, cursor) |
| `GET /historical-data` | `vgi_historical_data_by_date()`; `vgi_*_by_date()` wrappers (with `date`); `vgi_historical_data()` and the `vgi_insights_*()` series (with `steamAppIds`, default) | new / implemented |
| `GET /market-data` | `vgi_steam_market_data()` | implemented |
| `GET /player-insights/games/playtime` | `vgi_all_games_playtime()`; `vgi_insights_playtime()` (default) | implemented (+ ids, `countries`, `regions`) |
| `GET /player-insights/games/top-countries` | `vgi_all_games_top_countries()`; `vgi_top_countries()` (default) | implemented (+ ids) |
| `GET /player-insights/games/top-regions` | `vgi_all_games_regions()`; `vgi_insights_player_regions()`, `vgi_top_regions()` (default) | implemented (+ ids) |
| `GET /player-insights/games/top-wishlist-countries` | `vgi_all_games_wishlist_countries()`; `vgi_top_wishlist_countries()` (default) | implemented (+ ids) |
| `GET /player-overlap` | `vgi_player_overlap(vgi_id= / slug= / version = "v4")` | new (API returns null figures as of 2026-10) |
| `GET /price-history` | `vgi_price_history()`; `vgi_insights_price_history()` (default, Steam rows) | new |

## Exported functions -> endpoints

| Function | Endpoint (version) | Notes |
|---|---|---|
| `vgi_active_players_by_date()` | v4 `/historical-data?date` | |
| `vgi_all_developer_games()` | v4 `/companies/developers/game-ids`; v3 `/developers/game-ids` | |
| `vgi_all_games_metadata()` | v3 `/games/metadata` | |
| `vgi_all_games_player_overlap()` | v3 `/player-insights/games/player-overlap` | |
| `vgi_all_games_playtime()` | v4 `/player-insights/games/playtime` | |
| `vgi_all_games_regions()` | v4 `/player-insights/games/top-regions` | |
| `vgi_all_games_top_countries()` | v4 `/player-insights/games/top-countries` | |
| `vgi_all_games_wishlist_countries()` | v4 `/player-insights/games/top-wishlist-countries` | |
| `vgi_all_publisher_games()` | v4 `/companies/publishers/game-ids`; v3 `/publishers/game-ids` | |
| `vgi_auth_check()` | v3 `/games/game-list` | |
| `vgi_concurrent_players_by_date()` | v4 `/historical-data?date` | |
| `vgi_developer_games()` | v3 `/developers/{companyId}/game-ids` | |
| `vgi_developer_info()` | v3 `/developers/{companyId}` | |
| `vgi_developer_list()` | v4 `/companies/developers/list`; v3 `/developers/developer-list` | |
| `vgi_developers()` | v3 `/developers` | new |
| `vgi_developers_overview()` | v4 `/companies/developers` | |
| `vgi_entitlements_by_date()` | alias of `vgi_units_sold_by_date()` | deprecated (no API endpoint) |
| `vgi_followers_by_date()` | v4 `/historical-data?date` | |
| `vgi_game_list()` | v4 / v3 `/games/game-list` | |
| `vgi_game_metadata()` | v3 `/games/{steamAppId}/metadata` | |
| `vgi_game_metadata_batch()` | v3 `/games/{steamAppId}/metadata` (per id) | |
| `vgi_game_rankings()` | v3 `/games/rankings`, `/games/{steamAppId}/rankings` | `date` deprecated |
| `vgi_games_metadata()` | v4 `/games/metadata` | new |
| `vgi_historical_data()` | v4 `/historical-data?steamAppIds`; v3 `/historical-data/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_historical_data_by_date()` | v4 `/historical-data` | new |
| `vgi_insights_ccu()` | v4 `/historical-data?steamAppIds`; v3 `/engagement/concurrent-players/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_insights_dau_mau()` | v4 `/historical-data?steamAppIds`; v3 `/engagement/active-players/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_insights_entitlements()` | alias of `vgi_insights_units()` | deprecated |
| `vgi_insights_followers()` | v4 `/historical-data?steamAppIds`; v3 `/interest-level/followers/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_insights_player_regions()` | v4 `/player-insights/games/top-regions`; v3 `/player-insights/games/{steamAppId}/regions` | v4 default, `version = "v3"` |
| `vgi_insights_playtime()` | v4 `/player-insights/games/playtime`; v3 `/player-insights/games/{steamAppId}/playtime` | v4 default, `version = "v3"` |
| `vgi_insights_price_history()` | v4 `/price-history`; v3 `/commercial-performance/price-history/games/{steamAppId}[/{currency}]` | v4 default, `version = "v3"` |
| `vgi_insights_revenue()` | v4 `/historical-data?steamAppIds`; v3 `/commercial-performance/revenue/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_insights_reviews()` | v4 `/historical-data?steamAppIds`; v3 `/reception/reviews/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_insights_units()` | v4 `/historical-data?steamAppIds`; v3 `/commercial-performance/units-sold/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_insights_wishlists()` | v4 `/historical-data?steamAppIds`; v3 `/interest-level/wishlists/games/{steamAppId}` | v4 default, `version = "v3"` |
| `vgi_player_overlap()` | v3 `/player-insights/games/{steamAppId}/player-overlap`; v4 `/player-overlap` | v3 default |
| `vgi_price_history()` | v4 `/price-history` | new |
| `vgi_publisher_games()` | v3 `/publishers/{companyId}/game-ids` | |
| `vgi_publisher_info()` | v3 `/publishers/{companyId}` | |
| `vgi_publisher_list()` | v4 `/companies/publishers/list`; v3 `/publishers/publisher-list` | |
| `vgi_publishers()` | v3 `/publishers` | new |
| `vgi_publishers_overview()` | v4 `/companies/publishers` | |
| `vgi_revenue_by_date()` | v4 `/historical-data?date` | |
| `vgi_reviews_by_date()` | v4 `/historical-data?date` | |
| `vgi_snapshot_by_date()` | v3 `/{metric}/{date}` family | new |
| `vgi_steam_market_data()` | v4 `/market-data`; v3 `/analytics/steam-market-data` | |
| `vgi_top_countries()` | v4 `/player-insights/games/top-countries`; v3 `/player-insights/games/{steamAppId}/top-countries` | v4 default, `version = "v3"` |
| `vgi_top_games()` | v3 `/games/rankings` + metadata | |
| `vgi_top_regions()` | v4 `/player-insights/games/top-regions`; v3 `/player-insights/games/{steamAppId}/regions` | v4 default, `version = "v3"` |
| `vgi_top_wishlist_countries()` | v4 `/player-insights/games/top-wishlist-countries`; v3 `/player-insights/games/{steamAppId}/top-wishlist-countries` | v4 default, `version = "v3"` |
| `vgi_units_sold_by_date()` | v4 `/historical-data?date` | |
| `vgi_wishlists_by_date()` | v4 `/historical-data?date` | |
| `vgi_search_games()`, `vgi_smart_game_search()`, `vgi_game_summary()`, `vgi_game_summary_yoy()`, `vgi_peak_ccu_*()`, `vgi_fetch_*()`, `vgi_smart_search()`, `vgi_top_games_with_activity()`, cache and rate-limit helpers | compositions of the functions above | no direct endpoint |
