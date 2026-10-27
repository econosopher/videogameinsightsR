# VGI API coverage (v4 only, 2026-10-27)

Source of truth: the OpenAPI document served by Sensor Tower at
`https://app.sensortower.com/vgi/api/v4/api-docs/` (copy in
`dev/specs/vgi_v4_openapi.json`). Live base URL: `https://vginsights.com/api/v4`
(301 to `app.sensortower.com/vgi/api/v4`). Auth header: `api-key`.

Since 0.3.0 the package calls only v4. v4 endpoints take `cursor`/`limit` and
return `{ nextCursor, results }`; rows are multi-platform (`platform` +
`externalId`, `vgiId`, `slug`). With identifier filters (`steamAppIds`,
`vgiIds`, `slugs`) the API ignores `cursor` and repeats the same page, so
`.vgi_fetch_v4_pages()` stops when `nextCursor` repeats.

Live checks on 2026-10-27:

- `/player-overlap?steamAppId=..&limit=1` returns full overlap figures
  (counts, percentages, indices). `limit` must be 1; the catalogue is walked
  with `cursor`.
- `/companies/{developers,publishers}?vgiIds=` uses the same company IDs as
  the old v3 `/developers/{id}`; `platformData.steam` carries the same Steam
  revenue figures. Publishers carry `gamesReleased`, developers also
  `gamesDeveloped`.
- `/companies/*/game-ids` returns VGI game IDs on every platform; Steam App IDs
  come from `/games/metadata?vgiIds=` (`storeUrl.steam`).
- `/games/metadata?steamAppIds=999999999` answers 404 "Requested game IDs are
  not valid VGI IDs".
- There is no rankings endpoint in v4, so `vgi_game_rankings()`,
  `vgi_top_games()` and the helpers built on them were removed.

v4 field names: `premiumRevenue*` (was `revenue*`), `unitsOwned*` (was
`unitsSold*`), `paidUnits*` (excludes free copies; on Xbox, Game Pass);
the deprecated names are read only as a fallback.

## v4 endpoints -> package functions

| v4 endpoint | Function(s) |
|---|---|
| `GET /games/game-list` | `vgi_game_list()`, `vgi_auth_check()`, `vgi_fetch_all_games()` |
| `GET /games/metadata` | `vgi_games_metadata()`, `vgi_game_metadata()`, `vgi_game_metadata_batch()`, `vgi_build_cache()`, `vgi_search_games()` (slug fallback), `vgi_*_games()` (VGI -> Steam ids) |
| `GET /historical-data` (with `date`) | `vgi_historical_data_by_date()`, `vgi_{active_players,concurrent_players,revenue,units_sold,reviews,wishlists,followers}_by_date()` |
| `GET /historical-data` (with `steamAppIds`, no `date`) | `vgi_historical_data()`, `vgi_insights_{ccu,dau_mau,revenue,units,reviews,wishlists,followers}()` |
| `GET /price-history` | `vgi_price_history()`, `vgi_insights_price_history()` (Steam rows) |
| `GET /market-data` | `vgi_steam_market_data()` |
| `GET /player-insights/games/playtime` | `vgi_all_games_playtime()`, `vgi_insights_playtime()` |
| `GET /player-insights/games/top-countries` | `vgi_all_games_top_countries()`, `vgi_top_countries()` |
| `GET /player-insights/games/top-regions` | `vgi_all_games_regions()`, `vgi_insights_player_regions()`, `vgi_top_regions()` |
| `GET /player-insights/games/top-wishlist-countries` | `vgi_all_games_wishlist_countries()`, `vgi_top_wishlist_countries()` |
| `GET /player-overlap` | `vgi_player_overlap()`, `vgi_all_games_player_overlap()` |
| `GET /companies/publishers` | `vgi_publishers_overview()`, `vgi_publisher_info()` |
| `GET /companies/developers` | `vgi_developers_overview()`, `vgi_developer_info()` |
| `GET /companies/publishers/list` | `vgi_publisher_list()` |
| `GET /companies/developers/list` | `vgi_developer_list()` |
| `GET /companies/publishers/game-ids` | `vgi_all_publisher_games()`, `vgi_publisher_games()` |
| `GET /companies/developers/game-ids` | `vgi_all_developer_games()`, `vgi_developer_games()` |

`vgi_game_summary()`, `vgi_game_summary_yoy()`, `vgi_peak_ccu_*()`,
`vgi_smart_game_search()`, `vgi_fetch_by_ids()`, the cache helpers and the
rate-limit helpers are compositions of the functions above.
