# Video Game Insights API Migration Guide

## Overview
The Video Game Insights API has been restructured. This document maps the old endpoints to the new ones.

## Endpoint Mappings

### Game Metadata
- **OLD**: `/games/metadata?steam_app_id={id}` - Get single game metadata
- **NEW**: `/games/{steamAppId}/metadata` - Get single game metadata
- **Status**: ✅ Updated in `vgi_game_metadata()`

- **OLD**: `/games/metadata/batch` (POST) - Get multiple games metadata
- **NEW**: No direct replacement, must call individual endpoints
- **Status**: ✅ Updated in `vgi_game_metadata_batch()` to make multiple calls

### Game Search
- **OLD**: `/games/search?q={query}` - Search games
- **NEW**: `/games/game-list` - Get all games (must filter client-side)
- **Status**: ✅ Updated in `vgi_search_games()` to fetch and filter locally

### Revenue Data
- **OLD**: `/insights/revenue` - Get revenue data with filters
- **NEW**: `/commercial-performance/revenue/games/{steamAppId}` - Get revenue history
- **Status**: ✅ Updated in `vgi_insights_revenue()`

### Concurrent Users (CCU)
- **OLD**: `/insights/ccu` - Get CCU data with filters
- **NEW**: `/engagement/concurrent-players/games/{steamAppId}` - Get player history
- **Status**: ✅ Updated in `vgi_insights_ccu()`

### Units Sold
- **OLD**: `/insights/units` - Get units sold data
- **NEW**: `/commercial-performance/units-sold/games/{steamAppId}` - Get units history
- **Status**: ✅ Updated in `vgi_insights_units()`

### Reviews
- **OLD**: `/insights/reviews` - Get review data
- **NEW**: `/reception/reviews/games/{steamAppId}` - Get review analytics
- **Status**: ✅ Updated in `vgi_insights_reviews()`

### Price History
- **OLD**: `/insights/price_history` - Get price history
- **NEW**: `/commercial-performance/price-history/games/{steamAppId}` - Get price history
- **Status**: ✅ Updated in `vgi_insights_price_history()`

### DAU/MAU
- **OLD**: `/insights/dau_mau` - Get DAU/MAU data
- **NEW**: `/engagement/active-players/games/{steamAppId}` - Get active player data
- **Status**: ✅ Updated in `vgi_insights_dau_mau()`

### Playtime
- **OLD**: `/insights/playtime` - Get playtime data
- **NEW**: `/player-insights/games/{steamAppId}/playtime` - Get playtime stats
- **Status**: ✅ Updated in `vgi_insights_playtime()`

### Player Regions
- **OLD**: `/insights/player_regions` - Get player geographic data
- **NEW**: `/player-insights/games/{steamAppId}/regions` - Get player regions
- **Status**: ✅ Updated in `vgi_insights_player_regions()`

### Developers
- **OLD**: `/developers/{developer_id}` - Get developer info
- **NEW**: `/developers/{companyId}` - Get developer details
- **Status**: ✅ Updated in `vgi_developer_info()`

- **OLD**: `/developers/{developer_id}/games` - Get developer's games
- **NEW**: `/developers/{companyId}/game-ids` - Get developer's game IDs
- **Status**: ✅ Updated in `vgi_developer_games()` (returns IDs only now)

### Publishers
- **OLD**: `/publishers/{publisher_id}` - Get publisher info
- **NEW**: `/publishers/{companyId}` - Get publisher details
- **Status**: ✅ Updated in `vgi_publisher_info()`

- **OLD**: `/publishers/{publisher_id}/games` - Get publisher's games
- **NEW**: `/publishers/{companyId}/game-ids` - Get publisher's game IDs
- **Status**: ✅ Updated in `vgi_publisher_games()` (returns IDs only now)

## New Endpoints (Not Previously Implemented)
- `/games/rankings` - Get game rankings - ✅ Implemented in `vgi_game_rankings()`
- `/analytics/steam-market-data` - Get Steam market analytics - ✅ Implemented in `vgi_steam_market_data()`
- `/interest-level/wishlists/games/{steamAppId}` - Get wishlist data - ✅ Implemented in `vgi_insights_wishlists()`
- `/interest-level/followers/games/{steamAppId}` - Get follower data - ✅ Implemented in `vgi_insights_followers()`
- `/player-insights/games/{steamAppId}/player-overlap` - Get player overlap - ✅ Implemented in `vgi_player_overlap()`
- `/historical-data/games/{steamAppId}` - Get historical game data - ✅ Implemented in `vgi_historical_data()`

## Key Changes
1. **No Batch Endpoints**: Must make individual calls for multiple items
2. **Path Parameters**: Most endpoints now use path parameters instead of query parameters
3. **Simplified Responses**: Some endpoints return less data (e.g., game IDs only)
4. **Date-based Endpoints**: New endpoints allow fetching data by date for all games
5. **Restructured Categories**: 
   - `insights/*` → `commercial-performance/*`, `engagement/*`, `player-insights/*`
   - More granular categorization of endpoints

## Implementation Strategy
1. Update existing functions to use new endpoints
2. Maintain backward compatibility where possible
3. Add caching for frequently accessed data (e.g., game list)
4. Consider implementing new endpoints as separate functions