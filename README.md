# Fall Folks - A Code-Centric Talk Idea

## The Inspiration
Fall Guys is an online multiplayer game, presented in a number of rounds, with
a 'last player standing' format. It's a Knockout game.

There's a round in that game that's operates as, "memorize these cards before
we turn them over and test you."

Players stand on an NxN grid of tiles. The players spawn in on a random tile,
and can freely walk to any other tile.

The tiles have one of a set of logos on them, and the players are expected to
memorize where to find a nearby copy of each logo, because when the timer runs
out, every tile will go blank, then a logo will be displayed on screen, and
they will have 5 seconds to stand on a tile that used to display that logo.
After 5 seconds, players on a tile that matched the screen get a point, and all
other players fall to their death.

# The Talk

Write up that game a web-based variant. Make it event-based. Write a talk based
on what you learnt.

# The Events

```purescript
data ClientEvent
  = Move PlayerId < Up | Down | Left | Right >
  | SetName PlayerId String

data ServerEvent
  = PlayersMoved (Set { PlayerId, Position }) -- | Delta. We only send players that have changed position.
  | Leaderboard (List { PlayerId, Score } )

-- Frontend should implement a time to live sweep. If you haven't seen a player
--   move in N seconds, stop tracking them.
```
