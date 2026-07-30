# searchnet 0.3.1

## Performance

* `searchnet_game_step()` no longer recomputes choice probabilities once per
  actor. `compute_choice_probabilities()` evaluates all `M` actors in a single
  pass, but it was being called inside the actor loop with all but one element
  discarded each time — `M` times the necessary work, giving roughly `M^2 * N`
  scaling. The call is now hoisted out of the loop.

  Measured (5 moves incl. `searchnet_game_available_moves()`, R 4.5.3):

  | Size | Before | After | Speedup |
  |------|--------|-------|---------|
  | M=8, N=12 | 0.78 s/move | 0.234 s/move | ~3.3x |
  | M=20, N=30 | 18.23 s/move | 1.358 s/move | ~13x |

  This makes turn-based interactive play viable at experiment-relevant sizes.

## Behavior change

* As a consequence of the above, AI opponents in `searchnet_game_step()` now
  all respond to the same round-start board state (after the player's move),
  i.e. **simultaneous moves within a round**. Previously each AI observed the
  partially-updated board left by lower-indexed actors, which made round
  outcomes depend on actor ordering.

  This matches the semantics already used by `searchnet_classroom_advance()`
  and is the correct behavior for controlled experimental play. Games seeded
  identically will not reproduce pre-0.3.1 trajectories.

# searchnet 0.3.0

* Added classic NK module and cross-sectional regression discontinuity.

# searchnet 0.2.0

* Initial public pre-release.
