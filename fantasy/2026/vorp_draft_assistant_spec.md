# Product Spec: Fantasy Draft VORP + VONA Assistant

## 1. Summary

A local Python CLI script for real-time draft decisions. You run it each time you're on the clock; it shows available players ranked by two complementary metrics:

- **VORP** (Value Over Replacement Player) — *should I care about this position at all?* Stable, season-long positional scarcity signal.
- **VONA** (Value Over Next Available) — *can I wait a round on this position?* Volatile, pick-to-pick "can I wait" signal.

Single user, runs entirely from local files, no network calls during the draft. **Top priority: it must not break during the draft.**

---

## 2. Why VORP alone isn't enough (and why VONA is the second column, not a replacement)

- **VORP** answers: how much better is this player than the worst startable player at his position, league-wide, for the whole season? It's your guide to overall positional priority — stable and doesn't whipsaw pick to pick.
- **VONA** answers: how much better is this player than whoever will likely still be there at my *next* pick? It's what actually drives "take him now or gamble on next round," and by nature it's a constantly-shifting number that depends on the draft's live flow.

Using VONA alone would make the tool jumpy and would drop the season-long scarcity signal that keeps you from overreacting to short-term positional runs. Using VORP alone (the original spec) ignores real turn-by-turn opportunity cost. **Both are shown, side by side, with VORP as the primary sort.**

---

## 3. Deployment format: CLI script

Confirmed. No live-refreshing UI, no browser automation, no scraping. If something breaks, you re-run the script and lose 10 seconds, not your pick.

---

## 4. League settings (confirmed)

- 14 teams, snake draft
- 0.5 PPR scoring
- **Starting lineup:** QB (1), OP/Superflex — any offensive player including QB (1), RB (2), WR (2), TE (1), DST (1), K (1) = 9 starters/team
- **Bench:** 4 BE + 1 IR (not used in replacement-level math — see Section 6.1)
- **Only one flex-type slot** (OP/Superflex). No separate RB/WR/TE flex. This means there's a single spillover pool — QB/RB/WR/TE all competing for the 14 league-wide OP slots — rather than two separate flex pools.

---

## 5. Data inputs

### 5.1 Player projections & rankings (received)
Two files, half-PPR, OP/superflex-native (confirmed by Josh Allen ranking #1 overall — QB premium is already baked in, so no separate superflex-ADP sourcing problem):

| File | Key columns used |
|---|---|
| `HALF_PPR_PROJPOINTS_..._OP_Rankings.csv` | `PLAYER NAME`, `TEAM`, `FANTASYPTS` (0.5 PPR points, no conversion needed) |
| `HALF_PPR_RANK_..._OP_Rankings.csv` | `PLAYER NAME`, `POS` (e.g. `QB1`, `RB23` — position + positional rank combined, needs splitting), `TIERS`, `RK`, `BYE WEEK` |

**Parsing notes for the build:**
- The projections file has **duplicate column headers** (`YDS`/`TDS` repeated 3x for pass/rec/rush) — must be parsed by column *index*, not header name.
- `POS` needs to be split into position (letters) + positional rank (digits), e.g. `RB23` → `RB`, `23`.
- Join key is `PLAYER NAME` + `TEAM` across the two files (name-only risks collisions with duplicate names).
- Coverage: 67 QB / 249 RB / 408 WR / 222 TE. No K/DST — separate file incoming (Section 5.2).

### 5.2 K & DST — ADP received, points projections pending upload
`FantasyPros_2026_K_ADP_Rankings.csv` and `FantasyPros_2026_DST_ADP_Rankings.csv` are in — but both are ADP-only (draft order across ESPN/Yahoo/CBS/Fantrax/Sleeper/RTSports), with no `FANTASYPTS`-equivalent column. These cover VONA for K/DST. VORP still needs actual point projections, which you'll upload separately.

### 5.3 ADP — received (`FantasyPros_2026_Superflex_ADP_Rankings.csv`)
274 rows, confirmed superflex-native. **Important parsing detail:** this file has two different ADP-flavored columns that look similar but aren't:
- `Overall` — tracks a **standard 1-QB league** ADP (reference only; Josh Allen shows `Overall` 19, Lamar Jackson 37 — far too late for a superflex league).
- `OP` (integer rank) and `AVG` (decimal, composite across Sleeper/FFPC/Real-Time) — the **actual superflex-adjusted** values (Josh Allen `OP` 3 / `AVG` 2.5, Lamar `OP` 8 / `AVG` 8.0).

**Build decision: use `AVG` as the ADP value feeding VONA.** `Overall` is not used in any calculation — display only, if at all, and clearly labeled as "standard ADP for reference" to avoid confusing it with the real number.

Also needs the same name-parsing treatment as the K/DST files: `Player (Bye)` is a combined field like `Jahmyr Gibbs   DET (6)` (double-space-separated name/team, bye in parens) or, for DST, just `Houston Texans   (8)` with no team code. `Real-Time` values sometimes carry a trailing movement indicator (e.g. `8  -2`) that needs stripping before parsing as a number.

Coverage is 274 players — beyond that, players have no real ADP. For VONA on deep bench-level players outside ADP coverage, fall back to `TIERS` from the rank file as the drop-off signal (as originally planned in Section 6.2).

### 5.4 ESPN projected offensive rankings (received)
`2026_NFL_Unit_Grades_1.csv` — team-level (32 rows), joined via `TEAM` abbreviation. Used as a **display column only** (`Off Grade`, `Off Rank`), not part of the VORP/VONA math, unless you want it factored in — flag if so.

### 5.5 Live draft state — manual entry
As discussed: no reliable FantasyPros/ESPN live-sync API to hook into safely. The script tracks drafted players via a fast, typo-tolerant command while you watch the Draft Wizard/ESPN room as your source of truth:

```
> draft Ja'Marr Chase
✓ Marked drafted. Recomputing VORP/VONA...

> best
Rk  Player              Pos  VORP   VONA   Pts    Tier  ADP   OffRk
 1  Bijan Robinson      RB   68.2   41.0   301.4   1     4.1   7
 2  CeeDee Lamb         WR   61.4   12.8   288.0   1     6.8   3
 ...
```
- Fuzzy name matching with a confirmation prompt if ambiguous.
- `undo` command for mis-entered picks.
- State autosaves to disk after every command — a crash never loses draft progress.

---

## 6. Calculation methodology

### 6.1 VORP (primary ranking, stable)

1. **Effective starters per position, with OP spillover:** start with dedicated-slot counts (QB×14, RB×28, WR×28, TE×14). Rank all QB/RB/WR/TE by projected points, take the top (14 × 1 OP slot) of the *remaining* pool after dedicated slots are notionally filled, and add those to whichever position they land in. This gives an effective "how many will actually be startable" count per position.
2. **Replacement level per position** = the projected points of the player ranked just below that effective cutoff (e.g., if 30 RBs are effectively startable, the replacement RB is RB31).
3. **VORP = player's projected points − their position's replacement level.**
4. Recomputed after every `draft` command — remaining depth shifts where OP spillover lands, which shifts replacement level.

*(This is computed "VOLS-style" — replacement level = last effective starter, not deeper waiver-wire value. That's the right choice for pick-to-pick decisions; a true waiver-level baseline would compress differences between good starters and undervalue positional runs.)*

### 6.2 VONA (secondary column, live/volatile)

For each position, using ADP + your current draft slot:
1. Estimate how many total picks will happen before your next turn (shrinks/grows as the draft progresses through the snake order).
2. Using currently-available players sorted by ADP, identify the best player likely still available at that position when your next turn comes.
3. **VONA = current best available player's points at that position − that projected next-available player's points.**
4. `TIERS` from the rank file is used as a cross-check/simplification: a big VONA number should usually correspond to a tier break happening between now and your next pick — useful as a sanity check on the ADP-driven estimate, and a fallback if ADP data for a given player is missing.

Recomputed on every `draft` and every `status`/`best` call, since "picks until your next turn" changes constantly.

### 6.3 K/DST handling
Included in the main VORP/VONA table per your instruction, computed the same way as other positions (dedicated slot count = 14 each, no OP spillover eligibility for K/DST). Expect these to show low/flat VORP most of the draft, which is accurate — reflects genuine replaceability — and they'll naturally sort to the bottom until late.

---

## 7. Output columns

| Column | Source |
|---|---|
| Player | Projections/rank file |
| Position | Rank file (`POS`, split) |
| NFL team | Either file |
| **VORP** | Calculated (6.1) |
| **VONA** | Calculated (6.2) |
| Projected points (0.5 PPR) | Projections file (`FANTASYPTS`) |
| Tier | Rank file (`TIERS`) |
| ADP | ADP file (pending) |
| Overall rank | Rank file (`RK`) |
| Bye week | Rank file |
| Off Grade / Off Rank | Unit grades file, joined on team |

Sortable by any column; default sort VORP descending. Filterable by position (`best RB`, `best OP`, etc.).

---

## 8. CLI commands

| Command | Behavior |
|---|---|
| `draft <name>` | Mark a player drafted (yours or an opponent's), recompute VORP/VONA |
| `undo` | Revert the last drafted pick |
| `best [position]` | Show ranked available players, optionally filtered |
| `player <name>` | Full row for one player |
| `myteam` | Players you've drafted + remaining roster needs |
| `status` | Pick count, round, picks until your next turn |

---

## 9. Non-functional requirements

- No network calls during the draft; all data loaded from local files at startup.
- No dependency on ESPN/FantasyPros APIs or scraping for drafted-player tracking — manual entry only.
- Near-instant (<1 sec) recompute given the small player pool (~950 rows).
- Draft state persists to disk after every action.
- Single Python script + local CSVs; nothing requiring internet access on draft day.

---

## 10. Out of scope
- Auction draft support (confirmed snake)
- Trade evaluation, in-season tools, keeper logic
- Automated ESPN/FantasyPros syncing
- Multi-user or league-wide views

---

## 11. Status: built

All data received and mapped. `draft_engine.py` (data loading + VORP/VONA calculation) and `draft_assistant.py` (interactive CLI) are complete, tested against your real files, and delivered alongside this spec. See the accompanying `README.md` for setup and draft-day instructions.

**Two data quirks discovered during build, both handled in code (see inline comments in `draft_engine.py`):**
- K/DST `AVG` ADP column is a rank *among kickers/DSTs only* (1, 2, 3…), not a real overall pick number — the loader uses `Overall` instead, confirmed against Brandon Aubrey (AVG 1.0, but real ADP `Overall` 129).
- K/DST `FANTASYPTS` in the points files are per-game averages, unlike the season-total figures in the skill-player file — converted ×17 games for an apples-to-apples comparison.

**Recommended before draft day:** run a mock draft against the script per the README's dry-run instructions to confirm the numbers look right to you.
