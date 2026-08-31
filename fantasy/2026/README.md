# Draft Assistant — Quick Start

## Before draft day
1. Make sure Python 3 is installed (`python3 --version`).
2. Keep this whole folder together — `draft_assistant.py`, `draft_engine.py`, and the `data/` folder all need to stay in the same relative locations.
3. **Do a dry run.** Open a terminal, `cd` into this folder, run `python3 draft_assistant.py`, and try a few `pick` / `best` / `undo` commands against a mock draft. Confirm the top of the board looks right to you before trusting it live.
4. If your rankings/projections change before draft day, replace the CSVs in `data/` with updated exports of the same type (filenames can shift slightly year to year — the loader matches files by keyword, e.g. anything with "Superflex_ADP" in the name).

## On draft day
```
cd draft_assistant
python3 draft_assistant.py
```
First run asks for your number of teams and draft slot — answer once, it's saved. Every later run picks up right where you left off (state lives in `draft_state.json` in this folder).

## Commands
| Command | What it does |
|---|---|
| `pick <name>` | Mark a player drafted by someone else |
| `pick <name> mine` | Mark a player YOU drafted |
| `bulk` | Paste multiple picks at once (ESPN app copy/paste format, see below) |
| `teamname <name>` | Set/update your exact fantasy team name (used to detect your picks in `bulk`) |
| `undo` | Revert the last pick (works whether it was yours or an opponent's) |
| `best` | Top 20 available players by VORP, all positions |
| `best RB` | Top 20 available RBs |
| `best RB 5` | Top 5 available RBs || `best OP` | Top 20 available players eligible for your superflex/OP slot (QB/RB/WR/TE) |
| `board` | Top 6 available players for EACH position (QB/RB/WR/OP/TE/K/DST) in one view |
| `board 3` | Same, but top 3 per position instead of 6 |

| `player <name>` | Full detail on one player, drafted or not |
| `myteam` | Your roster so far + remaining dedicated-slot needs |
| `status` | Current pick number, round, and picks until your next turn |
| `help` | Show the command list |
| `quit` | Save and exit |

You don't need to type full names — partial names work (`pick cha` will offer Ja'Marr Chase, Chase Brown, etc. if there's more than one match) and small typos are tolerated. Defenses also work by nickname alone, or in ESPN's `Team D/ST` format — `pick Buccaneers D/ST`, `pick Bills`, and `pick Tampa Bay Buccaneers` all resolve to the same player, matching against the mascot name regardless of how it's phrased.

### Bulk paste
If you're copying multiple picks straight from the ESPN app (e.g. "everything since last round"), use `bulk` instead of typing `pick` one at a time. Type `bulk`, paste, then type `END` on its own line. Two paste formats are auto-detected -- paste whichever ESPN gives you and it'll figure out which one it's looking at:

**Format A** (team-roster view):
```
Josh Allen / BUF QB
R1, P1 - Luka and Loaded
```

**Format B** (draft-results table view):
```
1

Lamar Jackson
BAL
QB
Quinshon Rutabaga
214.9
322.9
3
```
Format B is tolerant of the mess ESPN's table view tends to produce when copied: table headers that repeat at the top of every new round ("Round 2 / Pick / Player / Team / ...") and stray "Q" lines (a queued-player badge that copies as its own line) can show up anywhere and get filtered out automatically.

The first time you use `bulk`, it'll ask for your exact ESPN team name (so it can tell which picks are yours) -- you can also set/update this anytime with `teamname <your team name>`. Matching is done on the fantasy team name specifically, so it's safe even if a fantasy team happens to be named after a real player (e.g. a team called "Jonathan Taylor" won't get confused with the player Jonathan Taylor).

Re-pasting picks you've already entered is safe -- duplicates are silently skipped. Any player it can't match, or any record it can't parse at all, gets reported individually so you can add it manually with `pick`, without holding up the rest of the batch.

## If something goes wrong mid-draft
- A bad command or typo never crashes the session — you'll see an error message and can just try again.
- If you need to close the terminal or the app crashes outright, your draft state is saved after every single pick. Just re-run `python3 draft_assistant.py` and you're back exactly where you were — re-enter picks you may have missed with `pick`, or `undo` anything entered by mistake.
- To wipe everything and start over (e.g. after a mock draft test), delete `draft_state.json` and re-run.

## Reading the columns
- **VORP** — season-long value over the last effective starter at that position (accounts for OP/superflex spillover). Your primary sort — tells you which position matters.
- **VONA** — value over the best player likely still there at your *next* pick, based on real superflex ADP. Tells you whether you can wait a round. Recomputed live as the draft moves.
- **Tier** — expert-consensus tier grouping; a tier change between now and your next pick is a good sanity check on a big VONA number.
- **OffRk** — that player's NFL team's projected offensive rank (1 = best), from the unit grades file. Always shown alongside the rest.
- **`*` on the board** — `board` marks exactly one player league-wide: the top option at whichever position has the single steepest drop from its best available player to its next-best (biggest top-to-second VONA gap). That's the position where punting this pick hurts the most.
- **TA Tier / FP Tier** — two separate expert-tier columns: The Athletic's tiering (QB/RB/WR/TE only) and FantasyPros'. Color coding (green = elite, yellow/orange = mid, red = deep) is on **TA Tier**; FP Tier displays plain. K/DST show `-` for TA Tier since that source doesn't cover them.
- **ADP delta** — the ADP column now shows `(+N)`/`(-N)` next to the value: how many picks past due (`+`) or still away (`-`) that player's ADP is relative to the current pick. A big positive number on someone still on the board is a real signal — other drafters are passing on the consensus for them.
- Kickers and DST projections were converted from FantasyPros' per-game figures to season totals (×17) to be comparable with the season-total skill-player projections.