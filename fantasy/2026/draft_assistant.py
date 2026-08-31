#!/usr/bin/env python3
"""
Live fantasy draft assistant: VORP + VONA, recomputed after every pick.

Run it, answer the one-time setup prompt (your draft slot), then use it
during your draft:

    pick <name>          mark a player drafted by someone else
    pick <name> mine      mark a player YOU drafted
    bulk                  paste multiple picks at once (ESPN copy/paste format)
    teamname <name>        set/update your fantasy team name (used by 'bulk'
                           to detect which picks are yours)
    undo                  revert the last pick
    best [pos] [n]        show top available players (default: all positions, 20)
    board [n]              top n (default 6) available players for EACH position:
                           QB, RB, WR, OP, TE, K, DST -- one draft-board view
    player <name>         full detail on one player
    myteam                your roster + remaining needs
    status                pick count / round / picks until your next turn
    help                  show this list
    quit                  save and exit

State autosaves to draft_state.json after every action, so closing the
terminal or a crash never loses your draft progress -- just restart the
script and everything picks up where you left off.
"""

import difflib
import json
import os
import re
import sys

import draft_engine as de

# ANSI colors for the Tier column. Auto-disabled when stdout isn't an
# interactive terminal (e.g. output redirected to a file) so nothing weird
# ever gets written to a log.
USE_COLOR = sys.stdout.isatty()
_RESET = "\033[0m"
_TIER_COLORS = {
    1: "\033[92m",  # bright green -- elite tier
    2: "\033[32m",  # green
    3: "\033[33m",  # yellow
    4: "\033[33m",  # yellow
    5: "\033[38;5;208m",  # orange
    6: "\033[38;5;208m",  # orange
}
_TIER_DEFAULT_COLOR = "\033[31m"  # red -- tier 7+ (deep/replaceable)
_TIER_NONE_COLOR = "\033[90m"     # gray -- no tier data
_TIER_COL_WIDTH = 7


def _colorize_tier(tier, plain=False):
    """Return a width-{_TIER_COL_WIDTH} tier string. Colored (ANSI, if
    enabled) unless plain=True. '-' for missing data, consistent with how
    other optional columns (ADP, Rk, Bye) are displayed. Padding is applied
    to the plain text BEFORE wrapping in color codes so terminal column
    alignment isn't thrown off by the invisible escape characters."""
    text = f"{tier:>{_TIER_COL_WIDTH}}" if tier is not None else f'{"-":>{_TIER_COL_WIDTH}}'
    if plain or not USE_COLOR:
        return text
    code = _TIER_NONE_COLOR if tier is None else _TIER_COLORS.get(tier, _TIER_DEFAULT_COLOR)
    return f"{code}{text}{_RESET}"

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
STATE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "draft_state.json")


class DraftSession:
    def __init__(self):
        self.pool = {}
        self.my_slot = None
        self.num_teams = de.NUM_TEAMS
        self.my_team_name = None
        self.picks = []  # list of dicts: {key: [name,team], mine: bool}

    # -- setup / persistence -------------------------------------------------

    def load_data(self):
        self.pool = de.build_player_pool(DATA_DIR)

    def load_state(self):
        if not os.path.exists(STATE_PATH):
            return False
        try:
            with open(STATE_PATH) as f:
                state = json.load(f)
        except (json.JSONDecodeError, OSError) as e:
            print(f"[warning] Could not read saved state ({e}). Starting fresh.")
            return False
        self.my_slot = state.get("my_slot")
        self.num_teams = state.get("num_teams", de.NUM_TEAMS)
        self.my_team_name = state.get("my_team_name")
        self.picks = state.get("picks", [])
        self._replay_picks()
        return True

    def save_state(self):
        state = {
            "my_slot": self.my_slot,
            "num_teams": self.num_teams,
            "my_team_name": self.my_team_name,
            "picks": self.picks,
        }
        tmp_path = STATE_PATH + ".tmp"
        try:
            with open(tmp_path, "w") as f:
                json.dump(state, f, indent=2)
            os.replace(tmp_path, STATE_PATH)  # atomic, avoids corrupt half-writes
        except OSError as e:
            print(f"[warning] Could not save state: {e}")

    def _replay_picks(self):
        for p in self.pool.values():
            p.drafted, p.drafted_by_me, p.pick_number = False, False, None
        for i, pk in enumerate(self.picks, start=1):
            key = tuple(pk["key"])
            player = self.pool.get(key)
            if player is None:
                continue
            player.drafted = True
            player.drafted_by_me = pk.get("mine", False)
            player.pick_number = i
        self._recompute()

    def _recompute(self):
        de.compute_vorp(self.pool)
        # VONA needs "how many picks happen AFTER the one I'm about to make,
        # before I'm on the clock again" -- not "picks until now," which is
        # always 0 exactly when it's your turn (the case that matters most).
        # Shifting the base by 1 simulates this pick resolving first, then
        # counts forward to the next occurrence of my_slot.
        picks_left = de.picks_until_next_turn(len(self.picks) + 1, self.my_slot, self.num_teams)
        de.compute_vona(self.pool, picks_left)

    def cmd_status(self, args):
        total = len(self.picks)
        rnd = total // self.num_teams + 1
        pick_in_round = total % self.num_teams + 1
        picks_left = de.picks_until_next_turn(total + 1, self.my_slot, self.num_teams)
        print(f"Pick {total + 1} overall (Round {rnd}, slot {pick_in_round} of {self.num_teams} on the clock).")
        print(f"Your draft slot: {self.my_slot}. Picks until your next turn (after this one): {picks_left}.")

    # -- player lookup ---------------------------------------------------

    _DST_SUFFIX_RE = re.compile(r"\s*(D\s*/\s*S\s*T|DST|DEF)\.?\s*$", re.IGNORECASE)

    def _dst_nickname_match(self, query, pool):
        """ESPN gives DSTs as 'Buccaneers D/ST'; our data has full team names
        like 'Tampa Bay Buccaneers'. Strip the D/ST-style suffix and match
        the remaining nickname against the last word of each DST's name.
        Returns (player, True) / (None, candidates) / (None, None) if this
        path found nothing (caller should fall through to normal matching)."""
        stripped = self._DST_SUFFIX_RE.sub("", query).strip()
        norm_stripped = de.normalize_name(stripped)
        if not norm_stripped:
            return None, None

        dst_pool = [p for p in pool if p.position == "DST"]
        nickname_matches = [p for p in dst_pool
                             if de.normalize_name(p.name).split()[-1] == norm_stripped]
        if not nickname_matches:
            nickname_matches = [p for p in dst_pool if norm_stripped in de.normalize_name(p.name)]

        if len(nickname_matches) == 1:
            return nickname_matches[0], True
        if len(nickname_matches) > 1:
            return None, nickname_matches[:5]
        return None, None

    def find_player(self, query, include_drafted=False):
        """Match a name against players: exact, then DST nickname (handles
        'Buccaneers D/ST' -> 'Tampa Bay Buccaneers'), then substring (handles
        partial typing like 'cha' -> 'Ja'Marr Chase'), then fuzzy.
        By default only searches available (undrafted) players; pass
        include_drafted=True to also match already-drafted players (used by
        bulk import to detect duplicates). Returns (player, True) or
        (None, candidates_list)."""
        norm_query = de.normalize_name(query)
        pool = list(self.pool.values()) if include_drafted else \
            [p for p in self.pool.values() if not p.drafted]

        exact = [p for p in pool if de.normalize_name(p.name) == norm_query]
        if len(exact) == 1:
            return exact[0], True

        dst_player, dst_result = self._dst_nickname_match(query, pool)
        if dst_player is not None:
            return dst_player, True
        if dst_result:  # ambiguous nickname match (multiple candidates)
            return None, dst_result

        if len(norm_query) >= 3:
            substr = [p for p in pool if norm_query in de.normalize_name(p.name)]
            if len(substr) == 1:
                return substr[0], True
            if len(substr) > 1:
                substr.sort(key=lambda p: p.vorp if p.vorp is not None else -9999, reverse=True)
                return None, substr[:5]

        name_map = {p.name: p for p in pool}
        close = difflib.get_close_matches(query, name_map.keys(), n=5, cutoff=0.6)
        if len(close) == 1:
            return name_map[close[0]], True
        if len(close) > 1:
            return None, [name_map[c] for c in close]
        return None, []

    def cmd_teamname(self, args):
        if not args:
            current = self.my_team_name or "(not set)"
            print(f"Current team name: {current}")
            print("Usage: teamname <your exact ESPN fantasy team name>")
            return
        self.my_team_name = " ".join(args)
        print(f"Team name set to: {self.my_team_name}")
        self.save_state()

    # -- bulk paste ----------------------------------------------------------

    _BULK_PLAYER_RE = re.compile(r"^(.+?)\s*/\s*(\S+)\s+(\S+)$")
    _BULK_PICK_RE = re.compile(r"^R(\d+)\s*,\s*P(\d+)\s*-\s*(.+)$")

    def parse_bulk_format_a(self, text):
        """Format A: ESPN team-roster-style paste --
            Josh Allen / BUF QB
            R1, P1 - Luka and Loaded
        Two lines per pick. Malformed blocks are skipped and reported, not fatal."""
        lines = [ln.strip() for ln in text.splitlines()]
        lines = [ln for ln in lines if ln]  # drop blank lines
        entries = []
        problems = []
        i = 0
        while i < len(lines):
            player_line = lines[i]
            pick_line = lines[i + 1] if i + 1 < len(lines) else None
            pm = self._BULK_PLAYER_RE.match(player_line)
            pkm = self._BULK_PICK_RE.match(pick_line) if pick_line else None
            if pm and pkm:
                name = pm.group(1).strip()
                rnd, pick_in_rnd, team_name = int(pkm.group(1)), int(pkm.group(2)), pkm.group(3).strip()
                overall = (rnd - 1) * self.num_teams + pick_in_rnd
                entries.append({"overall_pick": overall, "player_name": name, "fantasy_team": team_name})
                i += 2
            else:
                problems.append(player_line)
                i += 1
        return entries, problems

    # Lines that are pure formatting noise in format B: table headers that
    # ESPN repeats at the top of every new round, and a stray "Q" line for
    # players who were on someone's draft queue (a badge that copies as its
    # own line). None of these are ever real field data.
    _BULK_B_NOISE_WORDS = {"pick", "player", "team", "rk", "q"}
    _BULK_B_ROUND_RE = re.compile(r"^round\b", re.IGNORECASE)
    _BULK_B_YEAR_PTS_RE = re.compile(r"^\d{4}\s*pts\.?$", re.IGNORECASE)
    _BULK_B_PROJ_PTS_RE = re.compile(r"^proj\.?\s*pts\.?$", re.IGNORECASE)

    @classmethod
    def _is_bulk_b_noise(cls, line):
        s = line.strip()
        if s == "":
            return True
        sl = s.lower()
        return (sl in cls._BULK_B_NOISE_WORDS or cls._BULK_B_ROUND_RE.match(sl) or
                cls._BULK_B_YEAR_PTS_RE.match(sl) or cls._BULK_B_PROJ_PTS_RE.match(sl))

    def parse_bulk_format_b(self, text):
        """Format B: ESPN draft-results-table-style paste -- one field per
        line, pick number on its own line followed by a blank line:
            1
            <blank>
            Lamar Jackson
            BAL
            QB
            Quinshon Rutabaga
            214.9
            322.9
            3
        Field order per record is always: player name, team, position,
        fantasy team name, then trailing stats we don't need. Header rows
        ("Round 2 / Pick / Player / Team / ...") and stray "Q" queue markers
        can appear anywhere and are filtered out before reading fields, so
        only their fixed relative ORDER (not their line position) matters.
        The pick-number-then-blank-line pattern is what's used to find where
        each record starts, since it's the one thing that never appears
        elsewhere in this format.
        """
        raw_lines = text.splitlines()
        starts = []
        for i, line in enumerate(raw_lines):
            s = line.strip()
            if s.isdigit() and i + 1 < len(raw_lines) and raw_lines[i + 1].strip() == "":
                starts.append((i, int(s)))

        entries = []
        problems = []
        for idx, (start_i, pick_num) in enumerate(starts):
            end_i = starts[idx + 1][0] if idx + 1 < len(starts) else len(raw_lines)
            field_lines = raw_lines[start_i + 2:end_i]  # skip pick-number line + its blank
            clean = [ln.strip() for ln in field_lines if not self._is_bulk_b_noise(ln)]
            if len(clean) < 4:
                problems.append(f"Pick {pick_num}: only found {len(clean)} field(s), skipped")
                continue
            player_name, fantasy_team = clean[0], clean[3]
            entries.append({"overall_pick": pick_num, "player_name": player_name,
                             "fantasy_team": fantasy_team})
        return entries, problems

    def parse_bulk_text(self, text):
        """Auto-detect and parse either supported paste format."""
        entries, problems = self.parse_bulk_format_a(text)
        if entries:
            return entries, problems
        return self.parse_bulk_format_b(text)

    def cmd_bulk(self, args):
        if self.my_team_name is None:
            print("First, what's your exact fantasy team name as shown in ESPN?")
            print("(This is how bulk-imported picks get tagged as yours.)")
            name = input("Team name: ").strip()
            if name:
                self.my_team_name = name
                self.save_state()

        print("Paste your picks below. When you're done, type END on its own line and press Enter.")
        pasted_lines = []
        while True:
            try:
                line = input()
            except (EOFError, KeyboardInterrupt):
                break
            if line.strip().upper() == "END":
                break
            pasted_lines.append(line)
        text = "\n".join(pasted_lines)

        entries, problems = self.parse_bulk_text(text)
        if not entries:
            print("Didn't find any recognizable picks in that paste. No changes made.")
            return

        entries.sort(key=lambda e: e["overall_pick"])

        added, skipped_dup, unmatched, ambiguous = 0, 0, [], []
        for e in entries:
            player, result = self.find_player(e["player_name"], include_drafted=True)
            if player is None:
                if result:
                    ambiguous.append((e["player_name"], result))
                else:
                    unmatched.append(e["player_name"])
                continue
            if player.drafted:
                skipped_dup += 1
                continue
            mine = (self.my_team_name is not None and
                    de.normalize_name(self.my_team_name) == de.normalize_name(e["fantasy_team"]))
            self.picks.append({"key": list(player.key), "mine": mine})
            added += 1

        self._replay_picks()
        self.save_state()

        print(f"\nAdded {added} pick(s).", end="")
        if skipped_dup:
            print(f" Skipped {skipped_dup} already-recorded duplicate(s).")
        else:
            print()
        if unmatched:
            print(f"Could not find {len(unmatched)} player(s) (already off the board, or name "
                  f"didn't match) -- enter these manually with 'pick <name>' if needed:")
            for n in unmatched:
                print(f"  - {n}")
        if ambiguous:
            print(f"{len(ambiguous)} name(s) matched multiple available players -- enter manually:")
            for n, candidates in ambiguous:
                names = ", ".join(c.name for c in candidates)
                print(f"  - '{n}' matched: {names}")
        if self.my_team_name:
            matched_mine = sum(1 for e in entries if de.normalize_name(e["fantasy_team"]) ==
                                de.normalize_name(self.my_team_name))
            if matched_mine == 0:
                print(f"\nNote: none of the pasted picks matched your team name "
                      f"('{self.my_team_name}') -- double check it with 'teamname' if that's "
                      f"unexpected.")
        if problems:
            print(f"\n{len(problems)} line(s)/record(s) in the paste couldn't be parsed at all "
                  f"(different format, or genuinely malformed) -- these were skipped entirely:")
            for p in problems:
                print(f"  - {p}")

    # -- other commands -------------------------------------------------

    def cmd_pick(self, args):
        if not args:
            print("Usage: pick <player name> [mine]")
            return
        mine = False
        if args[-1].lower() == "mine":
            mine = True
            args = args[:-1]
        query = " ".join(args)
        player, result = self.find_player(query)
        if player is None:
            if result:
                print("Multiple matches, be more specific:")
                for c in result:
                    print(f"  {c.name} ({c.position}, {c.team})")
            else:
                print(f"No available player matching '{query}'.")
            return

        self.picks.append({
            "key": list(player.key), "mine": mine,
        })
        self._replay_picks()
        tag = " (yours)" if mine else ""
        print(f"Marked drafted: {player.name} ({player.position}, {player.team}){tag}")
        self.save_state()

    def cmd_undo(self, args):
        if not self.picks:
            print("No picks to undo.")
            return
        last = self.picks.pop()
        self._replay_picks()
        key = tuple(last["key"])
        name = self.pool[key].name if key in self.pool else last["key"][0]
        print(f"Undid pick: {name}")
        self.save_state()

    def _print_player_table(self, players, highlight_key=None):
        if not players:
            print("  (none available)")
            return
        current_pick = len(self.picks) + 1
        show_marker = highlight_key is not None
        prefix = "  " if show_marker else ""
        adp_width = 13
        header = (f'{prefix}{"Player":22} {"Pos":4} {"Team":4} {"VORP":>7} {"VONA":>7} {"Pts":>7} '
                  f'{"TA Tier":>{_TIER_COL_WIDTH}} {"FP Tier":>{_TIER_COL_WIDTH}} '
                  f'{"ADP":>{adp_width}} {"Rk":>5} {"Bye":>4} {"OffRk":>5}')
        print(header)
        print("-" * len(header))
        for p in players:
            if p.adp is not None:
                delta = round(current_pick - p.adp)
                adp_str = f"{p.adp:.1f} ({delta:+d})"
            else:
                adp_str = "-"
            rk_str = str(p.rk) if p.rk is not None else "-"
            bye_str = p.bye or "-"
            vona_str = f"{p.vona:.1f}" if p.vona is not None else "-"
            off_rk_str = str(p.off_rank) if p.off_rank is not None else "-"
            row_prefix = ("* " if (show_marker and p.key == highlight_key) else "  ") if show_marker else ""
            ta_tier_colored = _colorize_tier(p.ta_tier)
            fp_tier_plain = _colorize_tier(p.fp_tier, plain=True)
            print(f"{row_prefix}{p.name:22} {p.position:4} {p.team:4} {p.vorp:7.1f} {vona_str:>7} "
                  f"{p.points:7.1f} {ta_tier_colored} {fp_tier_plain} "
                  f"{adp_str:>{adp_width}} {rk_str:>5} {bye_str:>4} {off_rk_str:>5}")

    def cmd_best(self, args):
        pos_filter = None
        n = 20
        for a in args:
            if a.isdigit():
                n = int(a)
            else:
                pos_filter = a.upper()

        available = [p for p in self.pool.values() if not p.drafted]
        if pos_filter:
            if pos_filter == "FLEX" or pos_filter == "OP":
                available = [p for p in available if p.position in de.OP_ELIGIBLE_POSITIONS]
            else:
                available = [p for p in available if p.position == pos_filter]
        available.sort(key=lambda p: (p.vorp if p.vorp is not None else -9999), reverse=True)

        if not available:
            print("No matching players available.")
            return
        self._print_player_table(available[:n])

    def cmd_board(self, args):
        n = 6
        if args and args[0].isdigit():
            n = int(args[0])
        groups = ["QB", "RB", "WR", "OP", "TE", "K", "DST"]
        available = [p for p in self.pool.values() if not p.drafted]

        group_players = {}
        for pos in groups:
            if pos == "OP":
                players = [p for p in available if p.position in de.OP_ELIGIBLE_POSITIONS]
            else:
                players = [p for p in available if p.position == pos]
            players.sort(key=lambda p: p.vorp if p.vorp is not None else -9999, reverse=True)
            group_players[pos] = players[:n]

        # Find the position with the steepest cliff: the biggest gap between
        # its best available player's VONA and the next-best player's VONA
        # (the #1 player's VONA is 0 by construction, so this is effectively
        # "how much value do I lose here if I don't take the top guy now").
        highlight_player = None
        best_delta = -1
        for pos, players in group_players.items():
            if len(players) < 2:
                continue
            top, second = players[0], players[1]
            if top.vona is None or second.vona is None:
                continue
            delta = top.vona - second.vona
            if delta > best_delta:
                best_delta = delta
                highlight_player = top

        highlight_key = highlight_player.key if highlight_player else None
        for pos in groups:
            label = "OP (superflex-eligible)" if pos == "OP" else pos
            print(f"\n=== Top {n} {label} ===")
            self._print_player_table(group_players[pos], highlight_key=highlight_key)

        if highlight_player:
            print(f"\n* {highlight_player.name} ({highlight_player.position}) -- biggest cross-position "
                  f"cliff: {best_delta:.1f} VONA gap to the next-best option at that position.")

    def cmd_player(self, args):
        if not args:
            print("Usage: player <name>")
            return
        query = " ".join(args)
        norm_query = de.normalize_name(query)
        matches = [p for p in self.pool.values() if de.normalize_name(p.name) == norm_query]
        if not matches:
            name_map = {p.name: p for p in self.pool.values()}
            close = difflib.get_close_matches(query, name_map.keys(), n=5, cutoff=0.6)
            matches = [name_map[c] for c in close]
        if not matches:
            print(f"No player found matching '{query}'.")
            return
        for p in matches:
            status = "DRAFTED" + (" (yours)" if p.drafted_by_me else "") if p.drafted else "available"
            print(f"\n{p.name} -- {p.position}, {p.team} [{status}]")
            print(f"  Points: {p.points:.1f}   VORP: {p.vorp}   VONA: {p.vona}")
            ta_tier_display = _colorize_tier(p.ta_tier).strip()
            fp_tier_display = p.fp_tier if p.fp_tier is not None else "-"
            print(f"  TA Tier: {ta_tier_display}   FP Tier: {fp_tier_display}   "
                  f"Rank: {p.rk}   ADP: {p.adp}   Bye: {p.bye}")
            if p.off_rank is not None:
                print(f"  Team Off Rank: {p.off_rank} (grade {p.off_grade})   "
                      f"Def Rank: {p.def_rank} (grade {p.def_grade})")

    def cmd_myteam(self, args):
        mine = [p for p in self.pool.values() if p.drafted_by_me]
        mine.sort(key=lambda p: p.pick_number or 0)
        if not mine:
            print("No players drafted yet.")
        else:
            print("Your roster:")
            for p in mine:
                print(f"  Pick {p.pick_number:>3}: {p.name} ({p.position}, {p.team})")

        counts = {}
        for p in mine:
            counts[p.position] = counts.get(p.position, 0) + 1
        needs = dict(de.DEDICATED_SLOTS_PER_TEAM)
        print("\nDedicated starting-slot needs remaining (rough guide; OP/superflex not included):")
        for pos, need in needs.items():
            have = counts.get(pos, 0)
            print(f"  {pos}: {max(0, need - have)} more needed (have {have}, need {need})")

    def cmd_help(self, args):
        print(__doc__)


def run_setup(session):
    print("=== First-time setup ===")
    while True:
        raw = input(f"Number of teams in your league [{de.NUM_TEAMS}]: ").strip()
        if not raw:
            session.num_teams = de.NUM_TEAMS
            break
        if raw.isdigit() and int(raw) > 0:
            session.num_teams = int(raw)
            break
        print("Please enter a positive number.")
    while True:
        raw = input("Your draft slot (1-based, e.g. 5 for the 5th pick): ").strip()
        if raw.isdigit() and 1 <= int(raw) <= session.num_teams:
            session.my_slot = int(raw)
            break
        print(f"Please enter a number from 1 to {session.num_teams}.")
    session.save_state()
    print("Setup saved. You're ready to draft.\n")


def main():
    session = DraftSession()
    try:
        session.load_data()
    except FileNotFoundError as e:
        print(f"ERROR loading data: {e}")
        sys.exit(1)

    had_state = session.load_state()
    if not had_state or session.my_slot is None:
        run_setup(session)
        session._recompute()

    print("Draft assistant ready. Type 'help' for commands.\n")

    commands = {
        "pick": session.cmd_pick, "draft": session.cmd_pick,
        "bulk": session.cmd_bulk,
        "teamname": session.cmd_teamname,
        "undo": session.cmd_undo,
        "best": session.cmd_best,
        "board": session.cmd_board,
        "player": session.cmd_player,
        "myteam": session.cmd_myteam,
        "status": session.cmd_status,
        "help": session.cmd_help,
    }

    while True:
        try:
            line = input("> ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\nExiting. State is saved -- resume anytime by re-running this script.")
            break
        if not line:
            continue
        if line.lower() in ("quit", "exit", "q"):
            print("Exiting. State is saved -- resume anytime by re-running this script.")
            break
        parts = line.split()
        cmd, args = parts[0].lower(), parts[1:]
        fn = commands.get(cmd)
        if fn is None:
            print(f"Unknown command '{cmd}'. Type 'help' for the list of commands.")
            continue
        try:
            fn(args)
        except Exception as e:
            # Never let an unexpected error crash the session mid-draft.
            print(f"[error] Something went wrong with that command: {e}")
            print("Your draft state is unaffected -- try again or type 'help'.")


if __name__ == "__main__":
    main()