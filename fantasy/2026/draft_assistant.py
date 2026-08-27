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
        picks_left = de.picks_until_next_turn(len(self.picks), self.my_slot, self.num_teams)
        de.compute_vona(self.pool, picks_left)

    # -- player lookup ---------------------------------------------------

    def find_player(self, query, include_drafted=False):
        """Match a name against players: exact, then substring (handles
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

    def parse_bulk_text(self, text):
        """Parse ESPN's copy/paste draft-log format into a list of dicts:
        {round, pick_in_round, overall_pick, player_name, fantasy_team}.
        Malformed blocks are skipped and reported, not fatal."""
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
                entries.append({
                    "round": rnd, "pick_in_round": pick_in_rnd, "overall_pick": overall,
                    "player_name": name, "fantasy_team": team_name,
                })
                i += 2
            else:
                problems.append(player_line)
                i += 1
        return entries, problems

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

        header = f'{"Player":22} {"Pos":4} {"Team":4} {"VORP":>7} {"VONA":>7} {"Pts":>7} {"Tier":>4} {"ADP":>6} {"Rk":>5} {"Bye":>4}'
        print(header)
        print("-" * len(header))
        for p in available[:n]:
            adp_str = f"{p.adp:.1f}" if p.adp is not None else "-"
            rk_str = str(p.rk) if p.rk is not None else "-"
            bye_str = p.bye or "-"
            vona_str = f"{p.vona:.1f}" if p.vona is not None else "-"
            print(f"{p.name:22} {p.position:4} {p.team:4} {p.vorp:7.1f} {vona_str:>7} "
                  f"{p.points:7.1f} {(p.tier or -1):4} {adp_str:>6} {rk_str:>5} {bye_str:>4}")

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
            print(f"  Tier: {p.tier}   Rank: {p.rk}   ADP: {p.adp}   Bye: {p.bye}")
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

    def cmd_status(self, args):
        total = len(self.picks)
        rnd = total // self.num_teams + 1
        pick_in_round = total % self.num_teams + 1
        picks_left = de.picks_until_next_turn(total, self.my_slot, self.num_teams)
        print(f"Pick {total + 1} overall (Round {rnd}, slot {pick_in_round} of {self.num_teams} on the clock).")
        print(f"Your draft slot: {self.my_slot}. Picks until your next turn: {picks_left}.")

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