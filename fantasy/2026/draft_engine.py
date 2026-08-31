"""
Draft engine: loads FantasyPros CSV exports, builds a unified player pool,
and computes live VORP + VONA. Pure logic, no I/O prompts here, so it can
be tested independently of the CLI.
"""

import csv
import glob
import os
import re
from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# League / roster configuration
# ---------------------------------------------------------------------------

NUM_TEAMS = 14

# Dedicated (non-flex) starting slots per team, by position.
DEDICATED_SLOTS_PER_TEAM = {
    "QB": 1,
    "RB": 2,
    "WR": 2,
    "TE": 1,
    "DST": 1,
    "K": 1,
}

# The single flex-type slot: OP / Superflex, any offensive player (QB/RB/WR/TE).
OP_SLOTS_PER_TEAM = 1
OP_ELIGIBLE_POSITIONS = ("QB", "RB", "WR", "TE")

GAMES_PER_SEASON = 17  # used to convert K/DST per-game projections to season totals


def normalize_name(name: str) -> str:
    """Lowercase, strip punctuation/suffixes, collapse whitespace, for join keys."""
    if not name:
        return ""
    n = name.lower().strip()
    n = n.replace(".", "").replace("'", "").replace(",", "")
    n = re.sub(r"\b(jr|sr|ii|iii|iv|v)\b", "", n)
    n = re.sub(r"\s+", " ", n).strip()
    return n


# ---------------------------------------------------------------------------
# Player model
# ---------------------------------------------------------------------------

@dataclass
class Player:
    name: str
    team: str
    position: str          # QB / RB / WR / TE / K / DST
    points: float = 0.0    # projected season points, 0.5 PPR
    fp_tier: Optional[int] = None   # FantasyPros expert-consensus tier
    ta_tier: Optional[int] = None   # The Athletic tier (QB/RB/WR/TE only)
    rk: Optional[int] = None       # expert consensus rank within source file
    bye: Optional[str] = None
    adp: Optional[float] = None    # superflex-adjusted ADP where applicable
    off_grade: Optional[float] = None
    off_rank: Optional[int] = None
    def_grade: Optional[float] = None
    def_rank: Optional[int] = None

    # live-computed fields
    vorp: Optional[float] = None
    vona: Optional[float] = None

    drafted: bool = False
    drafted_by_me: bool = False
    pick_number: Optional[int] = None

    @property
    def key(self):
        return (normalize_name(self.name), self.team)

    @property
    def adp_sort_key(self):
        # Players with no known ADP are treated as "won't go soon" using rk as a
        # fallback so they still sort sensibly relative to each other.
        if self.adp is not None:
            return self.adp
        if self.rk is not None:
            return 1000 + self.rk
        return 9999


# ---------------------------------------------------------------------------
# File discovery (glob-based so next year's slightly-renamed exports still work)
# ---------------------------------------------------------------------------

def _find_one(data_dir: str, *must_contain: str) -> Optional[str]:
    candidates = glob.glob(os.path.join(data_dir, "*.csv"))
    for path in candidates:
        base = os.path.basename(path)
        if all(token.lower() in base.lower() for token in must_contain):
            return path
    return None


# ---------------------------------------------------------------------------
# Individual file loaders
# ---------------------------------------------------------------------------

def load_skill_projections(path):
    """HALF_PPR_PROJPOINTS_..._OP_Rankings.csv -> dict[(name,team)] = points.

    NOTE: this file has duplicate column headers (YDS/TDS repeated for
    pass/rec/rush), so it must be parsed by column index, not by header name.
    Columns: RK, TIERS, PLAYER NAME, TEAM, FANTASYPTS, ...
    """
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.reader(f)
        next(reader, None)  # header
        for row in reader:
            if len(row) < 5 or not row[2].strip():
                continue
            name, team = row[2].strip(), row[3].strip()
            try:
                points = float(row[4])
            except ValueError:
                points = 0.0
            out[(normalize_name(name), team)] = points
    return out


_POS_SPLIT_RE = re.compile(r"^([A-Za-z]+)(\d+)$")


def load_skill_ranks(path):
    """HALF_PPR_RANK_..._OP_Rankings.csv -> dict[(name,team)] = {name, pos, tier, rk, bye}."""
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = (row.get("PLAYER NAME") or "").strip()
            if not name:
                continue
            team = (row.get("TEAM") or "").strip()
            pos_field = (row.get("POS") or "").strip()
            m = _POS_SPLIT_RE.match(pos_field)
            pos = m.group(1) if m else pos_field
            try:
                tier = int(row.get("TIERS") or 0) or None
            except ValueError:
                tier = None
            try:
                rk = int(row.get("RK") or 0) or None
            except ValueError:
                rk = None
            bye = (row.get("BYE WEEK") or "").strip() or None
            out[(normalize_name(name), team)] = {
                "name": name, "position": pos, "tier": tier, "rk": rk, "bye": bye,
            }
    return out


def load_kdst_points_ranks(path, position):
    """FantasyPros_2026_Draft_{K,DST}_Rankings.csv -> list[Player] (no ADP yet).

    FANTASYPTS here is a PER-GAME average (unlike the skill-player file, which
    is a season total) -- confirmed by magnitude (~8-13 for top players).
    Converted to a season total via GAMES_PER_SEASON so it's comparable to
    the skill-player projections in the same VORP math.
    """
    players = []
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.reader(f)
        next(reader, None)
        for row in reader:
            if len(row) < 5 or not row[2].strip():
                continue
            name, team = row[2].strip(), row[3].strip()
            try:
                per_game = float(row[4])
            except ValueError:
                per_game = 0.0
            try:
                tier = int(row[1]) if row[1] else None
            except ValueError:
                tier = None
            try:
                rk = int(row[0]) if row[0] else None
            except ValueError:
                rk = None
            players.append(Player(
                name=name, team=team, position=position,
                points=per_game * GAMES_PER_SEASON,
                fp_tier=tier, rk=rk,
            ))
    return players


_NAME_TEAM_BYE_RE = re.compile(r"^(.*\S)\s{2,}([A-Z]{2,3})\s*\((\S+)\)\s*$")
_TEAM_BYE_RE = re.compile(r"^(.*\S)\s{2,}\((\S+)\)\s*$")


def load_superflex_adp(path):
    """FantasyPros_2026_Superflex_ADP_Rankings.csv -> dict[(name,team)] = adp.

    Uses the AVG column, NOT Overall -- Overall is standard 1-QB-league ADP
    included for reference (e.g. Josh Allen: OP rank 3 / AVG 2.5, but
    Overall 19). Using Overall here would badly understate QB scarcity.
    """
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            raw = (row.get("Player (Bye)") or "").strip()
            m = _NAME_TEAM_BYE_RE.match(raw)
            if not m:
                continue
            name, team, _bye = m.group(1), m.group(2), m.group(3)
            try:
                adp = float(row.get("AVG") or "")
            except ValueError:
                continue
            out[(normalize_name(name), team)] = adp
    return out


def load_k_adp(path):
    """FantasyPros_2026_K_ADP_Rankings.csv -> dict[(name,team)] = adp.

    Uses the 'Overall' column, NOT 'AVG'. Unlike the superflex file, here
    'AVG' is the average POSITIONAL rank among kickers only (e.g. 1.0, 3.0 --
    "best kicker, 3rd-best kicker"), not a real draft-pick number. 'Overall'
    is the actual overall ADP pick (e.g. 129) -- confirmed by Brandon Aubrey
    showing AVG 1.0 but Overall 129, which matches reality (kickers go late).
    Using AVG here would make kickers look like first-round picks.
    """
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            raw = (row.get("Player (Bye)") or "").strip()
            m = _NAME_TEAM_BYE_RE.match(raw)
            if not m:
                continue
            name, team, _bye = m.group(1), m.group(2), m.group(3)
            try:
                adp = float(row.get("Overall") or "")
            except ValueError:
                continue
            out[(normalize_name(name), team)] = adp
    return out


def load_dst_adp(path, fullname_to_abbrev):
    """FantasyPros_2026_DST_ADP_Rankings.csv -> dict[team_abbrev] = adp.

    Uses 'Overall' for the same reason as load_k_adp: 'AVG' here is an
    average positional rank among DSTs only, not a real overall pick number.
    """
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            raw = (row.get("Player (Bye)") or "").strip()
            m = _TEAM_BYE_RE.match(raw)
            if not m:
                continue
            full_name = m.group(1).strip()
            abbrev = fullname_to_abbrev.get(full_name.lower())
            if not abbrev:
                continue
            try:
                adp = float(row.get("Overall") or "")
            except ValueError:
                continue
            out[abbrev] = adp
    return out


def load_ta_tiers(path):
    """the_athletic_tiers.csv -> dict[normalize_name(name)] = ta_tier (int).

    QB/RB/WR/TE only -- no K/DST coverage, so those positions simply won't
    have a TA Tier value (handled as None/blank downstream, same as any
    other optional field). No team column in this file, so matching is by
    normalized player name alone.
    """
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = (row.get("Player") or "").strip()
            if not name:
                continue
            try:
                tier = int(row.get("TA Tier") or "")
            except ValueError:
                continue
            out[normalize_name(name)] = tier
    return out


def load_team_grades(path):
    """2026_NFL_Unit_Grades_1.csv -> dict[full_name.lower()] = grade info."""
    out = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            team = (row.get("Team") or "").strip()
            if not team:
                continue

            def _f(key):
                try:
                    return float(row.get(key))
                except (TypeError, ValueError):
                    return None

            def _i(key):
                try:
                    return int(row.get(key))
                except (TypeError, ValueError):
                    return None

            out[team.lower()] = {
                "off_grade": _f("Off Grade"), "off_rank": _i("Off Rank"),
                "def_grade": _f("Def Grade"), "def_rank": _i("Def Rank"),
            }
    return out


# ---------------------------------------------------------------------------
# Top-level pool builder
# ---------------------------------------------------------------------------

def build_player_pool(data_dir: str):
    """Locate and load all known input files, return a dict[key] = Player.

    Raises FileNotFoundError with a clear message if a required file is
    missing, rather than a cryptic traceback -- important given this needs
    to never surprise you mid-draft.
    """
    paths = {
        "proj": _find_one(data_dir, "PROJPOINTS", "OP_Rankings"),
        "rank": _find_one(data_dir, "_RANK_", "OP_Rankings"),
        "dst_pts": _find_one(data_dir, "Draft_DST_Rankings"),
        "k_pts": _find_one(data_dir, "Draft_K_Rankings"),
        "sf_adp": _find_one(data_dir, "Superflex_ADP"),
        "k_adp": _find_one(data_dir, "K_ADP"),
        "dst_adp": _find_one(data_dir, "DST_ADP"),
        "grades": _find_one(data_dir, "Unit_Grades"),
        "ta_tiers": _find_one(data_dir, "the_athletic_tiers"),
    }
    missing = [k for k, v in paths.items() if v is None]
    if missing:
        raise FileNotFoundError(
            f"Could not find data file(s) for: {', '.join(missing)}. "
            f"Check that all CSVs are present in {data_dir}."
        )

    proj = load_skill_projections(paths["proj"])
    ranks = load_skill_ranks(paths["rank"])
    sf_adp = load_superflex_adp(paths["sf_adp"])
    k_adp = load_k_adp(paths["k_adp"])
    grades = load_team_grades(paths["grades"])
    ta_tiers = load_ta_tiers(paths["ta_tiers"])

    # Build a team-name lookup from the DST points file (has both full name & abbrev).
    fullname_to_abbrev = {}
    dst_players_raw = load_kdst_points_ranks(paths["dst_pts"], "DST")
    for p in dst_players_raw:
        fullname_to_abbrev[p.name.lower()] = p.team

    dst_adp = load_dst_adp(paths["dst_adp"], fullname_to_abbrev)
    k_players_raw = load_kdst_points_ranks(paths["k_pts"], "K")

    # Precompute abbrev -> full name once, for O(1) grade lookups.
    abbrev_to_fullname_lower = {ab: full for full, ab in fullname_to_abbrev.items()}

    def grade_for_abbrev(abbrev):
        full_lower = abbrev_to_fullname_lower.get(abbrev)
        return grades.get(full_lower) if full_lower else None

    def apply_grade(p, grade):
        if grade:
            p.off_grade, p.off_rank = grade["off_grade"], grade["off_rank"]
            p.def_grade, p.def_rank = grade["def_grade"], grade["def_rank"]

    pool = {}

    # Skill players: merge ranks (authoritative for who exists) with projections.
    for key, rinfo in ranks.items():
        team = key[1]
        p = Player(
            name=rinfo["name"], team=team, position=rinfo["position"],
            points=proj.get(key, 0.0), fp_tier=rinfo["tier"], rk=rinfo["rk"],
            bye=rinfo["bye"], adp=sf_adp.get(key),
        )
        p.ta_tier = ta_tiers.get(normalize_name(p.name))
        apply_grade(p, grade_for_abbrev(team))
        pool[p.key] = p

    # K players
    for p in k_players_raw:
        p.adp = k_adp.get(p.key)
        apply_grade(p, grade_for_abbrev(p.team))
        pool[p.key] = p

    # DST players
    for p in dst_players_raw:
        p.adp = dst_adp.get(p.team)
        apply_grade(p, grade_for_abbrev(p.team))
        pool[p.key] = p

    return pool


# ---------------------------------------------------------------------------
# VORP calculation
# ---------------------------------------------------------------------------

def total_demand_by_position(pool):
    """Structural league-wide starter demand per position, computed once from
    the full player pool (dedicated slots + OP/superflex spillover).

    OP spillover: rank all OP-eligible players (QB/RB/WR/TE) by points, take
    the top (NUM_TEAMS * OP_SLOTS_PER_TEAM) that are NOT already the top
    dedicated-slot players at their position, and allocate them to whichever
    position they actually are.
    """
    demand = {pos: n * NUM_TEAMS for pos, n in DEDICATED_SLOTS_PER_TEAM.items()}

    op_pool = [p for p in pool.values() if p.position in OP_ELIGIBLE_POSITIONS]
    op_pool.sort(key=lambda p: p.points, reverse=True)

    # Track how many dedicated slots remain open per position as we walk the
    # sorted list; anyone beyond their position's dedicated allotment is a
    # spillover candidate for the OP pool.
    dedicated_remaining = {pos: DEDICATED_SLOTS_PER_TEAM[pos] * NUM_TEAMS
                            for pos in OP_ELIGIBLE_POSITIONS}
    spillover_candidates = []
    for p in op_pool:
        if dedicated_remaining.get(p.position, 0) > 0:
            dedicated_remaining[p.position] -= 1
        else:
            spillover_candidates.append(p)

    op_slots_total = NUM_TEAMS * OP_SLOTS_PER_TEAM
    for p in spillover_candidates[:op_slots_total]:
        demand[p.position] = demand.get(p.position, 0) + 1

    return demand


def compute_vorp(pool):
    """Recompute VORP for every player in-place. Call after every draft pick."""
    demand = total_demand_by_position(pool)

    drafted_count = {}
    available_by_pos = {}
    for p in pool.values():
        available_by_pos.setdefault(p.position, [])
        if p.drafted:
            drafted_count[p.position] = drafted_count.get(p.position, 0) + 1
        else:
            available_by_pos[p.position].append(p)

    replacement_level = {}
    for pos, players in available_by_pos.items():
        players.sort(key=lambda p: p.points, reverse=True)
        remaining_demand = max(0, demand.get(pos, 0) - drafted_count.get(pos, 0))
        if remaining_demand <= 0 or not players:
            replacement_level[pos] = players[-1].points if players else 0.0
        elif remaining_demand > len(players):
            replacement_level[pos] = players[-1].points
        else:
            # remaining_demand-th ranked player (1-indexed) among available
            replacement_level[pos] = players[remaining_demand - 1].points

    for p in pool.values():
        baseline = replacement_level.get(p.position, 0.0)
        p.vorp = round(p.points - baseline, 1)

    return replacement_level


# ---------------------------------------------------------------------------
# VONA calculation
# ---------------------------------------------------------------------------

def picks_until_next_turn(total_picks_made, my_slot, num_teams=NUM_TEAMS):
    """Snake draft: given total picks made so far (0-indexed count) and your
    draft slot (1-indexed), return picks remaining until your next turn."""
    current_overall_pick = total_picks_made + 1  # the upcoming pick

    def slot_on_the_clock(overall_pick):
        rnd = (overall_pick - 1) // num_teams
        pos_in_round = (overall_pick - 1) % num_teams
        if rnd % 2 == 0:
            return pos_in_round + 1
        return num_teams - pos_in_round

    pick = current_overall_pick
    while slot_on_the_clock(pick) != my_slot:
        pick += 1
        if pick - current_overall_pick > num_teams * 2:
            break  # safety valve, should never trigger
    return pick - current_overall_pick


def compute_vona(pool, picks_until_next):
    """Recompute VONA for every available player in-place."""
    available = [p for p in pool.values() if not p.drafted]
    available_sorted_by_adp = sorted(available, key=lambda p: p.adp_sort_key)

    expected_gone_keys = {p.key for p in available_sorted_by_adp[:picks_until_next]}

    by_position = {}
    for p in available:
        by_position.setdefault(p.position, []).append(p)

    for pos, players in by_position.items():
        players.sort(key=lambda p: p.points, reverse=True)
        if not players:
            continue
        # "Next available": the best player at this position NOT expected to
        # be drafted (by anyone) before your next turn.
        next_available = next((p for p in players if p.key not in expected_gone_keys),
                               players[-1])
        for p in players:
            p.vona = round(p.points - next_available.points, 1)