from sleeper.api import DraftAPIClient, PlayerAPIClient
from sleeper.enum import Sport
from sleeper.model import Draft, DraftPick, PlayerDraftPick, Player
import json
from dataclasses import asdict, is_dataclass

USER_ID = "1128735683528507392"
LEAGUE_ID = "1128738585244545024"
DRAFT_ID = "1128738586225946624"
MOCK_DRAFT_ID = "1136332496670453760"




def position_to_str(position):
    return str(position)[-2:]

def picks_to_json(draft_picks):
    blob = [
        {
            "name": f'{pick.metadata.first_name} {pick.metadata.last_name}',
            "position": position_to_str(pick.metadata.position),
            "player_id": pick.metadata.player_id,
            "team": position_to_str(pick.metadata.team)
        } for pick in draft_picks
    ]

    return json.dumps(blob)




###########################

def custom_encoder(obj):
    if isinstance(obj, Enum):  # Assuming PlayerStatus, PlayerPosition are enums
        return obj.name
    elif is_dataclass(obj):
        return asdict(obj)
    elif hasattr(obj, '__dict__'):  # If it's a custom object, convert to dict
        return obj.__dict__
    else:
        return str(obj)  # Fallback for other types


nfl_players: dict[str, Player] = PlayerAPIClient.get_all_players(sport=Sport.NFL)

def players_to_json(nfl_players):
    blob = [
        json.dumps(player, default=custom_encoder, indent=4) for player in nfl_players
    ]

    return blob



if __name__ == "__main__":
    # get all drafts that a user was in for a particular year
    user_drafts: list[Draft] = DraftAPIClient.get_user_drafts_for_year(
        user_id=USER_ID, sport=Sport.NFL, year="2024"
    )

    # get all drafts for a particular league
    league_drafts: list[Draft] = DraftAPIClient.get_drafts_in_league(league_id=LEAGUE_ID)

    # get a draft by its ID
    draft: Draft = DraftAPIClient.get_draft(draft_id=DRAFT_ID)

    # get all draft picks for a particular draft
    draft_picks: list[PlayerDraftPick] = DraftAPIClient.get_player_draft_picks(
        draft_id=MOCK_DRAFT_ID, sport=Sport.NFL
    )

    # get all traded draft picks for a particular draft
    # traded_draft_picks: list[DraftPick] = DraftAPIClient.get_traded_draft_picks(
    #     draft_id="my_draft_id"
    # )