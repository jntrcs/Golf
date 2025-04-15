import time
import requests
from bs4 import BeautifulSoup
import re
import json
import pandas as pd



def get_player_id_from_row(player_row):
    """
    Given a BeautifulSoup element for a single player's row,
    find the anchor tag containing the player's name and parse
    the player id from the href.
    """
    # Find the <a> element
    anchor = player_row.find("a", class_="AnchorLink leaderboard_player_name")
    if not anchor or not anchor.has_attr("href"):
        return None
    
    # Extract href
    href = anchor["href"]
    
    # Use a regex to capture the number that appears after "/id/"
    match = re.search(r'/id/(\d+)/', href)
    if match:
        return match.group(1)
    else:
        return None


def parse_player_json_to_df(player_json_str, tournament_name):
    """
    Given a JSON string (player_json_str) representing one player's info from ESPN,
    plus a string for 'tournament_name', return a Pandas DataFrame with columns:
        - tournament
        - playerName
        - round
        - hole
        - score
        - par
    """
    data = json.loads(player_json_str)

    # Grab the competitor (player) info
    competitor = data.get("competitor", {})
    player_name = competitor.get("displayName", "Unknown")
    print(player_name)

    # Prepare a list to hold row dictionaries
    rows = []

    # 'rounds' is a list; each element corresponds to one round
    rounds = data.get("rounds", [])
    for round_data in rounds:
        round_number = round_data.get("period")  # e.g. 1, 2, 3, 4

        # linescores is a list of hole-by-hole details for this round
        linescores = round_data.get("linescores", [])
        for hole_info in linescores:
            # The 'period' key for each hole is the hole number
            hole_number = hole_info.get("period")
            score_value = hole_info.get("value")
            hole_par = hole_info.get("par")

            # Append a new row to our list
            rows.append({
                "tournament": tournament_name,
                "playerName": player_name,
                "round": round_number,
                "hole": hole_number,
                "score": score_value,
                "par": hole_par
            })

    # Build a DataFrame from the list of dicts
    df = pd.DataFrame(rows, columns=["tournament", "playerName", "round", "hole", "score", "par"])
    return df



def scrape_tournament(tournament_id, year=2024):
    """
    Scrape a single tournament from ESPN to extract hole-by-hole data.
    Returns a list of dicts, each with player, event, hole, par, score, round, etc.
    """
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) \
                   AppleWebKit/537.36 (KHTML, like Gecko) \
                   Chrome/98.0.4758.102 Safari/537.36"
    }
    url = "https://www.espn.com/golf/leaderboard/_/tournamentId/"+str(tournament_id)
    response = requests.get(url, headers=headers)
    if response.status_code != 200:
        print(f"Error fetching {url}: status {response.status_code}")
        return []
    
    soup = BeautifulSoup(response.text, "html.parser")

    tournament_df = pd.DataFrame()
    
    # The structure on ESPN may differ; this is just a conceptual outline.
    tournament_name = soup.find("h1", class_="headline").get_text(strip=True) if soup.find("h1", class_="headline") else "Unknown"
    
    # Suppose ESPN’s leaderboard has sections for each player with hole-by-hole details
    # e.g., <table class="Table ..."> or some other structure
    players_data = []

    player_rows = soup.select("tr.PlayerRow__Overview")
    for player_row in player_rows:
        player_id = get_player_id_from_row(player_row)
        player_url = (
            "https://site.web.api.espn.com/apis/site/v2/sports/golf/pga/leaderboard/"
            + str(tournament_id)
            + "/competitorsummary/"
            + str(player_id)
            + "?region=us&lang=en&season="
            + str(year)
        )

    # Try the request once, if it fails with ConnectionError, wait and try again
        try:
            player_response = requests.get(player_url)
        except requests.exceptions.ConnectionError as e:
            print(f"Connection error: {e}")
            print("Waiting 10 seconds before retrying...")
            time.sleep(10)
            player_response = requests.get(player_url)  # second attempt

    # Parse the response into a DataFrame
        df = parse_player_json_to_df(player_response.text, tournament_name)
        tournament_df = pd.concat([tournament_df, df], ignore_index=True)

        time.sleep(0.3)  # short delay between requests
        
    
    return tournament_df


def scrape_year(start_range, end_range, year):
    data  = pd.DataFrame()
    tournament_ids = range(start_range, end_range+1)
    for tournament_id in tournament_ids:
        print(tournament_id)
        tournament_df = scrape_tournament(tournament_id, year)
        data  = pd.concat([data, tournament_df], ignore_index=True)
        data.to_csv("golfer"+str(year)+"_last_two.csv", index=False)
        time.sleep(2)

def main():
    #original range was 401580329, 401580366
    #need to check that I have tournament 401580340
    #scrape_year(401580329, 401580366, 2024)
    scrape_year(401703502, 401703503, 2025)
    print("Finished scraping.")



if __name__ == "__main__":
    #main()
    data = scrape_tournament(401703504, year=2025)
    data.to_csv("/Users/jacksoncurtis/Documents/Masters/latest_masters_data.csv", index=False)

