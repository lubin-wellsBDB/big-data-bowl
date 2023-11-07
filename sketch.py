import pandas as pd

tackles = pd.read_csv("data/tackles.csv")
tacklerIDs = tackles.nflId.dropna().unique()

players = pd.read_csv("data/players.csv")

data = []

for id in tacklerIDs:
    # calculate num tackles for a player - both tackle and assist counted as full credit
    num_tackles = sum(tackles[tackles["nflId"] == id].tackle) + sum(tackles[tackles["nflId"] == id].assist)

    # calculate num attempts for a player
    num_atts = num_tackles + sum(tackles[tackles["nflId"] == id].pff_missedTackle)

    # calculate success rate when tackling
    succ_rate = num_tackles/num_atts

    # fumbles forced
    num_fumbles = sum(tackles[tackles["nflId"] == id].forcedFumble)

    data.append((num_tackles, succ_rate, players[players["nflId"] == id].displayName.to_string(), num_fumbles))

# sort by number of tackles
data.sort(key=lambda a: a[3])

# print the players data nicely
for player in data:
    print(f"{player[2]} succeeded {player[1] * 100}% of the time, and forced {player[3]} fumbles on {player[0]} attempts.")