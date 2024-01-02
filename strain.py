import math
import pandas as pd
import matplotlib.pyplot as plt


if __name__ == "__main__":
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))
    print("Read weeks")

    plays = pd.read_csv("data/plays.csv")
    print("Read plays")

    ids_to_data = {}
    counter = 0

    for row in plays.itertuples():
        counter += 1
        if counter % 100 == 0:
            print(f"{counter/len(plays)}% Done")

        print(f"Play: {row.gameId}{row.playId}")
        
        for week in weeks:
            play = week.loc[week["gameId"] == row.gameId].loc[week["playId"] == row.playId]
            if play.empty:
                # wrong week
                continue

            carrier_track = play.loc[play["nflId"] == row.ballCarrierId]

            if carrier_track.empty:
                break # can't use this play

            defense = play.loc[play["club"] != carrier_track["club"].iat[0]].loc[play["club"] != "football"]

            for defender in defense.nflId.unique():
                strains = []
                success = True

                defender_track = defense.loc[defense["nflId"] == defender]
                for def_frame in defender_track.itertuples():
                    if def_frame.frameId == 1:
                        continue
                    
                    carrier_frame = carrier_track.loc[carrier_track["frameId"] == def_frame.frameId]
                    carrier_frame_prev = carrier_track.loc[carrier_track["frameId"] == def_frame.frameId - 1]
                    def_frame_prev = defender_track.loc[defender_track["frameId"] == def_frame.frameId - 1]

                    if carrier_frame.empty or carrier_frame_prev.empty or def_frame_prev.empty:
                        success = False
                        break

                    curr_dist = math.sqrt((def_frame.x - carrier_frame["x"].iat[0]) ** 2 + (def_frame.y - carrier_frame["y"].iat[0]) ** 2)

                    if curr_dist == 0:
                        # seems reasonable to call the play dead if the tackler is directly ontop of the ball carrier - divide by zero error otherwise
                        # keeping success as true, since I think this seems like a reasonable calculation in the case where the tackler reaches the carrier
                        break 

                    prev_dist = math.sqrt((def_frame_prev["x"].iat[0] - carrier_frame_prev["x"].iat[0]) ** 2 + (def_frame_prev["y"].iat[0] - carrier_frame_prev["y"].iat[0]) ** 2)
                    strains.append((-(curr_dist - prev_dist)/.1)/curr_dist)
                
                if success and len(strains) != 0:
                    if defender in ids_to_data:
                        ids_to_data[defender].append(sum(strains)/len(strains))
                    else:
                        ids_to_data[defender] = [sum(strains)/len(strains)]

    ids = []
    maxes = []
    means = []
    occs = []

    for player in ids_to_data:
        ids.append(math.floor(player))
        maxes.append(max(ids_to_data[player]))
        means.append(sum(ids_to_data[player])/len(ids_to_data[player]))
        occs.append(len(ids_to_data[player]))

    d = {"tacklerId": ids, "maxAvgStrain": maxes, "avgAvgStrain": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/strain.csv", index=False)

