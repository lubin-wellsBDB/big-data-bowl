import math
import pandas as pd


if __name__ == "__main__":
    tackles = pd.read_csv("data/tackles_bc_id.csv")
    print("read tackles")
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))
    print("read weeks")

    tacklers = tackles.nflId.unique()

    ids = []
    maxes = []
    means = []
    occs = []

    for tackler in tacklers:
        print(tackler)
        player_tackles = tackles.loc[tackles["nflId"] == tackler].loc[tackles["pff_missedTackle"] == 0]


        # the following code is based off of Nguyen et al.'s STRAIN paper
        # We did not use their code, but this is almost directly following
        # their calculations - except we average strain by play as opposed to
        # by frame - partially for ease, and partially because we felt that 
        # made more sense.
        averages = []

        for week in weeks:
            for row in player_tackles.itertuples():
                play = week.loc[week["gameId"] == row.gameId].loc[week["playId"] == row.playId]
                if play.empty:
                    # wrong week
                    continue

                tackler_track = play.loc[play["nflId"] == row.nflId]
                carrier_track = play.loc[play["nflId"] == row.ballCarrierId]

                success = True
                strains = []

                # find tackler and carrier info in that frame
                for tackler_frame in tackler_track.itertuples():
                    if tackler_frame.frameId == 1:
                        continue


                    carrier_frame = carrier_track.loc[carrier_track["frameId"] == tackler_frame.frameId]
                    carrier_frame_prev = carrier_track.loc[carrier_track["frameId"] == tackler_frame.frameId - 1]
                    tackler_frame_prev = tackler_track.loc[tackler_track["frameId"] == tackler_frame.frameId - 1]

                    if carrier_frame.empty or carrier_frame_prev.empty or tackler_frame_prev.empty:
                        success = False
                        break

                    curr_dist = math.sqrt((tackler_frame.x - carrier_frame["x"].iat[0]) ** 2 + (tackler_frame.y - carrier_frame["y"].iat[0]) ** 2)

                    if curr_dist == 0:
                        # seems reasonable to call the play dead if the tackler is directly ontop of the ball carrier - divide by zero error otherwise
                        # keeping success as true, since I think this seems like a reasonable calculation in the case where the tackler reaches the carrier
                        break 

                    prev_dist = math.sqrt((tackler_frame_prev["x"].iat[0] - carrier_frame_prev["x"].iat[0]) ** 2 + (tackler_frame_prev["y"].iat[0] - carrier_frame_prev["y"].iat[0]) ** 2)
                    strains.append((-(curr_dist - prev_dist)/.1)/curr_dist)
                
                if success and len(strains) != 0:
                    averages.append(sum(strains)/len(strains))
        
        if len(averages) != 0:
            ids.append(tackler)
            maxes.append(max(averages))
            means.append(sum(averages)/len(averages))
            occs.append(len(averages))



    d = {"tacklerId": ids, "maxAvgStrain": maxes, "avgAvgStrain": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/tackler_avg_strain.csv", index=False)
