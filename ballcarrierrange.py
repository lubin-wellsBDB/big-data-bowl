import math
import pandas as pd


if __name__ == "__main__":
    tackles = pd.read_csv("data/tackles_bc_id.csv")
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))

    seconds_out = .5
    frames_out = int(seconds_out * 10)

    ballCarriers = tackles.ballCarrierId.unique()

    ids = []
    maxes = []
    means = []
    occs = []

    for carrier in ballCarriers:
        print(carrier)
        player_tackles = tackles.loc[tackles["ballCarrierId"] == carrier].loc[tackles["pff_missedTackle"] == 0]

        distances = []

        for week in weeks:
            for row in player_tackles.itertuples():
                play = week.loc[week["gameId"] == row.gameId].loc[week["playId"] == row.playId]
                if play.empty:
                    # wrong week
                    continue

                tackler_track = play.loc[play["nflId"] == row.nflId]
                carrier_track = play.loc[play["nflId"] == carrier]

                # if we cant find tackle_frame, skip
                if tackler_track.loc[tackler_track["event"] == "tackle"].empty:
                    continue
                tackle_frame = tackler_track.loc[tackler_track["event"] == "tackle"]["frameId"].iat[0]

                # find distance frame
                frame_of_tackle = max(tackle_frame - frames_out, 1)

                # find tackler and carrier info in that frame
                tackler_frame = tackler_track.loc[tackler_track["frameId"] == frame_of_tackle]
                carrier_frame = carrier_track.loc[carrier_track["frameId"] == frame_of_tackle]
                distances.append(math.sqrt((tackler_frame["x"].iat[0] - carrier_frame["x"].iat[0]) ** 2 + (tackler_frame["y"].iat[0] - carrier_frame["y"].iat[0]) ** 2))
        
        if len(distances) != 0:
            ids.append(carrier)
            maxes.append(max(distances))
            means.append(sum(distances)/len(distances))
            occs.append(len(distances))



    d = {"ballCarrierId": ids, "maxTackleRad": maxes, "meanTackleRad": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/ballcarrier_range.csv", index=False)
