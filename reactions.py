import math
import pandas as pd
import matplotlib.pyplot as plt


if __name__ == "__main__":
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))
    print("Read weeks")

    plays = pd.read_csv("data/plays.csv")

    ids_to_data = {}

    for row in plays.itertuples():
        for week in weeks:
            play = week.loc[week["gameId"] == row.gameId].loc[week["playId"] == row.playId]
            if play.empty:
                # wrong week
                continue

            carrier_track = play.loc[play["nflId"] == row.ballCarrierId]

            if carrier_track.empty:
                break # can't use this play

            start_frame = 0

            handoff_row = carrier_track.loc[carrier_track["event"] == "handoff"]
            pass_caught_row = carrier_track.loc[carrier_track["event"] == "pass_outcome_caught"]
            run_row = carrier_track.loc[carrier_track["event"] == "run"]

            if not handoff_row.empty:
                if not pass_caught_row.empty or not run_row.empty:
                    break # not sure when to start tracking the bc in this case
                start_frame = min(1, handoff_row["frameId"].iat[0])

            if not pass_caught_row.empty:
                if not handoff_row.empty or not run_row.empty:
                    break # not sure when to start tracking the bc in this case
                start_frame = min(1, pass_caught_row["frameId"].iat[0])

            if not run_row.empty:
                if not handoff_row.empty or not pass_caught_row.empty:
                    break # not sure when to start tracking the bc in this case
                start_frame = min(1, run_row["frameId"].iat[0])
            
            if start_frame == 0:
                break # couldn't find the frame to start tracking with

            defense = play.loc[play["club"] != carrier_track["club"].iat[0]].loc[play["club"] != "football"]

            for defender in defense.nflId.unique():
                defender_track = defense.loc[defense["nflId"] == defender]
                for def_row in defender_track.itertuples():
                    if def_row.frameId < start_frame:
                        continue
                    carrier_row = carrier_track[carrier_track["frameId"] == def_row.frameId]
                    # have to flip x and y because of how direction is defined in the data
                    theta = math.degrees(math.atan2(carrier_row["x"].iat[0] - def_row.x, carrier_row["y"].iat[0] - def_row.y))
                    if theta < 0:
                        theta = 360 + theta
                    print(theta - def_row.o)
                quit()

    # d = {"tacklerId": ids, "maxImpulse": maxes, "avgImpulse": means, "occurances":occs}
    # df = pd.DataFrame(data=d)
    # df.to_csv("data/reactions.csv", index=False)
