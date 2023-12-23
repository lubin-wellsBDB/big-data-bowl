import math
import pandas as pd
import matplotlib.pyplot as plt


if __name__ == "__main__":
    tackles = pd.read_csv("data/tackles_bc_id.csv")
    print("Read tackles")

    players = pd.read_csv("data/players.csv")
    print("Read players")
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))
    print("Read weeks")

    tacklers = tackles.nflId.unique()
    missed_tackles = tackles.loc[tackles["pff_missedTackle"] == 1].whole_id

    ids = []
    maxes = []
    means = []
    occs = []

    for tackler in tacklers:
        print(tackler)
        player_tackles = tackles.loc[tackles["nflId"] == tackler].loc[tackles["pff_missedTackle"] == 0]

        impulses = []

        for row in player_tackles.itertuples():
            for week in weeks:
                play = week.loc[week["gameId"] == row.gameId].loc[week["playId"] == row.playId]
                if play.empty:
                    # wrong week
                    continue

                if str(row.gameId) + " " + str(row.playId) in missed_tackles:
                    break # this might mess with data - unclear how first contact works

                tackler_track = play.loc[play["nflId"] == row.nflId]
                carrier_track = play.loc[play["nflId"] == row.ballCarrierId]

                # Can do this with either the tackler or ballcarrier tracking - works either way
                contact_row = tackler_track.loc[tackler_track["event"] == "first_contact"]
                tackle_row = tackler_track.loc[tackler_track["event"] == "tackle"] # dont need for tracking, but using to remove outliers.

                if contact_row.empty or tackle_row.empty:
                    break # can't use this data - doesn't have contact or tackle labeled

                contact_frame = contact_row["frameId"].iat[0]
                next_frame = contact_frame + 1
                tackle_frame = tackle_row["frameId"].iat[0]

                carrier_frame_cont = carrier_track.loc[carrier_track["frameId"] == contact_frame]
                carrier_frame_next = carrier_track.loc[carrier_track["frameId"] == next_frame]

                if carrier_frame_cont.empty or carrier_frame_next.empty or tackle_frame - contact_frame > 50:
                    break

                delta_velocity = math.sqrt((carrier_frame_cont["s"].iat[0] * math.cos(math.radians(carrier_frame_cont["dir"].iat[0])) - carrier_frame_next["s"].iat[0] * math.cos(math.radians(carrier_frame_next["dir"].iat[0]))) ** 2 
                                 + (carrier_frame_cont["s"].iat[0] * math.sin(math.radians(carrier_frame_cont["dir"].iat[0])) - carrier_frame_next["s"].iat[0] * math.sin(math.radians(carrier_frame_cont["dir"].iat[0]))) ** 2)
                impulses.append(delta_velocity * players[players["nflId"] == row.ballCarrierId]["weight"].iat[0])
                break
        
        if len(impulses) != 0:
            ids.append(tackler)
            maxes.append(max(impulses))
            means.append(sum(impulses)/len(impulses))
            occs.append(len(impulses))

    d = {"tacklerId": ids, "maxImpulse": maxes, "avgImpulse": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/momentum.csv", index=False)
