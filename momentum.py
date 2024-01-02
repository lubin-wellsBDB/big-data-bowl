import math
import pandas as pd


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

    ids = []
    maxes = []
    max_play = []
    means = []
    occs = []

    for tackler in tacklers:
        print(tackler)
        player_tackles = tackles.loc[tackles["nflId"] == tackler].loc[tackles["tackle"] == 1]

        impulses = []

        for row in player_tackles.itertuples():
            for week in weeks:
                play = week.loc[week["gameId"] == row.gameId].loc[week["playId"] == row.playId]
                if play.empty:
                    # wrong week
                    continue

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
                impulses.append((delta_velocity * players[players["nflId"] == row.ballCarrierId]["weight"].iat[0], str(row.gameId) + str(row.playId)))
                break
        
        if len(impulses) != 0:
            ids.append(tackler)
            maxes.append(max(impulses)[0])
            max_play.append(max(impulses)[1])
            means.append(sum([x[0] for x in impulses])/len(impulses))
            occs.append(len(impulses))

    d = {"tacklerId": ids, "maxImpulse": maxes, "maxPlay": max_play, "avgImpulse": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/momentum_with_play.csv", index=False)
