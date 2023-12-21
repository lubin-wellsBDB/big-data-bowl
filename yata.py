import math
import pandas as pd
import matplotlib.pyplot as plt


if __name__ == "__main__":
    tackles = pd.read_csv("data/tackles_bc_id.csv")
    print("Read tackles")
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
    all_dists = []
    all_times = []

    for tackler in tacklers:
        print(tackler)
        player_tackles = tackles.loc[tackles["nflId"] == tackler].loc[tackles["pff_missedTackle"] == 0]

        dists = []

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
                tackle_row = tackler_track.loc[tackler_track["event"] == "tackle"]

                if contact_row.empty or tackle_row.empty:
                    break # can't use this data - doesn't have contact or tackle labeled

                contact_frame = contact_row["frameId"].iat[0]
                tackle_frame = tackle_row["frameId"].iat[0]

                carrier_frame_cont = carrier_track.loc[carrier_track["frameId"] == contact_frame]
                carrier_frame_tackle = carrier_track.loc[carrier_track["frameId"] == tackle_frame]

                if carrier_frame_cont.empty or carrier_frame_tackle.empty or tackle_frame - contact_frame > 50:
                    break

                dist = math.sqrt((carrier_frame_cont["x"].iat[0] - carrier_frame_tackle["x"].iat[0]) ** 2 + (carrier_frame_cont["y"].iat[0] - carrier_frame_tackle["y"].iat[0]) ** 2)
                dists.append(dist)
                all_dists.append(dist)
                all_times.append(tackle_frame - contact_frame)
                break
        
        if len(dists) != 0:
            ids.append(tackler)
            maxes.append(max(dists))
            means.append(sum(dists)/len(dists))
            occs.append(len(dists))

    d = {"tacklerId": ids, "maxYata": maxes, "avgYata": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/yata.csv", index=False)

    plt.hist(all_dists, bins=range(1, 40))
    plt.show()
    plt.scatter(all_times, all_dists)
    plt.xlabel("Contact to Down Time (Frames)")
    plt.ylabel("Contact to Down Distance (Yards)")
    plt.show()