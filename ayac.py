import math
import pandas as pd


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
                football_track = play.loc[play["displayName"] == "football"]

                # Can do this with either the tackler or football tracking - works either way
                contact_row = tackler_track.loc[tackler_track["event"] == "first_contact"]
                tackle_row = tackler_track.loc[tackler_track["event"] == "tackle"]

                if contact_row.empty or tackle_row.empty:
                    break # can't use this data - doesn't have contact or tackle labeled

                contact_frame = contact_row["frameId"].iat[0]
                tackle_frame = tackle_row["frameId"].iat[0]

                football_frame_cont = football_track.loc[football_track["frameId"] == contact_frame]
                football_frame_tackle = football_track.loc[football_track["frameId"] == tackle_frame]

                if football_frame_cont.empty or football_frame_tackle.empty or tackle_frame - contact_frame > 50:
                    break

                mirror = play.loc[play["displayName"] == "football"].loc[play["frameId"] == 1]["playDirection"].iat[0] == "left"

                dist = football_frame_tackle["x"].iat[0] - football_frame_cont["x"].iat[0]

                if mirror:
                    dist = -1 * dist
                
                if dist > 0:
                    dists.append(dist)
                else:
                    dists.append(0) # forward progress
                break
        
        if len(dists) != 0:
            ids.append(tackler)
            maxes.append(max(dists))
            means.append(sum(dists)/len(dists))
            occs.append(len(dists))

    d = {"tacklerId": ids, "maxAyac": maxes, "avgAyac": means, "occurances":occs}
    df = pd.DataFrame(data=d)
    df.to_csv("data/ayac.csv", index=False)
