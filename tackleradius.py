import pandas as pd
import math

if __name__ == "__main__":
    tackles = pd.read_csv("data/tackles_bc_id.csv")
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))

    seconds_out = .5
    frames_out = int(seconds_out * 10)

    while True:
        strid = input("Enter a tackler's nflId or a non-number to quit: ")

        if not strid.isdigit():
            break
        
        id = int(strid)

        player_tackles = tackles.loc[tackles["nflId"] == id].loc[tackles["pff_missedTackle"] == 0]

        if player_tackles.empty:
            print("Player has no tackles")
            continue
        
        distances = []

        for week in weeks:
            for row in player_tackles.itertuples():
                play = week[week["gameId"] == row.gameId].loc[week["playId"] == row.playId].loc
                if play.empty:
                    # wrong week
                    continue

                tackler_track = play.loc[play["nflId"] == id]
                carrier_track = play.loc[play["nflId"] == row.ballCarrierId]

                # if we cant find tackle_frame, skip
                if tackler_track.loc[tackler_track["event"] == "tackle"].empty:
                    continue
                tackle_frame = tackler_track.loc[tackler_track["event"] == "tackle"]["frameId"].iat[0]

                # find distance frame
                frame_of_tackle = max(tackle_frame - frames_out, 1)

                # find tackler and carrier info in that frame
                tackler_frame = tackler_track.loc[tackler_track["frameId"] == frame_of_tackle]
                carrier_frame = carrier_track.loc[carrier_track["frameId"] == frame_of_tackle]

                # # if we cant find start frame, skip
                # if not carrier_track.loc[carrier_track["event"] == "run"].empty:
                #     start_frame = carrier_track.loc[carrier_track["event"] == "run"]["frameId"].iat[0]
                # elif not carrier_track.loc[carrier_track["event"] == "handoff"].empty:
                #     start_frame = carrier_track.loc[carrier_track["event"] == "handoff"]["frameId"].iat[0]
                # elif not carrier_track.loc[carrier_track["event"] == "pass_outcome_caught"].empty:
                #     start_frame = carrier_track.loc[carrier_track["event"] == "pass_outcome_caught"]["frameId"].iat[0]
                # else:
                #     continue
                    
                

                # tackler_start = tackler_track[tackler_track["frameId"] == start_frame]
                # carrier_start = carrier_track[carrier_track["frameId"] == start_frame]

                # math.sqrt((tackler_start["x"].iat[0] - carrier_start["x"].iat[0]) ** 2 + (tackler_start["y"].iat[0] - carrier_start["y"].iat[0]) ** 2)
                distances.append(math.sqrt((tackler_frame["x"].iat[0] - carrier_frame["x"].iat[0]) ** 2 + (tackler_frame["y"].iat[0] - carrier_frame["y"].iat[0]) ** 2))
        
        print(max(distances))
        print(sum(distances)/len(distances))
        print(distances)

                
                

        

