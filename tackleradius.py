import pandas as pd
import math

if __name__ == "__main__":
    tackles = pd.read_csv("data/tackles.csv")
    weeks = []

    for week in range(1, 10):
        weeks.append(pd.read_csv(f"data/tracking_week_{week}.csv"))
    
    plays = pd.read_csv("data/plays.csv")

    seconds_out = .5
    frames_out = int(seconds_out * 10)

    while True:
        strid = input("Enter a tackler's nflId or a non-number to quit: ")

        if not strid.isdigit():
            break
        
        id = int(strid)

        player_tackles = tackles[tackles["nflId"] == id][tackles["pff_missedTackle"] == 0]

        if player_tackles.empty:
            print("Player has no tackles")
            continue
        
        distances = []

        for week in weeks:
            for row in player_tackles.itertuples():
                play = week[week["gameId"] == row.gameId][week["playId"] == row.playId]
                if play.empty:
                    # wrong week
                    continue

                play_data = plays[plays["gameId"] == row.gameId][plays["playId"] == row.playId]

                # if we can't find ball carrier, skip
                if play_data.empty:
                    continue
                ball_carrier = play_data["ballCarrierId"].iat[0]

                tackler_track = play[play["nflId"] == id]
                carrier_track = play[play["nflId"] == ball_carrier]

                # if we cant find tackle_frame, skip
                if tackler_track[tackler_track["event"] == "tackle"].empty:
                    continue
                tackle_frame = tackler_track[tackler_track["event"] == "tackle"]["frameId"].iat[0]

                # find distance frame
                frame_of_interest = max(tackle_frame - frames_out, 1)

                # find tackler and carrier info in that frame
                tackler_frame = tackler_track[tackler_track["frameId"] == frame_of_interest]
                carrier_frame = carrier_track[carrier_track["frameId"] == frame_of_interest]

                distances.append(math.sqrt((tackler_frame["x"].iat[0] - carrier_frame["x"].iat[0]) ** 2 + (tackler_frame["y"].iat[0] - carrier_frame["y"].iat[0]) ** 2))
        
        print(max(distances))
        print(sum(distances)/len(distances))
        print(distances)

                
                

        

