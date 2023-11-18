import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.animation as animation

if __name__ == "__main__":
    done = False

    while not done:
        # get frame to animate
        strweekID = input("Enter a weekID to animate (1-9) or \"q\" to quit: ")

        if strweekID == "q":
            break
        elif not strweekID.isdigit() or len(strweekID) > 1:
            print("Invalid week entered")
            continue

        week_done = False
        week = pd.read_csv("data/tracking_week_"+ strweekID + ".csv")
        while not week_done:
            strgameID = input("Enter a gameID to animate or \"q\" for a new week: ")

            if strgameID == "q":
                break
            elif not strgameID.isdigit():
                print("GameID is not a number")
                continue

            # get info from user inputs
            gameId = int(strgameID)
            game = week[week["gameId"] == gameId]

            if game.empty:
                print("Invalid game choice")
                continue

            game_done = False

            while not game_done:
                strplayID = input("Enter a playID to animate or \"q\" for a new game: ")

                if strplayID == "q":
                    break
                elif not strplayID.isdigit():
                    print("PlayID is not a number")
                    continue

                playId = int(strplayID)
                play = game[game["playId"] == playId]

                # only want plays where we can see snap-onwards
                if play.empty or play[play["event"] == "ball_snap"].size == 0:
                    print("Invalid week/game/play triple")
                    continue

                # get initial football position for normalization
                footpos = play[play["displayName"] == "football"][play["frameId"] == 1]["x"].iat[0]

                # normalize
                play["x"] -= footpos
                
                mirror = play[play["displayName"] == "football"][play["frameId"] == 1]["playDirection"].iat[0] == "left"

                # mirror plays based on direction
                if mirror:
                    play["x"] *= -1
                    play["y"] *= -1
                    play["y"] += 50

                # get players, and initialize things to store their positions
                players = play.displayName.unique()
                playerXPos = [[] for _ in players]
                playerYPos = [[] for _ in players]

                # get iterable of frames
                frames = range(1, play.frameId.max() + 1)

                # get first players team - arbitrarily red
                red = play[play["displayName"] == players[0]][play["frameId"] == 1]["club"].iat[0]
                
                # plot players on a matplot lib plot
                fig, ax = plt.subplots()
                lines = []

                # initial positions
                for i in range(len(players)):
                    # compute frame by frame positions and save
                    for frame in frames:
                        playX = play[play["displayName"] == players[i]][play["frameId"] == frame]["x"].iat[0]
                        playY = play[play["displayName"] == players[i]][play["frameId"] == frame]["y"].iat[0]
                        playerXPos[i].append(playX)
                        playerYPos[i].append(playY)
                    if play[play["displayName"] == players[i]][play["frameId"] == 1]["club"].iat[0] == red:
                        lines.append(ax.plot(playerXPos[i][1], playerYPos[i][1], color="red", alpha=0.4)[0])
                    elif play[play["displayName"] == players[i]][play["frameId"] == 1]["club"].iat[0] == "football":
                        lines.append(ax.plot(playerXPos[i][1], playerYPos[i][1], color="black")[0])
                    else:
                        lines.append(ax.plot(playerXPos[i][1], playerYPos[i][1], color="blue", alpha=0.4)[0])
                
                # create line of scrimmage
                scrimX = [0, 0, 0]
                scrimY = [0, 25, 50]
                
                lines.append(ax.plot(scrimX, scrimY, color="orange")[0])


                def update(frame):
                    # update lines to include this frame
                    for i in range(len(players)):
                        lines[i].set_xdata(playerXPos[i][:frame])
                        lines[i].set_ydata(playerYPos[i][:frame])
                    lines[-1].set_xdata(scrimX)
                    lines[-1].set_ydata(scrimY)
                    return tuple(lines)

                ani = animation.FuncAnimation(fig=fig, func=update, frames=frames, interval=200)
                plt.show()