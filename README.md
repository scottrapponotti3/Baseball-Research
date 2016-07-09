# Baseball-Research
Code to create the data set from pitch fx data and season statsitics from the Lahman database

Probably the most repeated phrase when it comes to hitting is "keep your eye on the ball". That is not possible because before the ball can cross the plate, the angular velocity is so great that the human eye cannot detect it. Because of this problem, hitters have to anticipate and predict where the location of the ball is going to be, and time it correctly to make optimal contact. That is the idea behind this research project: to create models based on the information that a hitter takes that can form predictions about the final locations. Right now this project is ongoing, so all I can show is the process of constructing the data sets for future models. 

The data used in this project was from a Pitch/fx database I created by scraping every pitch from 2008 to 2014. While there are already databases from Baseball Heat Maps (http://www.baseballheatmaps.com/pitch-fx-download/), I wanted to create my own to famialize myself more with working with Pitch/fx data. Once I scraped all the data, I ran several queries on MySQL to join the Pitching and Batting tables to create a new data frame baseball_data. This data frame contains the pitcher and batter names, pitch_types, at bat result, date, and all the pitch information: final horizontal and vertical ball position, velocity, acceleration along with the spin rates and direction. I wouldn't be using all these variables in my model, but I kept in the data set in case I needed it. 


