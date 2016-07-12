# Baseball-Research
Code to create the data set from pitch fx data and season statsitics from the Lahman database

One of the most repeated phrases when it comes to hitting is "keep your eye on the ball". It is impossible to track the ball all the way from the pitcher's hand to home plate because before the ball can cross the plate the angular velocity is so great that the human eye cannot detect it. Because of this problem, hitters have to anticipate and predict where the location of the ball is going to be and time it correctly to make optimal contact. That is the idea behind this research project: to create models based on the information that a hitter observes to form predictions about the final pitch locations. Right now this project is ongoing, so all I can show is the process of constructing the data sets for future models. 

The data used in this project was from a Pitch/fx database I created by scraping every pitch from 2008 to 2014. While there is already a database from Baseball Heat Maps (http://www.baseballheatmaps.com/pitch-fx-download/), I wanted to create my own to famialize myself more with working with Pitch/fx data. Once I scraped all the data, I ran several queries on MySQL to join the Pitching and Batting tables to create a new data frame baseball_data. This data frame contains the pitcher and batter names, pitch_types, at bat result, date, and all the pitch information: final horizontal and vertical ball position, velocity, acceleration along with the spin rates and direction. I wouldn't be using all these variables in my model, but I kept in the data set in case I needed it. 

Essentially I wanted to look at three questions with this research:

**1. How predictive is a given pitch at a particaular time during the flight or distance from the plate, to the final location of the pitch?**

**2. How do the models of the pitch at different times and distances compare?**

**3. How do we utilize this information and what does this mean? Does a pitcher who is more difficult to predict their pitch locations have a greater rate of success using season statistics (K/9, BB/9, BAOpp)? Does this change with age?**

When decising on how to create my model, I had to investigate the variables that a batter would see as the pitch would cross the plate, and create plots to look for relationships. I created two functions t_predictor and distance_predictor that would calculate the relative position and velocity of the pitch in both the x and z directions using kinematic equations, and merged the season statistics from the Lahman database with that of the Pitch/fx data. Then I created my testing and training sets that would be used for modeling and testing.

These two scatterplots are of the final x and z positions (px and pz) and how they relate to the position and velocity (x_t,z_t, vx, and vz) at a time of 20s from the release of the ball. We can already see that the starting position of the ball is going to be a factor in the model due to the fact that the handedness of the pitcher.

![scatterplot1](https://cloud.githubusercontent.com/assets/20291218/16707492/f01d8bf0-459d-11e6-9d10-d0b60725215a.jpeg)


![scatterplot2](https://cloud.githubusercontent.com/assets/20291218/16707493/1ba802a0-459e-11e6-9a9c-7ff013e85ee1.jpeg)








