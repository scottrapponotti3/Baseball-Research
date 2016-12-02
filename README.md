# Baseball-Research

One of the most repeated phrases when it comes to hitting is "keep your eye on the ball". It is impossible to track the ball all the way from the pitcher's hand to home plate because before the ball can cross the plate the angular velocity is so great that the human eye cannot detect it. Because of this problem, hitters have to anticipate and predict where the location of the ball is going to be and time it correctly to make optimal contact. That is the idea behind this research project: to create models based on the information that a hitter observes to form predictions about the final pitch locations and times to quantify the pitcher's deception, and identify the significance of that deception and how it can be used to find hitter's reaction times.

The raw data comes from a Pitch/fx database I created by scraping every pitch from 2008 to 2014. Once I scraped all the data, I ran several queries on MySQL to join and subset the Pitching and Batting tables to create a new data frame baseball_data. This data frame contains the pitcher and batter names, pitch_types, at bat result, date, and all the pitch information: final horizontal and vertical ball position, velocity, acceleration along with the spin rates and direction. I wouldn't be using all these variables in my model, but I kept in the data set in case I needed it.

So how do we quantify a pitcher's deception? Well hitters track the pitch coming out of the pitcher's hand, so we can infer some predictor variables based on what information hitters have. I can determine the pitch location or velcoity during the flight of the ball by constructing functions on R to apply kinematic equations to the PITCH/fx data, then we can predict final pitch locations and times from the ball's position at any time or distance. 

I used smoothing splines to create generalized additive models of each type of deception (x position, z position and time) for highest predictive accuracy, and applied generalized cross validation to find the optimal degrees of freedom for the smoothing factor. I quantified the deception at a given distance from the plate as the squared difference between the actual final position and remaining time and the predicted value. By iterating over many distances as the ball approaches the plate, each pitcher's deception can be visualized.

In summary I wanted to look at two questions with this research:

**1. What is pitcher's positional and time deception, and how are these deceptions related to each other and overall pitcher success, such as Runs and Pitch Values.**

**2. Iterate the deceptions over many distances, to identify different clusters of each pitcher's deception, and identifythe success each hitter has against each cluster to determine hitter reaction times, and how well each hitter performs against certian types of pitchers.**

In the figure below is the x-deception (the average squared difference of the final x position and the predicted x) for 10 different pitcher and the year they pitched over a distance from the plate of 45ft to 5ft. As you can see the pitcher's had different shapes of curves that all converged to a deception of about 0 when the ball reaches the plate. Clayton Kershaw and David Price had the highest deception, meaning that overall their pitches were more difficult to predict in the x direction.

![xdeception_distance](https://cloud.githubusercontent.com/assets/20291218/20826989/bfa38eea-b83c-11e6-9657-7c6ea641c972.jpeg)

In the figure below is the z-deception (the average squared difference of the final z position and the predicted z) for 10 different pitcher and the year they pitched over a distance from the plate of 45ft to 5ft. Here the z deception plots are much more scattered and don't have a set pattern like the x and time deception, making it more difficult to cluster differents pitchers.

![zdeception_distance2](https://cloud.githubusercontent.com/assets/20291218/20827215/6fe7db34-b83e-11e6-8fce-33881667706b.jpeg)

In the figure below is the time-deception (the average squared difference of the remaining time for the ball to cross the plate and the predicted time) for 10 different pitcher and the year they pitched over a distance from the plate of 45ft to 5ft.

![tdeception_distance](https://cloud.githubusercontent.com/assets/20291218/20828888/9f4393f6-b847-11e6-9bfb-eaaf5a3324c3.jpeg)

Right now, I am continuing to work on this project everyday, and the next step will be the combine the three types of deceptions into one by fidning different weights for each deception. Then I will need to iterate through for a given pitcher year or even pitch to find different clusters of pitchers. This is new and exciting research that can provide so much information and could go in many different directions. It can also be helpful for determining both hitter and pitcher future success for Major League Teams to factor in when making decisions. 









