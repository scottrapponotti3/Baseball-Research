# Baseball-Research

One of the most repeated phrases when it comes to hitting is "keep your eye on the ball". It is impossible to track the ball all the way from the pitcher's hand to home plate because before the ball can cross the plate the angular velocity is so great that the human eye cannot detect it. Because of this problem, hitters have to anticipate and predict where the location of the ball is going to be and time it correctly to make optimal contact. That is the idea behind this research project: to create models based on the information that a hitter observes to form predictions about the final pitch locations and times to quantify the pitcher's deception, and identify the significance of that deception and how it can be used to find hitter's reaction times.

The raw data comes from a Pitch/fx database I created by scraping every pitch from 2008 to 2014. Once I scraped all the data, I ran several queries on MySQL to join and subset the Pitching and Batting tables to create a new data frame baseball_data. This data frame contains the pitcher and batter names, pitch_types, at bat result, date, and all the pitch information: final horizontal and vertical ball position, velocity, acceleration along with the spin rates and direction. I wouldn't be using all these variables in my model, but I kept in the data set in case I needed it.

So how do we quantify a pitcher's deception? Well hitters track the pitch coming out of the pitcher's hand, so we can infer some predictor variables based on what information hitters have. I can determine the pitch location or velcoity during the flight of the ball by constructing functions on R to apply kinematic equations to the PITCH/fx data, then we can predict final pitch locations and times from the ball's position at any time or distance. 

I used smoothing splines to create generalized additive models of each type of deception (x position, z position and time) for highest predictive accuracy, and applied generalized cross validation to find the optimal degrees of freedom for the smoothing factor. I quantified the deception at a given distance from the plate as the squared difference between the actual final position and remaining time and the predicted value. By iterating over many distances as the ball approaches the plate, each pitcher's deception can be visualized.

In summary I wanted to look at two questions with this research:

**1. What is pitcher's positional and time deception, and how are these deceptions related to each other and overall pitcher success, such as Runs and Pitch Values.**

**2. Iterate the deceptions over many distances, to identify different clusters of each pitcher's deception, and identifythe success each hitter has against each cluster to determine hitter reaction times, and how well each hitter performs against certian types of pitchers.**
















