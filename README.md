# Baseball-Research

One of the most repeated phrases when it comes to hitting is "keep your eye on the ball". It is impossible to track the ball all the way from the pitcher's hand to home plate because before the ball can cross the plate the angular velocity is so great that the human eye cannot detect it. Because of this problem, hitters have to anticipate and predict where the location of the ball is going to be and time it correctly to make optimal contact. That is the idea behind this research project: to create models based on the information that a hitter observes to form predictions about the final pitch locations and times, and apply those models to determine and predict future success.

The raw data comes from a Pitch/fx database I created by scraping every pitch from 2008 to 2014. Once I scraped all the data, I ran several queries on MySQL to join and subset the Pitching and Batting tables to create a new data frame baseball_data. This data frame contains the pitcher and batter names, pitch_types, at bat result, date, and all the pitch information: final horizontal and vertical ball position, velocity, acceleration along with the spin rates and direction. I wouldn't be using all these variables in my model, but I kept in the data set in case I needed it.

So how do we quantify a pitcher's deception? Well hitters track the pitch coming out of the pitcher's hand, so we can infer some predictor variables based on what information hitters have. I can determine the pitch location or velcoity  during the flight of the ball by constructing functions on R to apply kinematic equations to the PITCH/fx data, then we can predict final pitch locations and times from the ball's position at any time or distance. The "deception" of each pitcher, each pitch is represented by the test Mean Square Error, which is the mean of the squared difference of the actual values and the predicted value.

I used smoothing splines to create the models since the focus of the research is on prediction accuracy, not interpretability. Smoothing Splines models consistentcy led to lower test error rates than other non parametric methods such as local regression or natural splines, and I could use generalized cross validation to find the optimal degrees of freedom for the smoothing factor.

In summary I wanted to look at three questions with this research:

**1. How predictive is a given pitch at a particular time or distance during the flight or distance from the plate, to the final location and time of the pitch?**

**2. How do the models of the pitch at different times and distances compare?**

**3. How do we utilize this information and what does this mean? How well do batter's read and predict each pitcher's pitch and how does their deception relate to other stats? (eg. SwgStr%, SoftHit%, HardHit%, LD%, ZContact%) **













