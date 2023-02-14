# SuperBowl-2023 🏈

Hello everyone, this is a small project using R to forecast the viewership in millions for the next Super Bowl.   
As many people know, forecasting is beneficial to any business, if you can forecast the total audience for an event like the Super Bowl, then you can decide 
if it is worth to publish an ad. 📈📉

## Summary
Here are the point estimates for each model, we can see that the `simple linear` model give the most optimistic forecast.😃
While the `Linear Piecewise` the pessimistic forecast.😰  
The other models give a in between.
| Model                 | Prediction (millions) |
| -------------         | -------------         |
| Simple Linear         | 116                   |
| Linear Piecewise      | 90.3                  |
| Exponential Smoothing | 102                   |
| 3 Moving Averages     | 102                   |
| Actual Viewership     | 112.17                |

## Plots
![Linear](https://github.com/garcii06/SuperBowl-2023/blob/main/Plots/Linear%20Model.png)
![Piece](https://github.com/garcii06/SuperBowl-2023/blob/main/Plots/Linear%20Piecewise.png)
![Exponential](https://github.com/garcii06/SuperBowl-2023/blob/main/Plots/Exponential%20Smoothing.png)
![MA](https://github.com/garcii06/SuperBowl-2023/blob/main/Plots/Moving%20Average.png)

## Result
Although most of the point estimates *predictions* where less than the actual value, at least the 95% confidence interval captured the actual.  
This means that the forecast was good at capturing in the higher part of the interval and maybe a factor really affected the viewership like:
 - Having Rihanna in the half-time show.
 - Teams are popular.
 - Or that people noticed that Rihanna is pregnant.
 
 Any of these circumstances might be the cause that the viewership was higher.
