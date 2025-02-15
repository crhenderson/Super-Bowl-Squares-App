Super Bowl Squares
========================================================
author: crhenderson
date: 2015-09-26

[Github](https://github.com/crhenderson/Super-Bowl-Squares-App)


 
 




What are Super Bowl Squares?
========================================================
<small>*Super Bowl Squares* is a betting game related to the NFL Super Bowl.  Each square in the 10x10 grid represents a combination of the last digit of each teams score.  Before the game starts, players buy squares and then the numbers 0:9 are randomly placed accross the top and side of the grid, ie, players blindly select squares.  
Example: The Cowboys just kicked a field goal and the score is now Cowboys 24 - Patriots 17.  The last digits are 4 & 7 marked on the grid below.  The owner of that square wins $.</small>
<center> 
![plot of chunk example](Presentation-figure/example-1.png) 
</center>

Historic Data
========================================================
<small>One feature of this app is a breakdown of historical frequencies and empirical probabilities of different last digit combinations occuring.  That way you can see what your chances of winning are if you drew the 3 & 5 square for example.  The chart shows you how many games have had instances of 3 & 5.</small>
 

<center>

```r
kable(historic.pergame(3,5))
```



|Combo |N     | Games|Probability |
|:-----|:-----|-----:|:-----------|
|3 & 5 |0     |  6465|97.7%       |
|3 & 5 |1     |   147|2.2%        |
|3 & 5 |2     |     6|0.1%        |
|3 & 5 |Total |  6618|100%        |
</center>
Historic Data
========================================================
<small>The **Historic Data** tab also shows the probability that each number combo will occur at least once in a game.  3 & 5 is marked in red.</small>


<center> 

```r
p<-historic.table(3,5)
```

![plot of chunk display 2](Presentation-figure/display 2-1.png) 
</center>
Next Winning Square
========================================================
<small>The other feature of the app is to predict the next most likely winning square based on the quarter, total score, and score differential.  You can input the current teams and score.  Suppose the Cowboys are beating the Patriots 35-28 in the 3rd quarter and the next play is not going to be an extra point.  The **Next Winning Square** tab shows the probabilities for the next possible winning squares.</small>


<center>

```r
p<-probability.table(35, 28, 3, 1,"Cowboys","Patriots")
```

![plot of chunk prob output](Presentation-figure/prob output-1.png) 
</center>
