## Confidence interval exercises


## Exercises 

For these exercises, we will use actual polls from the 2016 election. You can load the data from the __dslabs__ package.

```{r}
library(dslabs)
data("polls_us_election_2016")
```

Specifically, we will use all the national polls that ended within one week before the election.

```{r, message=FALSE, message=FALSE}
library(tidyverse)
polls <- polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.") 
```

1\. For the first poll, you can obtain the samples size and estimated Clinton percentage with:
  
  ```{r, eval=FALSE}
N <- polls$samplesize[1]
x_hat <- polls$rawpoll_clinton[1]/100
```

Assume there are only two candidates and construct a 95% confidence interval for the election night proportion $p$. 


2\. Now use `dplyr` to add a confidence interval as two columns, call them `lower` and `upper`, to the object `poll`. Then use `select` to show the `pollster`, `enddate`, `x_hat`,`lower`, `upper` variables. Hint: define temporary columns `x_hat` and `se_hat`. 

3\. The final tally for the popular vote was Clinton 48.2%	and Trump 46.1%. Add a column, call it `hit`, to the previous table stating if the confidence interval included the true proportion $p=0.482$ or not. 


4\. For the table you just created, what proportion of confidence intervals included $p$?
  
  
5\. If these confidence intervals are constructed correctly, and the theory holds up, what proportion should include $p$?
  
  
6\. A much smaller proportion of the polls than expected produce confidence intervals containing $p$. If you look closely at the table, you will see that most polls that fail to include $p$ are underestimating. The reason for this is undecided voters, individuals polled that do not yet know who they will vote for or do not want to say. 
Because, historically, undecideds divide evenly between the two main candidates on election day, it is more informative to estimate the spread or the difference between the proportion of two candidates $d$, which in this election was $0. 482 - 0.461 = 0.021$. 
Assume that there are only two parties and that $d = 2p - 1$, redefine `polls` as below and
re-do exercise 1, but for the difference.

```{r, message=FALSE, comment=FALSE}
polls <- polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.")  |>
  mutate(d_hat = rawpoll_clinton / 100 - rawpoll_trump / 100)
```

7\. Now repeat exercise 3, but for the difference.


8\. Now repeat exercise 4, but for the difference.


9\. Although the proportion of confidence intervals goes up substantially, it is still lower than 0.95. In the next chapter, we learn the reason for this. To motivate this, make a plot of the error, the difference between each poll's estimate and the actual $d=0.021$. Stratify by pollster.

10\. Redo the plot that you made for exercise 9, but only for pollsters that took five or more polls.


