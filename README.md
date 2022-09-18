# COMP396 Final Report
Contributors: Tianyi Wang, Shengying Li, Zheyu Huang, Kechen Shi, Zhangyuan Xu

### Contents
Section 1: Final choice of submitted strategy
* Section 1.1 About the strategy
* Section 1.2 Collaboration of the different parts of the strategy
* Section 1.3 Optimising and checking the robustness of your strategy

Section 2: Justification of submitted strategy
* Section 2.1 The reason why we choose this particular strategy and the combination of these sub-strategies
* Section 2.2 Justification of the choice of position size and other key elements of the strategy
* Section 2.3 Comparison of the final strategy with alternatives
* Section 2.4 Risk management

Section 3: Evaluation and analysis of performance on part 3
* Section 3.1 Comparison of the result of the strategy in part 3 with the expected result
* Section 3.2 Mistakes (at the technical level mainly, but also in terms of planning and teamwork)
* Section 3.3 Learning from this module and Improvement

Section 4: Breakdown of team work
* Tianyi Wang
* Kechen Shi
* Shengying Li
* Zheyu Huang
* Zhangyuan Xu

Reference List

## Section 1: Final choice of submitted strategy

*The final strategy is a diversified strategy including the momentum strategy, the Donchian Channel mean reversion strategy and the 3-factors mean reversion strategy. In this section, the final strategy will be presented in detail.*

### Section 1.1 About the strategy

* **3-factors mean reversion strategy**  

1. **KDJ indicator**  
This indicator is not contained in the design part. However, in the plan part we suggested that we may need more indicators to restrict the decision-making. That is the main reason for this indicator.  
In a sentence, **this indicator is to help us to detect the lowest (or highest) point in a period.** The KDJ contains three elements, K, D and J line respectively. When J line is below 0.2 or is higher than 0.8 we determine that it is a trigger point (Details will be discussed in section 1.3) [1]. Compared with other indicators, KDJ is more accurate in finding the enter point and it has a lot of optional parameters which means we have several approaches to use this indicator [2]. The purpose of this indicator is to enhance the accuracy and eliminate the wrong decision. Therefore, this indicator interacts with the others, and only exists in the if condition.  

2. **CCI indicator**  
The Commodity Channel Index (CCI) is not an indicator that is often used in mean reversion strategies. But in this mean reversion strategy, we use this indicator for **three main reasons**.  
   1. The CCI indicator itself does not have a strong tendency, it can be used for both mean reversion ideas and trend following strategies. Therefore, the CCI indicator can be suitable for mean reversion strategies, as long as its parameters are modified.  
   2. The CCI indicator is mainly calculated from the difference between typical prices (the average of high, low and close prices) and moving averages, which can be viewed as a comprehensive measure of the average prices [3]. Since the core of a mean reversion strategy is to define the “averages” that prices would reverse to, we think it is sensible to use the CCI indicator in a mean reversion strategy. And during the process of making this strategy, the results are found surprisingly promising, so we carried it over.  
   3. The CCI indicator is not directly related to the price of the time series. So, when using the CCI indicator difference within two days for flexible position control, there is no need to worry about being affected by price fluctuations and making the position uncontrolled.   
   
3. **BBands indicator**  
**The Bollinger Bands (BBands) indicator is used to stop loss when prices of time series exceed the upper band or lower band.** After the position sizing is done by CCI indicator, the BBands indicator will determine whether a stop loss is required. If needed, BBands will fill a market order with the exact opposite amount of all the 3-factors mean reversion strategy's current position.  

We also considered the use of other indicators such as MACD, which is explained in detail in Section 2.3 Alternatives, but ultimately, we believe that the BBands can work well intuitively with our parameter optimisation approaches, especially the two visualisation methods. It is because every change in the parameters of the BBands indicator can be mapped into our visualisations. Another advantage of BBands indicator lies in the fact that as a stop loss indicator, it will have as little impact as possible on day-to-day transactions. As long as the look back of BBands is reduced to make the two bands of BBands more flexible, and the standard deviation is increased to make them wide enough, the impact of the BBands indicator on most normal markets can be reduced as much as possible.  

**Overall, the 3- factors mean reversion strategy consists of three indicators: CCI, KDJ and Bbands.** The first 2 indicators control the decision making while the last one is for stop loss. The purpose of this strategy is to trade in short term perspective with low positions and gain when there is a clear signal for entry. In this way, we could have a low risk but active trading strategy to complete the diversity.   
To be more specific, through the CCI function embedded in the TTR package, this strategy first calculates daily CCI values through daily close prices and CCI related parameters. when the CCI value is lower than the oversold line and the J line crosses the low line we set, we will perform a long operation, and flexibly control the position according to the difference between the CCI values between today and yesterday, and vice versa. Besides, this flexible position control takes the weighted average of prices across different time series into account. Regarding to stop loss, this strategy uses the BBands indicator: when the close price is greater than the upper or lower band of the BBands, a close position operation is performed.

* **Momentum strategy**  
**Our momentum trading strategy is a relatively long-term trend following strategy. The basic idea is that we apply a correlation test on the series,** and if the series passes the test, it means that in the past x days, the return of past y days are positively correlated with the next z days (x, y, z are all parameters set by us). **So if the series passes the test, the strategy will decide whether to long or short according to past y day’s return using limit order. And in the next z days, we will check whether we should hold the position or stop loss every day.**

* **Donchian Channel mean reversion strategy**  
The Donchian Channel part of the strategy is originally a trend-following strategy, following the momentum of the stock price tendency (i.e., if the strategy identifies the trend to be going upward, the strategy thinks that the stock will continue to rise, hence long the position, and vice versa) [4]. However, after testing the data with Donchian Channel strategy, we found that **the mean-reversion way of manipulating the strategy will give a lot more return and PD (5.59 PD in part 1 and 5.3 in part 2) than trend-following.** In terms of the execution of the strategy, it firstly finds the highest and lowest closing price of the series in the last 20 days to be the **upper and lower bound of the Donchian Channel**, and calculates the simple moving average price of the last 3 days. If the SMA line is bigger than the upper line of Donchian Channel, then short the position; if the SMA line is smaller than lower bound, long the position.

### Section 1.2 Collaboration of the different parts of the strategy

Firstly, this strategy can be divided into two components, one of which is **long-term strategies (the momentum strategy and the Donchian Channel mean reversion strategy)** and the other is **short-term strategy (the 3-factors mean reversion strategy)**. Then all series will be run by the strategy, except for series 3 (not be run in the momentum strategy).  
The long-term strategies and the short-term strategy will work simultaneously and will not affect each other. In terms of the long-term strategies, the Donchian Channel mean reversion strategy and the momentum strategy are exclusive to each other.

When the long-term strategies are applied to one series, only the Donchian Channel mean reversion strategy runs until the test length of the momentum strategy is satisfied. To test whether it applies to a trend following strategy or a mean reversion strategy, every series will be conducted by the correlation test. The result will decide which strategy will be operated. So, we can **get a position of the long-term strategy**. Meanwhile, we will **get the other position of the short-term strategy**.

Finally, we **combine these two positions together and get a whole position for this series. Then repeat the above steps for the other series**.  
<div align="center">
   
   Figure 1  
![Figure 1](/pic/Figure%201.png "Figure 1")
   
</div>
This graph shows that when the length of trading days is bigger than the lookback of the Donchian Channel mean reversion strategy and smaller than 225(the test length of momentum strategy), the Donchian Channel mean reversion strategy will be executed.

If the length of trading days is bigger than 225, then we will conduct a correlation test every 45 days to decide whether the momentum strategy will hold a position. If the momentum strategy makes a trade decision (correlation coefficient is bigger than 0.2), then in the next 44 days, we will test whether the stop loss is triggered every day. If the momentum strategy does not make a trade decision, in the next 44 days, we will run Donchian Channel mean recersion strategy.  

Simultaneously, as long as the length of trading days is bigger than the lookback of the 3-factors mean reversion strategy, we will run the 3-factors mean reversion strategy.  


### Section 1.3 Optimising and checking the robustness of your strategy

Optimize:  
We have four main approaches to optimise strategies:   
1. **Visualising close price plots**  
Using this method, we are able to map the data from time series into the format of close price plots so that we can have a clearer view of how prices change over time. Since this method is prone to overfitting, we have always confirmed during the parameter optimisation process that the close price plots cannot be used as the sole reason for the adjustment. Our final submission will include the code for this function.   
For example, we introduced some new real-market datasets when testing robustness (more about this later). In one of the data sets, we found that our strategy can bankrupt us at a very fast rate during certain market conditions. By visualising the close price plot as below, we found that these markets are extremely unfavourable for mean reversion strategies because many markets show clear long-term trends like Figure 2. So we need to refine our stop-loss approaches for our mean reversion strategies to avoid bad performances during these market conditions.  

<div align="center">
   
Figure 2  
![Figure 2](/pic/Figure%202.png "Figure 2")
   
</div>

2. **Visualising current positions**  
This method can record the current position changes of all 10 time series when trading strategies are used and plot it into 10 different graphs. With the help of this visualisation, we can double check whether the strategy is running following our expectations. Our final submission will include the code for this function.  
<div align="center">

   Figure 3  
![Figure 3](/pic/Figure%203.png "Figure 3")  

*Above: the close price chart of one series;*   
*Below: the change of current position size of this series*  
   
</div>

3. **In-sample and out-of-sample test**  
With 2000 days of data, we divide it into three parts, the first part is the in-sample data (1000 days), and the second and third parts are out-of-sample data (500 days respectively). The difference is that we only move to the third part to test when the second part also performs well, as the flow chart shows below. We believe this method can help us avoid the problem of over fitting better than just dividing the data into two parts: in-sample and out-of-sample.  

<div align="center">

   Figure 4  
![Figure 4](/pic/Figure%204.png "Figure 4")
   
</div>

4. **Printing key variables within the function**  
In many cases, a good use of the print statement can help us to identify problems in time.  
For example, we believe that a higher correlation coefficient means a more accurate prediction, so at the beginning we used 0.8 as a criterion. However, by printing the variables, we found that there was only a small chance that the correlation coefficient would be close to 0.8 for all the time series. Furthermore, after trying other optimisation methods mentioned earlier, we concluded that as long as the correlation coefficient was positive, we were confident that the momentum strategy had the potential to be profitable with the help of other judgement conditions. Since the correlation coefficient is greater than 0.2 in most cases, we choose 0.2 as the final value.  

**Robustness:**   
To test the robustness of the final strategy, we use the new data sets such as stock prices in NYSEto help us find more shortcomings of the strategy. One thing should be mentioned is that this method is only used to check the robustness but not to optimize the parameters.  
For example, we got a data set with extreme case (The stock price shows clear trends over a period of 1000 days). (see Figure 2)  
And in this dataset, mean reversion strategies performed badly because the main idea of this strategy is not applicable to this stock at all. So we need to reconsider the position of mean reversion strategies and optimize it in order to minimize the loss and impact when mean reversion strategies are running in an extreme case.  



## Section 2: Justification of submitted strategy

*In section 1 we describe in detail the rationale and operation of the individual sub-strategies, as well as the merging of the final strategy. This is followed by a detailed analysis of the strategy in Section 2, including the reasons for the strategy merge, the choice of key elements, the choice of strategy and risk management.*

### Section 2.1 The reason why we choose this particular strategy and the combination of these sub-strategies
1. **Choosing this particular strategy**  
We believe that well-diversified strategy holds more chance to maintain profits and has higher ability to resist the risk. Therefore, our final strategy contains 3 types of methodology to ensure the diversity:  
   1. **3-factors Mean reversion for short-term**  
   Capture profit in fluctuation, however bear a loss when there is a clear trend  

   2. **Momentum for relatively long-term**   
   Get benefit when there is a clear trend (like part 3 data)
   
   3. **Donchian channel for speculation (or one day holding)**   
   Speculation holds a chance to make a huge profit but has higher risk.

Speaking of the structure of final strategy, the Figure 5 illustrates the relationship between each component with respective to time series, like mentioned in [Section 1](#Section-1:-Final-choice-of-submitted-strategy). The isolation of long-term and short-term hedges the risk to a certain extent.  

<div align="center">

Figure 5  
![Figure 5](/pic/Figure%205.png "Figure 5")

</div>

To explain the arrow in the image, the solid arrow indicates this strategy will be run in this period, in opposite, the dotted lines show that there is a choice and may execute the strategy. (The code structure see Figure 1) 

2. **how to combine with other (and series)**  
   1. **short-term combination (the 3-factors Mean reversion strategy)**  
   KDJ indicator was used in the trend following strategy, however during implementation period there is a significant side effect of **delaying in the trend following**.   
To be more specific, trend following strategy will enter the market after the KDJ alarmed, so there will be a gap between two points.  
In contrast, theoretically deploying the KDJ in mean-reversion strategy perfectly prevent this side effect even enhance the accuracy. It can be seen from figure that KDJ can prevent a loss and assist mean reversion to reach the accurate point. The decisions are made only when both requirements meet. So, apparently KDJ is more suitable for mean reversion.  
   <div align="center">

   Figure 6  
   ![Figure 6](/pic/Figure%206.png "Figure 6")

   </div>

   2. **combination of momentum strategy and Donchian channel mean reversion strategy**  
   The correlation coefficient is a core element for these two strategies. Due to the high risk of the speculation, we have to make sure that it makes accurate decisions as much as we can. In this situation, correlation coefficient can warn that following period may have a trend when variable `cor` is greater than 0.2. This prevents the Donchian channel as actually a mean reversion strategy from suffering a huge loss. Besides, when `cor` is smaller than 0.2, momentum holds no position, and Donchian channel, as a speculation strategy, is more likely to make a profit in this case.  

   3. **series choice**  
   The momentum strategy excludes series 3. Because we observed that the momentum strategy performs badly in series 3 with both part 1 and part 2 data. At first we think this can be attributed to the extremely low price of series 3, as the average price of series 3 is lowest among all series, ranging from 0.6 to 0.7. Cause when we printed the price in Rstudio, the figure is account to two decimal places and the price will change by at least 0.01. That is about 1.5% of the price of series 3. As a result, the price change of series 3 is a little more inflexible. However, after we submitted the code, we found that the price of series 3 in primitive data is account to four decimal places. So we would say that the decision to exclude series 3 may be unreasonable.


### Section 2.2 Justification of the choice of position size and other key elements of the strategy

1. **Position sizing**   
   1. A very simple method is applied to control the position sizing of the momentum strategy. **Every time a trading decision is made, we will long or short assets worth 100K, accounting for 10% of the total value of the equity we hold at the beginning.** As for why we assign the same amount of money to every trade, we believe that as long as the series passes the correlation test, it indicates that the series will follow the trend in the following days. And the value of the correlation coefficient is irrelevant to the continuity of trend. Additionally, although 100K seems to be large and risky, it turns out to be reasonable. After testing part 1 data and part 2 data, it is found that at most 3 series will trade at the same time. Chances are very low that the momentum strategy will face the risk of bankrupt.
   2. For the position sizing of the Donchian Channel mean reversion strategy and the 3-factors mean reversion strategy, considers **every series equal and give the same weight to their trading**. According to the position sizing code, on the numerator it considers the relative size of Donchian Channel. `maxCl/Cl` will give a fairly equal weighting to each series on the position sizing, eg: if the maximum close price of the 10 series is 1000, series 1 is 10, series 2 is 20, the allocation for the series that has 1000 close price will be 1000/1000=1, series 1 = 1000/10 =100, series 2 = 1000/ 20 = 50. This is to allocate equal weighting for each series. Plus, the position sizing also considers the distance between the moving average and Donchian Channel price. If there is bigger gap between these two prices, it means the arbitrage space is bigger, so we will have a larger position on that trade. As for the 3-factors mean reversion strategy, we apply a similar method of position sizing but the result can’t reach our expectation. This mistake will be discussed in the Section 3.2. 

2. **SMA3**  
Another key element in the Donchian Channel strategy is the moving average. We use Simple Moving Average for the last 3 days to indicate whether or not the strategy triggers the trading point. The 3-day moving average represents a more accurate price for the most up-to-date trend, because the single day closing price can be an outlier, whereas the moving average gives a true indication of stock price that excludes any outlier. We believe the parameter 3 is the most suitable since we have tried 2, 4 and other parameters, but 2 showed a very volatile result and 4 or bigger could hardly satisfy trading condition.

3. **Correlation coefficient**  
Firstly, the idea of using correlation coefficient as a measurement of time series momentum comes from ***Algorithmic Trading : Winning Strategies and Their Rationale*** [5]. The example provided in the book uses the whole data set to calculate the correlations between returns of different time frames for the purpose of understanding and instead of using R, it uses MATLAB. In R, we cannot get the correlation coefficient of return of different time frames directly using `cor()` function as the length of the time series are different. So we think of a correlation test to get the correlation coefficient.  
Secondly, we will explain how specifically the correlation test works. Let **x** =225, **y** =90, **z** =45 (these have been introduced in Section1.1) for example. Within 225 days, from the first day to 90th day, we get a lookback return = close price of 90th day – close price of the first day. From 91th day to 135th day, we get a corresponding hold return = close price of 135th day – close price of 91th day. And if we put off all the dates mentioned above by one day, we get another pair of lookback return and hold return. In total, we can get 90 pairs of lookback return and hold return. After that, we calculate the correlation coefficient between the lookback return and hold return using `cor()` function. If the correlation coefficient is positive, it means that choose any 90 days in the specific 225 days, if the return of holding the series for the 90 days is positive, then the return of holding the series for the next 45 days will also be positive, vice versa.

<div align="center">

The code of the correlation test is as followed:  
`testlength` = **x** =225  
`lookback` = **y** =90  
`holddays` = **z** =45  

Figure 7  
![Figure 7](/pic/Figure%207.png "Figure 7")

</div>
   
So the strategy will do a correlation test every **x** days, and long or short or do nothing according to the result.  
Lastly, we will show how we get the specific value of **x**, **y**, **z**. We picked the first 500 days of part 1 data and calculated the correlation coefficient of the returns of **y** days and the following **z** days in this period. We tried several combinations of **y** and **z** and found that (90, 45) and (90, 30) give satisfactory results. After getting part 2 data, we tried the two combination on the whole part 1 data and other data we randomly selected from NYSE and found that (90, 45) gives a better correlation coefficient. Finally, with **y** =90, **z** =45, we tried several options of value of **x** and got the value of **x** that gave the best pd ratio in part 1 data.

4. **EMA90**  
EMA90 is treated as the indicator of the stop loss of the Momentum strategy. The reason why we use EMA is that, compared with SMA, EMA give recent data more weight and can make the stop loss more sensitive to recent change in price. And we choose EMA90 after testing. We tried EMA30, 45, 60, 90, 120 respectively, and the corresponding aggregate pd ratios are -1135.51, -294.89, 0.31, 1.81, 0.8. 90 is the best choice if we run the strategy on Part1 data.

5. **Store**  
Store is the key element in strategies. We add the positions of our sub-strategies into store for the purpose of clearing the positions when needed. When we initialize the store, we initialize the parameters `momentumPos`, `dcPos` and `cciPos` used to record the positions of every sub-strategies. And every time the positions of sub-strategies change, we will call the `updateMomentumPos`, `updateDcPos` and `updateCciPos` functions to update the positions. So, we can make sure that, for instance, if the stop loss of the momentum strategy is triggered, only the position of momentum strategy will be cleared and the positions of the other two strategies will not be affected.  
The graph below shows the store related code (The code is added at the end of the team1.R file which is below the `getOrder()` function):  

<div align="center">

Figure 8  
![Figure 8](/pic/Figure%208.png "Figure 8")

</div>

   
### Section 2.3 Comparison of the final strategy with alternatives

1. In the **3-factors mean reversion strategy**, moving average convergence divergence (MACD) was viewed as an alternative stop loss indicator of Bollinger Bands (BBands). Although the MACD indicator is mainly used to instruct buy and sell operations, it can also provide relatively reliable medium-term trend judgments, because it takes into account the convergence and divergence of long-term and short-term moving average.  
**For instance**, when the fast moving average of the MACD indicator exceeds the slow moving average, it usually represents a long position. Nevertheless, if the CCI indicator implies a short operation at this time, it means that the medium-term trend does not support CCI to continue shorting. In this case, as the 3-factors mean reversion strategy in nature requires to follow the medium-term trend to take the advantage of the price fluctuation to make a profit, we should not continue to add to a short position when the two indicators against each other. To prevent the possibility of larger losses, losses for now are taken and positions are liquidated.   
However, when the MACD indicator is used in the 3-factors mean reversion strategy, **we find it difficult to optimise**. This is because even if the parameters are controllable, the adjustments may still hard to be reflected in the real situation. For example, with the help of the optimisation methods, we can get the information of when the MACD indicator triggers, and whether or not it is triggered at the right time. Nonetheless, given the complexity of how MACD works, our parameter adjustments may cause the MACD indicator to perform better in some quotes but worse in others. When the adjustment can only be performed by guess rather than by theory, we believe that the optimisation cannot be reliable. Therefore, BBands is selected for its ease of visualisation and ability to intuitively adjust parameters to reflect plots changes.   

2. Typically, the **Donchian Channel strategy** is considered trend following strategy. To be more specific, if a stock’s close price creates a new high for the last 20 trading days, long the position, and vice versa. However, after examining the trend following strategy for the part 1 of the data, we feel unsatisfied with the result (the PD has all been negative). We have tested many other alternatives of the strategy while comparing the results using the visualization. One of our findings show that the stock price in part 1 will mostly fall after the close price reach a new high for the last 20 trading days, which makes the data more suitable for mean-reversion trading. Thus, we reverse the trend-following strategy into mean-reversion. From a holding period point of view, trend-following strategy requires a period of holding after long or short execution, but it is not certain how long the holding should last. In the Donchian Channel strategy, we hold the position just for one day and clear the position on the next day, so that we can avoid the uncertainty of holding period. Besides, an advantage of the mean-reversion strategy is that in the momentum strategy, the correlation test helps to confirm whether the combined strategy is not suitable for trend-following, where mean-reversion strategy can take over.  


### Section 2.4 Risk management

In the **3-factors mean reversion strategy**, we have a stop loss method (BBands) and a position sizing to manage risks.   
The BBands indicator is mainly used to prevent the potential problem of over-positioning. Although the position of this strategy is small in most cases, in some extreme cases, the difference between two days of CCI can be large (the position of this strategy is flexibly controlled by the difference within two days as explained in [section 1](#Section-1:-Final-choice-of-submitted-strategy)), therefore, the positions of the strategies are also highly volatile. Under this circumstance, we mainly consider two aspects:  
1. Even if a stop loss is needed, CCI and KDJ indicators should be given enough trust to **prevent the stop loss from being triggered too frequently**. This is because as a mean reversion strategy, it profits from price fluctuations. For example, the 3-factors mean reversion strategy keeps buying long position when prices are falling. By liquidating our position prematurely when the price is still likely to rise, we lose both the principal and the possibility of profit. Therefore, we want the stop loss to be triggered only after we have lost as much as we can afford, by adjusting the standard deviation of the Bollinger Bands indicator wide, which is 5.  
2. **Relatively long-term trends should be considered.** We believe finding the “mean” is the main point of a strategy centred on mean reversion idea. In order for mean reversion strategies to be profitable, the price volatility needs to be based on the trend of the current mean. This requires the indicator to provide a grasp of the relatively long-term trend, but not too long to prevent the indicator from being too sluggish and giving false signals. Therefore, the lookback of BBands was set to 50 days.  

**Position sizing** is another way of risk management for the 3-factors mean reversion strategy. As a strategy that is traded almost daily, setting the position too high will increase the risk and affect the performance of other strategies when they need funds. In this strategy, position sizing is controlled by multiplying the difference in CCI values over two days by a factor, so it can be limited within a small range.  

In **the momentum strategy**, we control the risk through stop loss.   
The stop loss plays a significant role in momentum strategy for the drawback of momentum strategy is obvious. If the correlation test fails to predicate future trend, for example, the series is expected to follow an uptrend, but it actually drops significantly in the next 45 days, then we are going to suffer from great loss. So, we must stop loss. **Our momentum strategy selects EMA90 as an indicator for stop loss.** If we are holding a position, and the close price of the current day is lower than the EMA90 of the close price of the day we long, then we will exit the position, vice versa. However, there are special cases exist where the series passes the correlation test but the trend stops a few days ago and the close price goes in the opposite direction. Stop loss will be triggered immediately after the momentum strategy make a trading decision and we will definitely loss money. Our solution is to exclude the special cases when make trade decision, because we think the special cases may indicate that the following days will not go in the way we expect (see Figure 9).  

<div align="center">

Figure 9  
![Figure 9](/pic/Figure%209.png "Figure 9")

</div>

In the **Donchian Channel mean reversion strategy**, the risk is managed by setting an upper hard limit of 200,000 pounds for each trade. This is because during our test, we have found that the single trade of Donchian Channel can reach 400,000 pounds, which may cause insufficient found to execute. Setting such hard limit can avoid the problem of insufficient fund and bankruptcy.

## Section 3: Evaluation and analysis of performance on part 3

_In this section, we will provide an in-depth analysis of performance on Part 3. And we will present a critical evaluation and analysis: We believe that the performance of the final strategy is acceptable on part 3, but more important are the reasons that led to the less than satisfactory results. Besides, our mistakes in the technical level and preparation phase will be reported. We have received a huge boost through this module, both in terms of learning and in terms of human interaction._

### Section 3.1 Comparison of the result of the strategy in part 3 with the expected result

In part 1 and part 2, the strategy performs well.  

<div align="center">

Figure 10 - The result of Part 1  
![Figure 10](/pic/Figure%2010.png "Figure 10 - The result of Part 1")  
Figure 11 - The result of Part 2  
![Figure 11](/pic/Figure%2011.png "Figure 11 - The result of Part 2")

According to the above results, we expected this strategy to perform well in part 3 too.  
Figure 12 - The result of Part 3  
![Figure 12](/pic/Figure%2012.png "Figure 12 - The result of Part 3")

</div>

From the result of part 3 data, we end up with a **pd of 1.66** and a **profit over 50K**. We think that the reasons we can get a relatively acceptable result are the diversity of our strategy and a reasonable risk management.   
To clarify the reason why the PD is less than 3, we visualised the data in part 3 as close prices plots. We find that **the time series prices in part 3 as a whole will show clear upward or downward trends over a long period of time, whereas in part 1 and part 2, the price movements are more diverse**. The Figure 13, the Figure 14 and the Figure 15 use the close price movements of series 1 in different parts as an example to illustrate the above point.  

<div align="center">

Figure 13 - The close price plot of series 1 in part 1  
![Figure 13](/pic/Figure%2013.png "Figure 13 - The close price plot of series 1 in part 1")  
   
Figure 14 - The close price plot of series 1 in part 2  
![Figure 14](/pic/Figure%2014.png "Figure 14 - The close price plot of series 1 in part 2")

Figure 15 - The close price plot of series 1 in part 3  
![Figure 15](/pic/Figure%2015.png "Figure 15 - The close price plot of series 1 in part 3")

</div>

Since mean reversion strategies are not suitable for series with clear long-term trends, while the opposite is true for trend following strategies. As a result, the momentum strategy performed well in part 3, while the Donchian Channel mean reversion strategy and the 3-factors mean reversion strategy performed poorly.   

To be specific, 
1. As for **the momentum strategy**, the performance of it is better than we expected. Both the frequency of trades and the percent of winning trades in part 3 data are higher than those in part 1 and 2 data. This can be attributed to the fact that many series in part 3 show clear long-term trends which are signal for trading. Additionally, we notice that if we exclude the stop loss from the momentum strategy, the profit and the pd ratio can be even higher than what they are currently. This can also indicate that the part 3 data is very favorable for the momentum strategy. So we believe that our momentum strategy is a relatively reliable strategy and we did not overfit it in part 1 and part 2.
    
   <div align="center">
   
   Figure 16 - the result of the momentum strategy in part 3  
   ![Figure 16](/pic/Figure%2016.png "Figure 16 - the result of the momentum strategy in part 3")
   
   </div>

2. As for **the 3-factors mean reversion strategy**, in the Part 1 and Part 2 data, there was no clear tendency for the time series, so it had a good performance. However, the data in Part 3 were not as friendly to mean reversion strategies, so it was expected that the strategy itself would not be profitable in such a circumstance. Looking at the results of the individual 3-factors mean reversion strategy, the loss was £16,000, which is a little high due to the incorrect weighted average of our positions (more on this in Section 3.2). After correcting the error, the strategy was able to make a loss of only 168 pounds, which is satisfactory.   
As expected, the 3-factors mean reversion strategy achieved 98% active days, proving that the strategy does indeed trade nearly every day, as we had designed. Looking at the trend of the results, we can see that the strategy can control losses at very low levels in even unsuitable quotes, so we can confidently say that no overfitting has occurred.

   <div align="center">
   
   Figure 17 - the result of the 3-factors mean reversion strategy in part 3  
   ![Figure 17](/pic/Figure%2017.png "Figure 17 - the result of the 3-factors mean reversion strategy in part 3")
   
   </div>

3. As for **the Donchian Channel mean-reversion strategy**, the performance is not as good as we expect. Compared to 5.59 PD in part 1 and 5.3 in part 2, part 3 made a loss, which we believe the reason is that part 3 data is not very suitable for mean reversion. In the combination with momentum strategy, the momentum strategy had managed to stop some of the loss of Donchian Channel mean-reversion strategy and gain a profit. It is therefore very important to consider some extra circumstance that we may not see in part 1 and 2, so we can add risk management in the strategy. Since the parameter setting of the strategy is quite general, we believe the loss has nothing to do with overfitting, but the nature of the data.

   <div align="center">
   
   Figure 18 - the result of the Donchian Channel mean reversion strategy in part 3  
   ![Figure 18](/pic/Figure%2018.png "Figure 18 - the result of the Donchian Channel mean reversion strategy in part 3")
   
   </div>

Although the losses from the Donchian Channel mean reversion strategy have been reduced due to the restriction of correlation coefficient, the profit from the momentum strategy has not been able to offset the losses from the 3-factors mean reversion strategy.   
At the aim of diversification and risk management, we pose a limit on the position of every strategy so that the performance of the final strategy will not be dominated by one single strategy. Consequently, although the momentum strategy performs well in part 3, the aggregated PD is smaller than 3.


### Section 3.2 Mistakes (at the technical level mainly, but also in terms of planning and teamwork)

1. After reviewing and checking the codes, we found that the position sizing in the 3-factors mean reversion strategy is unable to achieve the weighted average and this cause the series with small prices can dominate the others.  

   <div align="center">
   
   Figure 19 – the wrong code of the position sizing in the 3-factors mean reversion strategy  
   ![Figure 19](/pic/Figure%2019.png "Figure 19 – the wrong code of the position sizing in the 3-factors mean reversion strategy")
   
   </div>

After discussion, we agreed that the correct position sizing is: `cciPos = (abs(round(cci-cciYesterday)))/cl`, which means **there is no need to multiple the max close price and divided by the current close price.** The reason for that is `maxcl` is the largest price of 10 series, `maxcl/cl` can and the position sizing of small price over times. For example, if the largest close price (`maxcl`) is 100, current close price is 1, extent of deviation (cci-cci yesterday) is 1, the money we spent for price is 1 is 100, but for price is 100 is 1. This math issue makes the unusual large position in series 3, therefore we lost almost 10k in part3 data in terms of the 3-factors mean reversion strategy. In Donchian Channel strategy, the position sizing had achieved its goal. This is because the position sizing of the Donchian Channel strategy is based on the individual stock price so that it could equally assign fund to each series, but it is not required in the 3-factors mean reversion strategy.

   <div align="center">
   
   Figure 20 – the correct code of the position sizing in the 3-factors mean reversion strategy  
   ![Figure 20](/pic/Figure%2020.png "Figure 20 – the correct code of the position sizing in the 3-factors mean reversion strategy")
   
   Figure 21 - the result of the 3-factors mean reversion strategy after adjusting position sizing  
   ![Figure 21](/pic/Figure%2021.png "Figure 21 - the result of the 3-factors mean reversion strategy after adjusting position sizing")
   
   Figure 22 - the result of the final strategy after adjusting position sizing  
   ![Figure 22](/pic/Figure%2022.png "Figure 22 - the result of the final strategy after adjusting position sizing")
   
   </div>

2. In the Design part, we come to realize that **the data analysis has weak coherence with the strategy design**. To be more specific, we initially want to have ACF test to find how the data series are correlated with each other, but we fail to build connection with the finding in the test with the strategy design. Moreover, there is similar problem with the volatility test, where we calculate the volatility by representing the daily return change in average percentage. More insight should be explained beyond the test. Although we know that more modifications should be made to the strategic analysis of the first assignment, the specific data analysis is still a serious difficulty for us.  

3. Looking back on our design stage, we **pay too much attention to our sub-strategies and coding implementation, and ignore the quality of report**. This results in inconsistency in our report. For instance, there is no connection between the analysis of the data and the strategy design. And the in-sample optimization and out-of-sample tests are not explained in detail. This can be attributed to lack of communication and bad planning. Because at the design stage, we are supposed to focus on the general idea of our strategy and the general approaches, rather than how specifically we realize our sub strategies.


### Section 3.3 Learning from this module and Improvement

In this Automated Trading Project, the first thing we will keep in mind is the coherence of the task. Each member should not only focus on their own tasks, but also go through others’ materials. One of our mistakes is to make tasks so separated so that there is almost no coherence between data analysis and the strategy in assignment1.   
Secondly, the idea that modify the parameter by observing the position, not merely paying attention to the result, really helps us to avoid serious losses and figure out the problems timely. The beautiful results always lure us to raise the position size, instead of think deeply. Blindly chasing the large position size may lead us to a huge loss in the unexpected market.   
Finally, it is vital to contract the supervisor in time and thankfully we received the feedback immediately every time. We launch the online meeting with supervisor and have an on-campus meeting with members every week. Regular meeting and studying the guidance from supervisor enhance the efficiency of problem solving and also clarify the purpose.    
What we have learnt is more than we have mentioned. The standardized procedure, self-study skills, communication skills and leadership, all those potential benefits we may not realize until we step into the workplace. Last, we want to thank everyone that makes effort on this program.  


## Section 4: Breakdown of team work
### Tianyi Wang
Our strategy was found to have insufficient diversity in assignment 1. To solve this problem, I took on the responsibility of formulating a new strategy in assignment 2, because my strategy was custom and had more flexibility. For coding part, I implemented the 3-factors mean reversion strategy with Kechen. Apart from that, I completed a small program to visualize the current position by myself, for parameter optimization and final strategy testing. Also, I contributed some valuable ideas and helped some team members to complete several debugs in meetings. More details can be found in the [Work Logs](/Tianyi/strategy%20documents%20for%20s2/Work%20Logs.md) file in the folder named under my name on GitHub.  
In assignment 3, I sorted out the report structure in this assignment based on the final report template at the meeting with the team members. In this report, I contribute over 2000 effective word counts and some graphics, such as flow charts and visualization results. Besides, I also prepare for the submission of my visualisation file, including comments and readme contents.  
In meetings, I tend to lead the process as an organizer so that let everyone always understands what to expect from the meeting and what to do after the meeting. Therefore, our team members find meetings to be very efficient and are very happy to solve various problems together. In general, I believe I contribute a lot in terms of teamwork, negotiation, assignment of tasks, etc.  

### Kechen Shi

After the first part of the project, we found that we needed more indicators in our final strategy, so I was responsible for searching the relevant indicators and matching them with the existing ones like CCI and Donchain channel. After a series of attempts I found KDJ is a reasonable indicator, and the team members decide to combine it with the CCI and BBand indicator as the 3-factors mean reversion strategy. After completing my part of the work, the team found that it is necessary to design a visualization to help optimize the parameters, so I assist Wang to code the optimization part. In the last part of the assignment 2, all team member worked together to optimize the parameters by comparing the visualization result and the close price plot, everybody is in there so the credit is for everybody.  
The tasks in the assignment 3 are relatively fixed, I am mainly responsible for explaining the part I made in assignment 2, the KDJ indicator, partial work of visualization and optimization. Furthermore, we found that there are some bugs remained in the 3-factors mean reversion strategy. After debugging the code, I write the Mistake in the evaluation part which explain what the bug is and how we fix it. Last, I conclude the things we learnt from this project and how things will be different if we do this project again.  

### Shengying Li

In the Automated Trading Project, I am primarily responsible for the documentation and report of the entire project and organise offline meetings. In the assignment 3, I provide the structure and framework for the entire report which helps us to write it in a better and clearer way. When the team members hand over their tasks to me according to the allocation of tasks, I am responsible for organising all the documents by sorting out the ideas, which help supervisors to better understand our project. Moreover, I am responsible for the editorial content of the text on the final strategy, reporting clearly on our final strategy in graphic form. An example of this is my contribution to Section 1.2. With the combined efforts and discussions of the group, we finally complete the report.  
As well as contributing to the documentation, I also made contribution to the team in assignment 2. Firstly, I assist Zheyu in designing the correlation test to get the correlation coefficient. Secondly, I point out that the stop loss of the momentum strategy did not considered a special situation which may lead to unreasonable clear position operation and work with Zheyu to improve the strategy. Lastly, I also contributed to the implement of limit order. When we try to use the limit order, we found that it may cause some problems, for example how to detect whether the limit order fail or not and what is the best choice when we set the limit price. I solve the problems and finally submit a good example of limit order.  

### Zheyu Huang

In this project, I am responsible for the momentum strategy and make a great contribution to the combination of momentum strategy and Dochian Channel mean reversion strategy and the code of implement stop loss.   
In order to complete my momentum strategy, I carefully read many suggested reading, especially **_Algorithmic trading: winning strategies and their rationale_** by Ernest Chan, which gave me a deeper understanding of automated trading system. Apart from that, in order to implement the momentum strategy using R, I searched for many materials online and finally, with the assistance from Shengying, I thought of a correlation test to catch the momentum which is proven to be reliable in given data set. For the purpose of risk management, I thought of a stop loss which takes many special cases into account so that the momentum strategy is more solid. To conclude, I contribute a good momentum strategy.  
I also helped other team member to solve many problems during the project. When I considered the stop loss of the momentum strategy, I came up with the idea of recording the current position using store, which helped other team members a lot when they faced the problem of getting the position of past trades. Additionally, when we found that Dochian channel mean reversion strategy had a unreasonable position because of the difference in prices of series, I suggested that we can add a coefficient `maxCl/Cl` to average the position. Lastly, when we tried to combine the momentum strategy and Dochian channel strategy, many bugs arised and I succeeded in solving them all.  
As to the final report, I am responsible for momentum strategy related content and check the readability and gramma.  

### Zhangyuan Xu

Throughout the project, I am mainly responsible for the Donchian Channel strategy design, as well as some of the visualization and data analysis of the series.
To be more specific, in assignment 1, I was responsible for the data analysis, where I draw the candlestick plot to help see the trend of the stock price clearly, as well as the volatility test and ACF test to see which series are suitable for the strategies we may use in assignment 2. I had saved candlestick plot in the Github.  
In assignment 2, I designed the Donchian Channel part of the strategy, where I took responsibility of optimizing parameters and helping to combine Donchian Channel stratey with others. In the combination process, I am able to gain insight from the in-sample result and reflect on Donchian Channel strategy optimization, so I come up with the risk management of the strategy.  
In assignment 3, I explained what does the Donchian Channel strategy do and all other things related to that, such as optimization, key element of the strategy, risk management. Additionally, I also attach visualization of the close price line chart code that we use for this report.   

## Reference List
[1]	‘KDJ | Technical Indicators | Stock Charts’, AnyChart Documentation. https://docs.anychart.com/Stock_Charts/Technical_Indicators/KDJ (accessed May 02, 2022).  
[2]	‘The combination of MACD+CCI+KDJ indicators’: https://www.liaochihuo.com/info/621612.html (accessed May 02, 2022).  
[3]	‘Commodity Channel Index (CCI) Definition and Uses’, Investopedia. https://www.investopedia.com/terms/c/commoditychannelindex.asp (accessed Apr. 30, 2022).  
[4]	‘Donchian Channels Definition’, Investopedia. https://www.investopedia.com/terms/d/donchianchannels.asp (accessed May 02, 2022).  
[5]	E. Chan and E. P. Chan, Algorithmic Trading: Winning Strategies and Their Rationale. Somerset, UNITED STATES: John Wiley & Sons, Incorporated, 2013. Accessed: May 02, 2022. [Online]. Available: http://ebookcentral.proquest.com/lib/liverpool/detail.action?docID=1204071  

