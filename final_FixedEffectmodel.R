# Loading required libraries
library(RODBC)
library(dplyr)
library(ggplot2)
library(AnomalyDetection)
library(dplyr)
dbHandle <- odbcDriverConnect('driver={SQL Server};server=localhost;database=CSOM_MSBA_ULDATA;trusted_connection=true')

walmart_3 <- sqlQuery(dbHandle, paste("SELECT [WeekEnding],
                                      [Attr2],
                                      SUM([Dollars]) AS Dollars,
                                      SUM([Dollars_Feature]) AS Dollars_feature,
                                      SUM([Dollars_Display]) AS Dollars_Display,
                                      SUM([Dollars_TPR]) AS Dollars_TPR,
                                      SUM([Dollars_FeatAndDisp]) AS Dollars_FeatAndDisp,
                                      SUM([Units]) AS Sales,
                                      SUM([Units_Feature]) AS Sales_feature,
                                      SUM([Units_Display]) AS Sales_display,
                                      SUM([Units_TPR]) AS Sales_tpr,
                                      SUM([Units_FeatAndDisp]) AS Sales_FeatAndDisp
                                      FROM [dbo].[t_WeeklySales]
                                      WHERE [GEO] = 'WALMART'
                                      AND [Attr1] = 'FACE CARE'
                                      GROUP BY [GEO],
                                      [WeekEnding],
                                      [Attr1],
                                      [Attr2]"))

target_3 <- sqlQuery(dbHandle, paste("SELECT [WeekEnding],
                                     [Attr2],
                                     SUM([Dollars]) AS Dollars,
                                     SUM([Dollars_Feature]) AS Dollars_feature,
                                     SUM([Dollars_Display]) AS Dollars_Display,
                                     SUM([Dollars_TPR]) AS Dollars_TPR,
                                     SUM([Dollars_FeatAndDisp]) AS Dollars_FeatAndDisp,
                                     SUM([Units]) AS Sales,
                                     SUM([Units_Feature]) AS Sales_feature,
                                     SUM([Units_Display]) AS Sales_display,
                                     SUM([Units_TPR]) AS Sales_tpr,
                                     SUM([Units_FeatAndDisp]) AS Sales_FeatAndDisp
                                     FROM [dbo].[t_WeeklySales]
                                     WHERE [GEO] = 'TARGET'
                                     AND [Attr1] = 'FACE CARE'
                                     GROUP BY [GEO],
                                     [WeekEnding],
                                     [Attr1],
                                     [Attr2]"))

cvs_3 <- sqlQuery(dbHandle, paste("SELECT [WeekEnding],
                                  [Attr2],
                                  SUM([Dollars]) AS Dollars,
                                  SUM([Dollars_Feature]) AS Dollars_feature,
                                  SUM([Dollars_Display]) AS Dollars_Display,
                                  SUM([Dollars_TPR]) AS Dollars_TPR,
                                  SUM([Dollars_FeatAndDisp]) AS Dollars_FeatAndDisp,
                                  SUM([Units]) AS Sales,
                                  SUM([Units_Feature]) AS Sales_feature,
                                  SUM([Units_Display]) AS Sales_display,
                                  SUM([Units_TPR]) AS Sales_tpr,
                                  SUM([Units_FeatAndDisp]) AS Sales_FeatAndDisp
                                  FROM [dbo].[t_WeeklySales]
                                  WHERE [GEO] = 'CVS'
                                  AND [Attr1] = 'FACE CARE'
                                  GROUP BY [GEO],
                                  [WeekEnding],
                                  [Attr1],
                                  [Attr2]"))

walgree_3 <- sqlQuery(dbHandle, paste("SELECT [WeekEnding],
                                      [Attr2],
                                      SUM([Dollars]) AS Dollars,
                                      SUM([Dollars_Feature]) AS Dollars_feature,
                                      SUM([Dollars_Display]) AS Dollars_Display,
                                      SUM([Dollars_TPR]) AS Dollars_TPR,
                                      SUM([Dollars_FeatAndDisp]) AS Dollars_FeatAndDisp,
                                      SUM([Units]) AS Sales,
                                      SUM([Units_Feature]) AS Sales_feature,
                                      SUM([Units_Display]) AS Sales_display,
                                      SUM([Units_TPR]) AS Sales_tpr,
                                      SUM([Units_FeatAndDisp]) AS Sales_FeatAndDisp
                                      FROM [dbo].[t_WeeklySales]
                                      WHERE [GEO] = 'WALGREENS'
                                      AND [Attr1] = 'FACE CARE'
                                      GROUP BY [GEO],
                                      [WeekEnding],
                                      [Attr1],
                                      [Attr2]"))


big_y_3 <- sqlQuery(dbHandle, paste("SELECT [WeekEnding],
                                    [Attr2],
                                    SUM([Dollars]) AS Dollars,
                                    SUM([Dollars_Feature]) AS Dollars_feature,
                                    SUM([Dollars_Display]) AS Dollars_Display,
                                    SUM([Dollars_TPR]) AS Dollars_TPR,
                                    SUM([Dollars_FeatAndDisp]) AS Dollars_FeatAndDisp,
                                    SUM([Units]) AS Sales,
                                    SUM([Units_Feature]) AS Sales_feature,
                                    SUM([Units_Display]) AS Sales_display,
                                    SUM([Units_TPR]) AS Sales_tpr,
                                    SUM([Units_FeatAndDisp]) AS Sales_FeatAndDisp
                                    FROM [dbo].[t_WeeklySales]
                                    WHERE [GEO] = 'BIG Y'
                                    AND [Attr1] = 'MARG_BUTTER'
                                    GROUP BY [GEO],
                                    [WeekEnding],
                                    [Attr1],
                                    [Attr2]"))




# Normalize

#perform log transformation to avgTP and all EMV related attributes and nearwinter_snow
#normalize by z-score 


#normalize <- function(x){return ((x - mean(x, na.rm = TRUE))/sd(x,na.rm = TRUE))}                                                                                                                            [Attr2]"))

mutation <- function(x) {return (normalize(log(x +1)))}

walmart_3[,3:12] <- sapply(walmart_3[,3:12],function(x) {return (normalize(log(x +1)))})


target_3[,3:12] <- sapply(walmart_3[,3:12],function(x) {return (normalize(log(x +1)))})


walgree_3[,3:12] <- sapply(walmart_3[,3:12],function(x) {return (normalize(log(x +1)))})


cvs_3[,3:12] <- sapply(walmart_3[,3:12],function(x) {return (normalize(log(x +1)))})


# target & FACE CARE

target_3$Attr2<- as.factor(target_3$Attr2)

plm_target_3_u <-plm(Sales ~ Sales_feature + Sales_display
                     + Sales_tpr + Sales_FeatAndDisp, data=target_3,effect="twoways",
                     index=c('Attr2','WeekEnding'),model="within")
summary(plm_target_3_u)


Coefficients :
  Estimate Std. Error t-value  Pr(>|t|)    
Sales_feature      0.2178360  0.0634401  3.4337 0.0006777 ***
  Sales_display      1.0222268  0.0505022 20.2412 < 2.2e-16 ***
  Sales_tpr         -0.0077182  0.0535111 -0.1442 0.8854091    
Sales_FeatAndDisp -0.1663502  0.0625653 -2.6588 0.0082553 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    312.37
Residual Sum of Squares: 102.4
R-Squared:      0.67217
Adj. R-Squared: 0.55287
F-statistic: 156.343 on 4 and 305 DF, p-value: < 2.22e-16

# walmart  & FACE CARE

walmart_3$Attr2<- as.factor(walmart_3$Attr2)

plm_walmart_3_u <-plm(Sales ~ Sales_feature + Sales_display
                      + Sales_tpr + Sales_FeatAndDisp, data=walmart_3,effect="twoways",
                      index=c('Attr2','WeekEnding'),model="within")
summary(plm_walmart_3_u)

Coefficients :
  Estimate Std. Error t-value  Pr(>|t|)    
Sales_feature     -0.0134231  0.0088880 -1.5102    0.1318    
Sales_display      0.1849935  0.0081270 22.7629 < 2.2e-16 ***
  Sales_tpr          0.0489255  0.0084556  5.7862 1.436e-08 ***
  Sales_FeatAndDisp  0.0059780  0.0082543  0.7242    0.4693    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    6.825
Residual Sum of Squares: 2.5003
R-Squared:      0.63366
Adj. R-Squared: 0.53399
F-statistic: 176.43 on 4 and 408 DF, p-value: < 2.22e-16


# walgreen  & FACE CARE

walgree_3$Attr2<- as.factor(walgree_3$Attr2)

plm_walgree_3_u <-plm(Sales ~ Sales_feature + Sales_display
                      + Sales_tpr + Sales_FeatAndDisp, data=walgree_3,effect="twoways",
                      index=c('Attr2','WeekEnding'),model="within")
summary(plm_walgree_3_u)


Coefficients :
  Estimate Std. Error t-value Pr(>|t|)    
Sales_feature      0.154161   0.060744  2.5379  0.01165 *  
  Sales_display      0.952714   0.051967 18.3331  < 2e-16 ***
  Sales_tpr          0.075368   0.057023  1.3217  0.18726    
Sales_FeatAndDisp -0.132096   0.059508 -2.2198  0.02717 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    300.35
Residual Sum of Squares: 104.37
R-Squared:      0.65249
Adj. R-Squared: 0.52602
F-statistic: 143.167 on 4 and 305 DF, p-value: < 2.22e-16


# CVS & FACE CARE

cvs_3$Attr2<- as.factor(cvs_3$Attr2)

plm_cvs_3_u <-plm(Sales ~ Sales_feature + Sales_display
                  + Sales_tpr + Sales_FeatAndDisp, data=cvs_3,effect="twoways",
                  index=c('Attr2','WeekEnding'),model="within")
summary(plm_cvs_3_u)

Estimate Std. Error t-value Pr(>|t|)  
Sales_feature     -0.136680   0.073256 -1.8658  0.06365 .
Sales_display     -0.162374   0.074214 -2.1879  0.02992 *
  Sales_tpr          0.015521   0.069546  0.2232  0.82364  
Sales_FeatAndDisp -0.071193   0.069018 -1.0315  0.30364  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    183.05
Residual Sum of Squares: 175.56
R-Squared:      0.040897
Adj. R-Squared: -0.52116
F-statistic: 1.98279 on 4 and 186 DF, p-value: 0.098861

# Butter


big_y_3[,3:12] <- sapply(big_y_3[,3:12],function(x) {return (normalize(log(x +1)))})


# big y & butter

big_y_3$Attr2<- as.factor(big_y_3$Attr2)

plm_big_y_3_u <-plm(Sales ~ Sales_feature + Sales_display
                    + Sales_tpr + Sales_FeatAndDisp, data=big_y_3,effect="twoways",
                    index=c('Attr2','WeekEnding'),model="within")
summary(plm_big_y_3_u)
