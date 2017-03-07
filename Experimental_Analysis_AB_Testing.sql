/*
Idea & Intuition:
- Find items to compare for pseudo A/B testing to see the "sensitivity" of different items in different retailers 
- Uses Euclidean distance
- Minimize the distance in parts to keep consistent, maximize in parts to compare

Terminology:
- Baseline: Item by retailer/week with no features/displays/TPR/promotion/etc. The control in the experiment.
- Treatment: The items with features/displays/TPR/promotions/etc. to compare against the control.
*/

WITH Staging AS (
SELECT
 GEO AS Retailer
, UPC
, WeekEnding
, MONTH(WeekEnding) AS Month

-- , WEEKDAY(WeekEnding) AS WeekDay
-- , Description
-- , Attr1
-- , Attr2
-- , Attr3
-- , Attr4
-- , BRAND

-- Price per unit
, Dollars / NULLIF(Units, 0) AS PricePerUnit
, BaseDollars / NULLIF(BaseUnits, 0) AS PricePerBaseUnit
, (Dollars - Dollars_Display - Dollars_TPR - Dollars_FeatAndDisp - Dollars_Feature) / NULLIF((Units - Units_Display - Units_TPR - Units_FeatAndDisp - Units_Feature), 0) AS NormalPricePerUnit
, Dollars_Feature / NULLIF(Units_Feature, 0) AS FeaturePricePerUnit
, Dollars_Display / NULLIF(Units_Display, 0) AS DisplayPricePerUnit
, Dollars_TPR / NULLIF(Units_TPR, 0) AS TPRPricePerUnit
, Dollars_FeatAndDisp / NULLIF(Units_FeatAndDisp, 0) AS FeatAndDisPricePerUnit

-- Percent changes from base price
, 1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars / NULLIF(Units, 0))) AS PctPriceChange
, 1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Feature / NULLIF(Units_Feature, 0))) AS PctFeaturePriceChange
, 1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Display / NULLIF(Units_Display, 0))) AS PctDisplayPriceChange
, 1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_TPR / NULLIF(Units_TPR, 0))) AS PctTPRPriceChange
, 1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_FeatAndDisp / NULLIF(Units_FeatAndDisp, 0))) AS PctFeatAndDispPriceChange

-- Percent of unit sales attributed to each special
, ((Units - Units_Display - Units_TPR - Units_FeatAndDisp - Units_Feature) / NULLIF(Units, 0)) AS PctNormalUnits
, (Units_Feature / NULLIF(Units, 0)) AS PctFeatureUnits
, (Units_Display / NULLIF(Units, 0)) AS PctDisplayUnits
, (Units_TPR / NULLIF(Units, 0)) AS PctTPRUnits
, (Units_FeatAndDisp / NULLIF(Units, 0)) AS PctFeatAndDisUnits

, Dollars
, BaseDollars
, Dollars - Dollars_Display - Dollars_TPR - Dollars_FeatAndDisp - Dollars_Feature AS NormalPrice
, Dollars_Feature
, Dollars_Display
, Dollars_TPR
, Dollars_FeatAndDisp

, Units
, BaseUnits
, Units - Units_Display - Units_TPR - Units_FeatAndDisp - Units_Feature AS NormalUnits
, Units_Display
, Units_TPR
, Units_FeatAndDisp
, Units_Feature

, PACV_FeatWODisp
, PACV_DispWOFeat
, PACV_FeatAndDisp
, PACV_Discount

, ACV

-- Flags for filtering
-- Use for finding the baseline to compare to and mutually exclusive (instances where there are not multiple categories of discounts/features/etc.)
, CASE WHEN (Units_Display + Units_TPR + Units_FeatAndDisp + Units_Feature) = 0 THEN 1 ELSE 0 END 
    AS NoSpecials
, CASE WHEN Units_Display > 0 AND Units = Units_Display AND (Units_TPR + Units_FeatAndDisp + Units_Feature) = 0 THEN 1 ELSE 0 END
    AS DisplayOnly
, CASE WHEN Units_Feature > 0 AND Units = Units_Feature AND (Units_TPR + Units_FeatAndDisp + Units_Display) = 0 THEN 1 ELSE 0 END
    AS FeatureOnly
, CASE WHEN Units_TPR > 0 AND Units = Units_TPR AND (Units_Feature + Units_FeatAndDisp + Units_Display) = 0 THEN 1 ELSE 0 END
    AS TPROnly
, CASE WHEN Units_FeatAndDisp  > 0 AND Units = Units_FeatAndDisp  AND (Units_Feature + Units_TPR + Units_Display) = 0 THEN 1 ELSE 0 END
    AS FeatAndDispOnly
, CASE WHEN Units_Display > 0 AND (Units_TPR + Units_FeatAndDisp + Units_Feature) > 0 THEN 1
       WHEN Units_Feature > 0 AND (Units_TPR + Units_FeatAndDisp + Units_Display) > 0 THEN 1
	   WHEN Units_FeatAndDisp > 0 AND (Units_TPR + Units_Feature + Units_Display) > 0 THEN 1
	   WHEN Units_TPR > 0 AND (Units_Display + Units_Feature + Units_FeatAndDisp) > 0 THEN 1
	 ELSE 0 END
    AS FuzzyMethods

---- Normalized values for distance calculation
-- Distances to miniminize/maximinize (PACV comparing against)
-- Look into adding: Dates/seasonality, correlated items, external factors
, (ACV - (MIN(ACV) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX(ACV) OVER (PARTITION BY GEO, UPC) - MIN(ACV) OVER (PARTITION BY GEO, UPC), 0) 
    AS NormalizedACV
, ISNULL((PACV_FeatWODisp - (MIN(PACV_FeatWODisp) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX(PACV_FeatWODisp) OVER (PARTITION BY GEO, UPC) - MIN(PACV_FeatWODisp) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedPACVFeat
, ISNULL((PACV_DispWOFeat - (MIN(PACV_DispWOFeat) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX(PACV_DispWOFeat) OVER (PARTITION BY GEO, UPC) - MIN(PACV_DispWOFeat) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedPACVDisp
, ISNULL((PACV_FeatAndDisp - (MIN(PACV_FeatAndDisp) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX(PACV_FeatAndDisp) OVER (PARTITION BY GEO, UPC) - MIN(PACV_FeatAndDisp) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedPACVFeatAndDisp
, ISNULL((PACV_Discount - (MIN(PACV_Discount) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX(PACV_Discount) OVER (PARTITION BY GEO, UPC) - MIN(PACV_Discount) OVER (PARTITION BY GEO, UPC),0),0) 
    AS NormalizedPACVDiscount
, ISNULL(((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars / NULLIF(Units, 0))))- (MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars / NULLIF(Units, 0))))) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars / NULLIF(Units, 0))))) OVER (PARTITION BY GEO, UPC) - MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars / NULLIF(Units, 0))))) OVER (PARTITION BY GEO, UPC),0),0) 
    AS NormalizedPctPriceChange
, ISNULL(((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Feature / NULLIF(Units_Feature, 0)))) - (MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Feature / NULLIF(Units_Feature, 0))))) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Feature / NULLIF(Units_Feature, 0))))) OVER (PARTITION BY GEO, UPC) - MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Feature / NULLIF(Units_Feature, 0))))) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedPctFeaturePriceChange
, ISNULL(((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Display / NULLIF(Units_Display, 0)))) - (MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Display / NULLIF(Units_Display, 0))))) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Display / NULLIF(Units_Display, 0))))) OVER (PARTITION BY GEO, UPC) - MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_Display / NULLIF(Units_Display, 0))))) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedPctDisplayPriceChange
, ISNULL(((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_TPR / NULLIF(Units_TPR, 0))))- (MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_TPR / NULLIF(Units_TPR, 0))))) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_TPR / NULLIF(Units_TPR, 0))))) OVER (PARTITION BY GEO, UPC) - MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_TPR / NULLIF(Units_TPR, 0))))) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedTRPPriceChange
, ISNULL(((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_FeatAndDisp / NULLIF(Units_FeatAndDisp, 0)))) - (MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_FeatAndDisp / NULLIF(Units_FeatAndDisp, 0))))) OVER (PARTITION BY GEO, UPC))) / NULLIF(MAX((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_FeatAndDisp / NULLIF(Units_FeatAndDisp, 0))))) OVER (PARTITION BY GEO, UPC) - MIN((1 - ((BaseDollars / NULLIF(BaseUnits, 0)) / (Dollars_FeatAndDisp / NULLIF(Units_FeatAndDisp, 0))))) OVER (PARTITION BY GEO, UPC),0),0)
    AS NormalizedPctFeatAndDispPriceChange

FROM [CSOM_MSBA_ULDATA].[dbo].[t_WeeklySales]
WHERE 
--GEO = 'ACME' AND UPC = 45019302  -- Specific item/retailer
GEO = 'ACME' AND Attr1 = 'COTTON'
)


/* Selecting the control group */

-- Calculates the average price per unit of the baseline items
-- This is to be used to compare the distance between normal price and the average price per item in order to pick items without natural price fluctuations
, BaselineAvg AS (
SELECT
 UPC, Retailer
 , AVG(NormalPricePerUnit) AS AvgNormalPricePerUnit
 , AVG(Units) AS AvgUnits
FROM Staging
WHERE NoSpecials = 1
GROUP BY UPC, Retailer
)

-- Selecting a baseline to be compared against
, Baseline AS (
SELECT
*
FROM
(
SELECT 
 RANK() OVER (ORDER BY SQRT(SQUARE(AvgNormalPricePerUnit - PricePerUnit) + SQUARE(AvgUnits - Units))) AS NormalityRank
, SQRT(SQUARE(AvgNormalPricePerUnit - PricePerUnit)) AS Distance
, BaselineAvg.AvgNormalPricePerUnit
, 'Control' AS ExperimentGroup
, Staging.* 
FROM Staging
  JOIN BaselineAvg ON Staging.UPC = BaselineAvg.UPC AND Staging.Retailer = BaselineAvg.Retailer) AS Ranking
WHERE NormalityRank <= 5
)


/* Selecting the treatment group */

-- Features
, FeatureDist AS (
SELECT
SQRT(
 SQUARE(BaselineDist.NormalizedACV - Staging.NormalizedACV)
+ SQUARE(BaselineDist.NormalizedPACVDiscount - Staging.NormalizedPACVDiscount)
+ SQUARE(BaselineDist.NormalizedPACVDisp - Staging.NormalizedPACVDisp)
+ SQUARE(BaselineDist.NormalizedPACVFeatAndDisp - Staging.NormalizedPACVFeatAndDisp)
+ SQUARE(BaselineDist.NormalizedPctDisplayPriceChange - Staging.NormalizedPctDisplayPriceChange)
+ SQUARE(BaselineDist.NormalizedPctFeatAndDispPriceChange - Staging.NormalizedPctFeatAndDispPriceChange)
+ SQUARE(BaselineDist.NormalizedPctPriceChange - Staging.NormalizedPctPriceChange)
+ SQUARE(BaselineDist.NormalizedTRPPriceChange - Staging.NormalizedTRPPriceChange)
) AS DistToMinimize
, ((Units_Feature - BaselineDist.Units) / BaselineDist.Units) / ((FeaturePricePerUnit - BaselineDist.NormalPricePerUnit) / BaselineDist.NormalPricePerUnit)
  AS PriceSensitivity
, Staging.Retailer
, Staging.UPC
, Staging.WeekEnding
, 'Treatment-Feature' AS ExperimentGroup
, Month
-- Price per unit
, PricePerUnit, PricePerBaseUnit, Staging.NormalPricePerUnit, FeaturePricePerUnit, DisplayPricePerUnit, TPRPricePerUnit, FeatAndDisPricePerUnit
-- Percentages
, PctPriceChange, PctFeaturePriceChange, PctDisplayPriceChange, PctTPRPriceChange, PctFeatAndDispPriceChange
-- Percentage of units sold
, Staging.PctNormalUnits, PctFeatureUnits, PctDisplayUnits, PctTPRUnits, PctFeatAndDisUnits
-- Dollars
, BaseDollars, Dollars_Feature, Dollars_Display, Dollars_TPR, Dollars_FeatAndDisp
-- Units
, Staging.Units, BaseUnits, NormalUnits, Units_Display, Units_TPR, Units_FeatAndDisp
-- PACV/ACV
, PACV_FeatAndDisp, PACV_FeatWODisp, PACV_Discount, PACV_DispWOFeat, ACV
-- Flags
, NoSpecials, DisplayOnly, FeatAndDispOnly, FeatureOnly, TPROnly, FuzzyMethods

-- Baseline identifiers - the control to compare against
, BaselineDist.Retailer AS BaselineRetailer
, BaselineDist.UPC AS BaselineUPC
, BaselineDist.WeekEnding AS BaselineWeekEnding
FROM Staging
  -- Joining only the normalized distances and keys from baseline in order to calculate the distances to determine the parts to be compared in the "A/B Testing"
  JOIN (SELECT UPC, Retailer, WeekEnding, Units, NormalPricePerUnit
		, NormalizedACV, NormalizedPACVDiscount, NormalizedPACVDisp, NormalizedPACVFeat, NormalizedPACVFeatAndDisp, NormalizedPctDisplayPriceChange, NormalizedPctFeatAndDispPriceChange, NormalizedPctFeaturePriceChange, NormalizedPctPriceChange, NormalizedTRPPriceChange 
		FROM Baseline) AS BaselineDist ON Staging.UPC = BaselineDist.UPC AND Staging.Retailer = BaselineDist.Retailer
WHERE FeatureOnly = 1 OR PctFeatureUnits > 0.7
)

, FeaturePriceSensitivity AS (
SELECT
 UPC, Retailer
, AVG(PriceSensitivity) AS Feature_PriceSensitivity  -- Look into weighted averages
-- Lower DistToMinimize are weighted heavier
-- Achieved by (sum(distances) / dist) /sum(sum(distances) / dist)
--, ((SELECT SUM(DistToMinimize) FROM FeatureDist) / DistToMinimize) / (SELECT SUM(a) FROM (SELECT SUM(DistToMinimize) AS a FROM FeatureDist) / DistToMinimize)\
FROM FeatureDist
  --JOIN (SELECT SUM(DistToMinimize) AS WeightedAvgSum FROM FeatureDist) AS WeightedAvgSum)
GROUP BY UPC, Retailer
)


-- Display
, DisplayDist AS (
SELECT
SQRT(
 SQUARE(BaselineDist.NormalizedACV - Staging.NormalizedACV)
+ SQUARE(BaselineDist.NormalizedPACVDiscount - Staging.NormalizedPACVDiscount)
+ SQUARE(BaselineDist.NormalizedPACVFeat - Staging.NormalizedPACVFeat)
+ SQUARE(BaselineDist.NormalizedPACVFeatAndDisp - Staging.NormalizedPACVFeatAndDisp)
+ SQUARE(BaselineDist.NormalizedPctFeaturePriceChange - Staging.NormalizedPctFeaturePriceChange)
+ SQUARE(BaselineDist.NormalizedPctFeatAndDispPriceChange - Staging.NormalizedPctFeatAndDispPriceChange)
+ SQUARE(BaselineDist.NormalizedPctPriceChange - Staging.NormalizedPctPriceChange)
+ SQUARE(BaselineDist.NormalizedTRPPriceChange - Staging.NormalizedTRPPriceChange)
) AS DistToMinimize
, ((Units_Display - BaselineDist.Units) / BaselineDist.Units) / ((DisplayPricePerUnit - BaselineDist.NormalPricePerUnit) / BaselineDist.NormalPricePerUnit)
  AS PriceSensitivity
, Staging.Retailer
, Staging.UPC
, Staging.WeekEnding
, 'Treatment-Display' AS ExperimentGroup
, Month
-- Price per unit
, PricePerUnit, PricePerBaseUnit, Staging.NormalPricePerUnit, FeaturePricePerUnit, DisplayPricePerUnit, TPRPricePerUnit, FeatAndDisPricePerUnit
-- Percentages
, PctPriceChange, PctFeaturePriceChange, PctDisplayPriceChange, PctTPRPriceChange, PctFeatAndDispPriceChange
-- Percentage of units sold
, Staging.PctNormalUnits, PctFeatureUnits, PctDisplayUnits, PctTPRUnits, PctFeatAndDisUnits
-- Dollars
, BaseDollars, Dollars_Feature, Dollars_Display, Dollars_TPR, Dollars_FeatAndDisp
-- Units
, Staging.Units, BaseUnits, NormalUnits, Units_Display, Units_TPR, Units_FeatAndDisp
-- PACV/ACV
, PACV_FeatAndDisp, PACV_FeatWODisp, PACV_Discount, PACV_DispWOFeat, ACV
-- Flags
, NoSpecials, DisplayOnly, FeatAndDispOnly, FeatureOnly, TPROnly, FuzzyMethods

-- Baseline identifiers - the control to compare against
, BaselineDist.Retailer AS BaselineRetailer
, BaselineDist.UPC AS BaselineUPC
, BaselineDist.WeekEnding AS BaselineWeekEnding
FROM Staging
  -- Joining only the normalized distances and keys from baseline in order to calculate the distances to determine the parts to be compared in the "A/B Testing"
  JOIN (SELECT UPC, Retailer, WeekEnding, Units, NormalPricePerUnit
		, NormalizedACV, NormalizedPACVDiscount, NormalizedPACVDisp, NormalizedPACVFeat, NormalizedPACVFeatAndDisp, NormalizedPctDisplayPriceChange, NormalizedPctFeatAndDispPriceChange, NormalizedPctFeaturePriceChange, NormalizedPctPriceChange, NormalizedTRPPriceChange 
		FROM Baseline) AS BaselineDist ON Staging.UPC = BaselineDist.UPC AND Staging.Retailer = BaselineDist.Retailer
WHERE DisplayOnly = 1 OR PctDisplayUnits > 0.7
)

, DisplayPriceSensitivity AS (
SELECT
 UPC, Retailer
, AVG(PriceSensitivity) AS Display_PriceSensitivity  -- Look into weighted averages
-- Lower DistToMinimize are weighted heavier
-- Achieved by (sum(distances) / dist) /sum(sum(distances) / dist)
--, ((SELECT SUM(DistToMinimize) FROM FeatureDist) / DistToMinimize) / (SELECT SUM(a) FROM (SELECT SUM(DistToMinimize) AS a FROM FeatureDist) / DistToMinimize)\
FROM DisplayDist
  --JOIN (SELECT SUM(DistToMinimize) AS WeightedAvgSum FROM FeatureDist) AS WeightedAvgSum)
GROUP BY UPC, Retailer
)


-- Discounts
, DiscountDist AS (
SELECT
SQRT(
 SQUARE(BaselineDist.NormalizedACV - Staging.NormalizedACV)
+ SQUARE(BaselineDist.NormalizedPACVFeat - Staging.NormalizedPACVFeat)
+ SQUARE(BaselineDist.NormalizedPACVDisp - Staging.NormalizedPACVDisp)
+ SQUARE(BaselineDist.NormalizedPACVFeatAndDisp - Staging.NormalizedPACVFeatAndDisp)
+ SQUARE(BaselineDist.NormalizedPctDisplayPriceChange - Staging.NormalizedPctDisplayPriceChange)
+ SQUARE(BaselineDist.NormalizedPctFeatAndDispPriceChange - Staging.NormalizedPctFeatAndDispPriceChange)
+ SQUARE(BaselineDist.NormalizedPctPriceChange - Staging.NormalizedPctPriceChange)
+ SQUARE(BaselineDist.NormalizedPctFeaturePriceChange- Staging.NormalizedPctFeaturePriceChange)
) AS DistToMinimize
, ((Units_TPR - BaselineDist.Units) / BaselineDist.Units) / ((TPRPricePerUnit - BaselineDist.NormalPricePerUnit) / BaselineDist.NormalPricePerUnit)
  AS PriceSensitivity
, Staging.Retailer
, Staging.UPC
, Staging.WeekEnding
, 'Treatment-Discount' AS ExperimentGroup
, Month
-- Price per unit
, PricePerUnit, PricePerBaseUnit, Staging.NormalPricePerUnit, FeaturePricePerUnit, DisplayPricePerUnit, TPRPricePerUnit, FeatAndDisPricePerUnit
-- Percentages
, PctPriceChange, PctFeaturePriceChange, PctDisplayPriceChange, PctTPRPriceChange, PctFeatAndDispPriceChange
-- Percentage of units sold
, Staging.PctNormalUnits, PctFeatureUnits, PctDisplayUnits, PctTPRUnits, PctFeatAndDisUnits
-- Dollars
, BaseDollars, Dollars_Feature, Dollars_Display, Dollars_TPR, Dollars_FeatAndDisp
-- Units
, Staging.Units, BaseUnits, NormalUnits, Units_Display, Units_TPR, Units_FeatAndDisp
-- PACV/ACV
, PACV_FeatAndDisp, PACV_FeatWODisp, PACV_Discount, PACV_DispWOFeat, ACV
-- Flags
, NoSpecials, DisplayOnly, FeatAndDispOnly, FeatureOnly, TPROnly, FuzzyMethods

-- Baseline identifiers - the control to compare against
, BaselineDist.Retailer AS BaselineRetailer
, BaselineDist.UPC AS BaselineUPC
, BaselineDist.WeekEnding AS BaselineWeekEnding
FROM Staging
  -- Joining only the normalized distances and keys from baseline in order to calculate the distances to determine the parts to be compared in the "A/B Testing"
  JOIN (SELECT UPC, Retailer, WeekEnding, Units, NormalPricePerUnit
		, NormalizedACV, NormalizedPACVDiscount, NormalizedPACVDisp, NormalizedPACVFeat, NormalizedPACVFeatAndDisp, NormalizedPctDisplayPriceChange, NormalizedPctFeatAndDispPriceChange, NormalizedPctFeaturePriceChange, NormalizedPctPriceChange, NormalizedTRPPriceChange 
		FROM Baseline) AS BaselineDist ON Staging.UPC = BaselineDist.UPC AND Staging.Retailer = BaselineDist.Retailer
WHERE TPROnly = 1 OR PctTPRUnits > 0.7
)

, DiscountPriceSensitivity AS (
SELECT
 UPC, Retailer
, AVG(PriceSensitivity) AS Discount_PriceSensitivity  -- Look into weighted averages
-- Lower DistToMinimize are weighted heavier
-- Achieved by (sum(distances) / dist) /sum(sum(distances) / dist)
--, ((SELECT SUM(DistToMinimize) FROM FeatureDist) / DistToMinimize) / (SELECT SUM(a) FROM (SELECT SUM(DistToMinimize) AS a FROM FeatureDist) / DistToMinimize)\
FROM DiscountDist
  --JOIN (SELECT SUM(DistToMinimize) AS WeightedAvgSum FROM FeatureDist) AS WeightedAvgSum)
GROUP BY UPC, Retailer
)


-- Feature And Display
, FeatAndDispDist AS (
SELECT
SQRT(
 SQUARE(BaselineDist.NormalizedACV - Staging.NormalizedACV)
+ SQUARE(BaselineDist.NormalizedPACVFeat - Staging.NormalizedPACVFeat)
+ SQUARE(BaselineDist.NormalizedPACVDisp - Staging.NormalizedPACVDisp)
+ SQUARE(BaselineDist.NormalizedPACVDiscount - Staging.NormalizedPACVDiscount)
+ SQUARE(BaselineDist.NormalizedPctDisplayPriceChange - Staging.NormalizedPctDisplayPriceChange)
--+ SQUARE(BaselineDist.NormalizedPctPriceChange- Staging.NormalizedPctFeatAndDispPriceChange)
+ SQUARE(BaselineDist.NormalizedPctPriceChange - Staging.NormalizedPctPriceChange)
+ SQUARE(BaselineDist.NormalizedPctFeaturePriceChange- Staging.NormalizedPctFeaturePriceChange)
) AS DistToMinimize
, ((Units_FeatAndDisp - BaselineDist.Units) / BaselineDist.Units) / ((FeatAndDisPricePerUnit - BaselineDist.NormalPricePerUnit) / BaselineDist.NormalPricePerUnit)
  AS PriceSensitivity
, Staging.Retailer
, Staging.UPC
, Staging.WeekEnding
, 'Treatment-FeatureAndDisplay' AS ExperimentGroup
, Month
-- Price per unit
, PricePerUnit, PricePerBaseUnit, Staging.NormalPricePerUnit, FeaturePricePerUnit, DisplayPricePerUnit, TPRPricePerUnit, FeatAndDisPricePerUnit
-- Percentages
, PctPriceChange, PctFeaturePriceChange, PctDisplayPriceChange, PctTPRPriceChange, PctFeatAndDispPriceChange
-- Percentage of units sold
, Staging.PctNormalUnits, PctFeatureUnits, PctDisplayUnits, PctTPRUnits, PctFeatAndDisUnits
-- Dollars
, BaseDollars, Dollars_Feature, Dollars_Display, Dollars_TPR, Dollars_FeatAndDisp
-- Units
, Staging.Units, BaseUnits, NormalUnits, Units_Display, Units_TPR, Units_FeatAndDisp
-- PACV/ACV
, PACV_FeatAndDisp, PACV_FeatWODisp, PACV_Discount, PACV_DispWOFeat, ACV
-- Flags
, NoSpecials, DisplayOnly, FeatAndDispOnly, FeatureOnly, TPROnly, FuzzyMethods

-- Baseline identifiers - the control to compare against
, BaselineDist.Retailer AS BaselineRetailer
, BaselineDist.UPC AS BaselineUPC
, BaselineDist.WeekEnding AS BaselineWeekEnding
FROM Staging
  -- Joining only the normalized distances and keys from baseline in order to calculate the distances to determine the parts to be compared in the "A/B Testing"
  JOIN (SELECT UPC, Retailer, WeekEnding, Units, NormalPricePerUnit
		, NormalizedACV, NormalizedPACVDiscount, NormalizedPACVDisp, NormalizedPACVFeat, NormalizedPACVFeatAndDisp, NormalizedPctDisplayPriceChange, NormalizedPctFeatAndDispPriceChange, NormalizedPctFeaturePriceChange, NormalizedPctPriceChange, NormalizedTRPPriceChange 
		FROM Baseline) AS BaselineDist ON Staging.UPC = BaselineDist.UPC AND Staging.Retailer = BaselineDist.Retailer
WHERE FeatAndDispOnly = 1 OR PctFeatAndDisUnits > 0.7
)

, FeatAndDispPriceSensitivity AS (
SELECT
 UPC, Retailer
, AVG(PriceSensitivity) AS Feature_PriceSensitivity  -- Look into weighted averages
-- Lower DistToMinimize are weighted heavier
-- Achieved by (sum(distances) / dist) /sum(sum(distances) / dist)
--, ((SELECT SUM(DistToMinimize) FROM FeatureDist) / DistToMinimize) / (SELECT SUM(a) FROM (SELECT SUM(DistToMinimize) AS a FROM FeatureDist) / DistToMinimize)\
FROM FeatAndDispDist
  --JOIN (SELECT SUM(DistToMinimize) AS WeightedAvgSum FROM FeatureDist) AS WeightedAvgSum)
GROUP BY UPC, Retailer
)


SELECT
*
FROM Baseline
  LEFT JOIN FeatureDist ON Baseline.UPC = FeatureDist.UPC AND Baseline.Retailer = FeatureDist.Retailer
    LEFT JOIN FeaturePriceSensitivity ON Baseline.UPC = FeaturePriceSensitivity.UPC AND Baseline.Retailer = FeaturePriceSensitivity.Retailer
  LEFT JOIN DisplayDist ON Baseline.UPC = DisplayDist.UPC AND Baseline.Retailer = DisplayDist.Retailer
    LEFT JOIN DisplayPriceSensitivity ON Baseline.UPC = DisplayPriceSensitivity.UPC AND Baseline.Retailer = DisplayPriceSensitivity.Retailer
  LEFT JOIN DiscountDist ON Baseline.UPC = DiscountDist.UPC AND Baseline.Retailer = DiscountDist.Retailer
    LEFT JOIN DiscountPriceSensitivity ON Baseline.UPC = DiscountPriceSensitivity.UPC AND Baseline.Retailer = DiscountPriceSensitivity.Retailer
  LEFT JOIN FeatAndDispDist ON Baseline.UPC = FeatAndDispDist.UPC AND Baseline.Retailer = FeatAndDispDist.Retailer
    LEFT JOIN FeatAndDispPriceSensitivity ON Baseline.UPC = FeatAndDispPriceSensitivity.UPC AND Baseline.Retailer = FeatAndDispPriceSensitivity.Retailer