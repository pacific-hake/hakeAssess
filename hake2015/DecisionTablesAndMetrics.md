Steps to create decision and metrics tables for the hake assessment
===================================================================

Create folders for
- decision table runs
- next year metrics
- second year metrics

Decision table runs
--------------------


Next year metrics
------------------
This is the metrics for a fixed catch in the next year (i.e., for the 2015 assessment, this is the 2015 catch).  In the forecast file, put in the next year's fixed catch. Make sure to enter only next year's catch so that the catch can be determined for the second year and compared to.

Second year metrics
-------------------
This is the metrics for the two year forecast. Enter catch for the next two years in the forecast file.


Catch Levels
------------
A fixed catch level is easy because you simply enter that catch level. There are a few catch levels that are determined based on specific states. These are listed below and how to determine them.

- B~curr~ = B~nextYr~ : This is the catch that results in an equal probability that the biomass in the current year is equal to the biomass in the next year.  You must iteratively modify the catch until the metric for P(BnextYr>Pcurr) is 50%.  Don't forget to run mceval after modifying the forecast file.
- med(B~curr~) = med(B~nextYr~) : This is the catch that results in the median spawning biomass next year to equal the median spawning biomass for this year. Iteratively modify the catch until the median spawning biomasses are approximately equal.  Don't forget to run mceval after modifying the forecast file.
- Stable Catch (Ccurr=CnextYr): This is the catch that results in teh default harvest rule catch for the next being the same. Iteratively enter the catch for the curent year until the median catch next year is the same.  Don't forget to run mceval after modifying the forecast file.
- SPR=100% : This si the catch that results in a median SPR of 100%. The default harvest rate catch may not have an SPR of 100% because of time-varying selectivity, growth (i.e., weight-at-age), etc.  This gives an indication of the current pattern of fishing and how it relates to the benchmark population.  Iteratively search for the catch that results in a median SPRratio of 1.  Don't forget to run mceval after modifying the forecast file.
- Default harvest rule: The catch determined by the default harvest rule. Simply copy the median ForeCatch caluclated by SS into the forecast file, so that every mceval uses that fixed catch. Don't forget to run mceval after modifying the forecast file.

NOTE: for second year metrics, fix the first year to determine the second year (i.e., fix 2015 catch at median default harvest rate catch, to determine the median default harvest catch for 2016).

Decision Tables
---------------
For decision tables, you will need to enter in catches for every year (different than metrics).  I have some Rcode that will create the directories and forecast files with the catch levels.  Then, run the mceval for each folder.  NOTE: you can have a zero catch, except for the final forecast year, so I usually enter 0.01 for zero catch.

With the decision tables displaying Bratio and SPR, the final year of forecasted catch is only pertinent for SPR, since Bratio is beginning of the year.