## thesis_20151004.R

##############################################################
## Title: Master's Thesis
## Purpose: Create Dataset for Linear Regression
## Date: October/4/2015
## Author: Kentaro Wada
##############################################################

##=============================##
## === [1. Basic Settings] === ##
##=============================##

## install necessary package
library("foreign") ## import data other than csv format
library("Hmisc") ## correlations with significance levels
library("dplyr") ## create new variables with mutate function, 
                 ## extract necessary variables with filter function
library("arm") ## show regression result with  table
library("coefplot") ## show regression result with high quality figure
library("ggplot2") ## show estimation result with high quality figure
library("xtable") ## export LaTeX style table 
library("stargazer") ## output LaTeX style table
library("psych") ## show basic statistics with describe function
library("car") ## B-P test (test for heteroskedasticity in a linear regression model)

## enable users to output in Japanese
theme_set(theme_gray(base_size = 12, base_family = "HiraKakuProN-W3"))

## import stata format data, and convert stata format data into csv format
rawdata <- read.dta("dataset/LAC_Political_2012.dta")
write.csv(rawdata, "lac_political_2012.csv", quote = FALSE, row.names = TRUE)

## import the data
polity <- read.csv("dataset/polity.csv", skip =1)
outcome <- read.csv("dataset/coverage.csv", skip = 1) 
explanatory <- read.csv("dataset/latin_america_data.csv", skip = 6)

## rearrange polity
polity.dominica <- filter(polity, year > 1944 & ccode == 42)
polity <- filter(polity, year > 1944 & ccode > 69)
polity <- filter(polity, ccode < 200)
polity <- filter(polity, ccode != 110 & ccode != 115)
polity <- merge(polity.dominica, polity, all = T)
polity <- polity[, -2]
polity <- polity[, -3:-4]
polity <- polity[,1:3]

## rearrange outcome
outcome <- outcome[, -3]
outcome <- outcome[, -4:-7] 
outcome <- filter(outcome, health_coverage_total != "NA")

## rearrange explanatory
explanatory <- explanatory[, -11:-24] 
explanatory <- mutate(explanatory, total_left = (left + center_left))

## created new dataset
newdata <- merge(polity, explanatory, all = T)
newdata <- merge(outcome, newdata, all = T)
newdata <- filter(newdata, state != "Latin America")
write.csv(newdata, file = "regress.csv")

## convert democracy into accumulated democracy year
## change the order manually, then import the dataset (democracy and total_left)
regress <- read.csv("dataset/regress.csv")

##======================================##
## === [2. Correlation Coefficient] === ##
##======================================##

## calculate correlation coefficient
cor <- regress[, -1:-3] ## remove "state" and "year" and "health_coverage_total"

## plot the correlation table
cor.table <- rcorr(as.matrix(cor), type = "pearson") ## type can be pearson or spearman
cor.table
## copy and paste "cor.table" at Excel and reload it (exclude variable "democracy")
cor.table <- read.csv("dataset/correlation_table.csv")
xtable(cor.table, align="lccccccccccc", label="tb-ref", caption = "説明変数の相関係数表") ##  table 3


##===============================##
## === [3. Basic Statistics] === ##
##===============================##

## output table with LaTeX style (table 1)
xtable(outcome, align="lccc", label="tb-ref", caption="ラテンアメリカにおける医療保険普及率")

## show basic statistics
basic <- describe(cor)
xtable(basic, align="lccccccccccccc",
       caption="変数の記述統計")


## measurement change (sample)
regress$gini <- explanatory$gini * 100
regress$left <- regress$left * 100
regress$center_left <- regress$center_left * 100
regress$total_left <- regress$total_left * 100
regress$foreign_direct_investment <- explanatory$foreign_direct_investment / 10^3 ## use "標準"
regress$gdp_per_capita <- regress$gdp_per_capita / 1000

## calcurate total yaer of democracy
length(which(polity$democracy > 5 & polity$state == "Venezuela" & polity$year < 1999))

## calcurate means of each variable
colMeans(argentina[,c(3:13)], na.rm = TRUE)
