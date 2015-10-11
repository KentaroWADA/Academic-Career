## thesis_20151008.R

##############################################################
## Title: Master's Thesis
## Purpose: Conduct Linear Regression Analysis
## Date: October/8/2015
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

## import dataset
regress <- read.csv("dataset/regress.csv")

##==========================##
## === [2. OLS / Level] === ##
##==========================##

## open "regress.csv"
## manually lag outcome variable manually
## manually substitute most recent data for NA 
## rename it "ols.csv"
ols <- read.csv("dataset/ols.csv")
ols <- filter(ols, health_coverage_total != "NA")

## ols analysis (level)
fit.1 <- lm(health_coverage_total ~ gini + democracy + gdp_per_capita + 
              left + inflation + trade_openness + 
              foreign_direct_investment, data = ols)
summary(fit.1)
stargazer(fit.1)
fit.2 <- lm(health_coverage_total ~ gini + democracy + gdp_per_capita + 
              center_left + inflation + trade_openness + 
              foreign_direct_investment + external_debt, data = ols)
summary(fit.2)
stargazer(fit.2)
fit.3 <- lm(health_coverage_total ~ gini + democracy + gdp_per_capita + 
              total_left + inflation + trade_openness + 
              foreign_direct_investment + external_debt, data = ols)
summary(fit.3)
stargazer(fit.3)

## check the result in detail
str(fit.1)

## display residual plot
par(mfrow = c(3, 3))
for(i in 1:9) 
  plot(y = fit.3$residuals, x = fit.3$model[, i], 
       ylab = "Residuals", xlab = colnames(fit.3$model)[i])
par(mfrow = c(1, 1))


## display the result of regression using coef-plot
coefplot(fit.1, intercept = F,
         title = "モデル1：係数の推定値（結果変数は医療保険普及率）", 
         xlab = "係数の推定値", ylab = "説明変数",
         newNames = c(fdim = "海外直接投資",　age65 = "65歳以上の人口比率",
                      trade_openness = "経済開放度", inflation = "インフレ率",
                      left = "議会における左派政党の割合", edm = "外部負債", gini = "ジニ係数",
                      gdp_per_capita = "一人当たりGDP", democracy = "民主主義度合",
                      left_center = "議会における中道左派政党の割合",
                      total_left = "議会における左派・中道左派政党の割合"))

coefplot(fit.2, intercept = F,
         title = "モデル2：係数の推定値（結果変数は医療保険普及率）", 
         xlab = "係数の推定値", ylab = "説明変数",
         newNames = c(fdim = "海外直接投資",　age65 = "65歳以上の人口比率",
                      trade_openness = "経済開放度", inflation = "インフレ率",
                      left = "議会における左派政党の割合", edm = "外部負債", gini = "ジニ係数",
                      gdp_per_capita = "一人当たりGDP", democracy = "民主主義度合",
                      left_center = "議会における中道左派政党の割合",
                      total_left = "議会における左派・中道左派政党の割合"))

coefplot(fit.3, intercept = F,
         title = "モデル3：係数の推定値（結果変数は医療保険普及率）", 
         xlab = "係数の推定値", ylab = "説明変数",
         newNames = c(fdim = "海外直接投資",　age65 = "65歳以上の人口比率",
                      trade_openness = "経済開放度", inflation = "インフレ率",
                      left = "議会における左派政党の割合", edm = "外部負債", gini = "ジニ係数",
                      gdp_per_capita = "一人当たりGDP", democracy = "民主主義度合",
                      center_left = "議会における中道左派政党の割合",
                      total_left = "議会における左派・中道左派政党の割合"))

##===========================##
## === [3. OLS / Change] === ##
##===========================##

## open "ols.csv"
## manually calculate degree of change for each variable
## rename it "change_data.csv"
write.csv(ols, file = "change_data.csv")
change <- read.csv("dataset/change_data.csv")
change <- filter(change, health_coverage_total != "NA")

## ols analysis (change)
change.1 <- lm(health_coverage_total ~ gini + democracy + gdp_per_capita + 
              left + inflation + trade_openness + 
              foreign_direct_investment, data = change)
summary(change.1)
stargazer(change.1)

change.2 <- lm(health_coverage_total ~ gini + democracy + gdp_per_capita + 
              center_left + inflation + trade_openness + 
              foreign_direct_investment + external_debt, data = change)
summary(change.2)
stargazer(change.2)
change.3 <- lm(health_coverage_total ~ gini + democracy + gdp_per_capita + 
              total_left + inflation + trade_openness + 
              foreign_direct_investment + external_debt, data = change)
summary(change.3)
stargazer(change.3)

## regression diagnostic with residual plot
par(mfrow = c(3, 3))
for(i in 1:9) 
  plot(y = change.1$residuals, x = change.1$model[, i], 
       ylab = "Residuals", xlab = colnames(change.1$model)[i])
par(mfrow = c(1, 1))
