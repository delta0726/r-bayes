#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-4 デザイン行列を用いた一般化線形モデルの推定
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P180- P184
#***************************************************************************************


# ＜テーマ＞
# - デザイン行列を用いて一般化線形モデルを表現してStanで推定


# ＜目次＞
# 0 準備
# 1 デザイン行列の作成
# 2 MCMCの実行


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(bayesplot)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_2 <- read_csv("book/bayesian_model/csv/3-2-1-beer-sales-2.csv")

# データ確認
file_beer_sales_2 %>% print()


# 1 デザイン行列の作成 ------------------------------------------------------------

# formulaの作成
formula_lm <- formula(sales ~ temperature)

# デザイン行列の作成
X <- formula_lm %>% model.matrix(file_beer_sales_2)

# formulaとmodel.matrixを使ったデザイン行列
X %>% head(n = 5)


# 2 MCMCの実行 -------------------------------------------------------------------

# データ作成
# --- N： サンプルサイズ
# --- K： デザイン行列の列数（説明変数の数＋１）
# --- Y： 応答変数
# --- X： デザイン行列
data_list_design <-
  list(N = nrow(file_beer_sales_2),
       K = 2,
       Y = file_beer_sales_2$sales,
       X = X)

# MCMCの実行
mcmc_result_design <-
  stan(file = "book/bayesian_model/stan/3-4-1-lm-design-matrix.stan",
       data = data_list_design,
       seed = 1)

# 結果の表示
mcmc_result_design %>% print(probs = c(0.025, 0.5, 0.975))
