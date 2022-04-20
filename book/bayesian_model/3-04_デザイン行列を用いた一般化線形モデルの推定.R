# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 4 デザイン行列を用いた一般化線形モデルの推定
# Date    : 2022/4/21
# Page    : P180- P184
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - デザイン行列を用いて一般化線形モデルを表現してStanで推定する
#   --- 前回までXはベクトルだったが今回は行列を渡す


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

# データロード
file_beer_sales_2 <- read_csv("csv/3-2-1-beer-sales-2.csv")

# データ確認
file_beer_sales_2 %>% print()


# 1 デザイン行列の作成 ------------------------------------------------------------

# ＜ポイント＞
# - デザイン行列とは一般化線形モデルなどを解くために行列表記したもの
#   --- モデルを表列表記したもので切片項も含む
#   --- Rの場合はフォーミュラとmodel.matrix()から作成することができる


# formulaの作成
formula_lm <- formula(sales ~ temperature)

# デザイン行列の作成
X <- formula_lm %>% model.matrix(file_beer_sales_2)

# 確認
X %>% head(n = 5)
X %>% class()


# 2 MCMCの実行 -------------------------------------------------------------------

# ＜ポイント＞
# - Xのデータを行列(デザイン行列)でインプットする（前回まではベクトル）


# Stanデータ作成
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
  stan(file = "stan/3-4/3-4-1-lm-design-matrix.stan",
       data = data_list_design,
       seed = 1)

# 結果の表示
mcmc_result_design %>% print(probs = c(0.025, 0.5, 0.975))
