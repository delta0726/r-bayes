# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 2 単回帰モデル
# Date    : 2022/4/18
# Page    : P167 - P172
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - Stanを用いた単回帰モデルの推定するフローを確認する
#   --- モデル構築はstanファイルで行う
#   --- データからMCMCによる乱数生成で確率分布を生成する


# ＜目次＞
# 0 準備
# 1 分析データ確認
# 2 モデル構築とMCMCの実行
# 3 事後分布の確認


# 0 準備 --------------------------------------------------------------------------

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


# 1 分析データ確認 -----------------------------------------------------------------

# ＜ポイント＞
# - 気温(X)とビール売上高(Y)の関係を観測したデータセット
#   --- 概ね正の相関がある


# データ確認
file_beer_sales_2 %>% print()
file_beer_sales_2 %>% glimpse()

# プロット確認
file_beer_sales_2 %>%
  ggplot(aes(x = temperature, y = sales)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE, colour = "black", size = 0.7) +
    labs(title = "Relationship between beer sales and temperature")


# 2 モデル構築とMCMCの実行 ---------------------------------------------------------

# Stanデータ作成
data_list <-
  list(sales       = file_beer_sales_2$sales,
       temperature = file_beer_sales_2$temperature,
       N           = nrow(file_beer_sales_2))

# 乱数の生成
# --- 正規分布を想定
mcmc_result <-
  stan(file = "stan/3-2/3-2-1-simple-lm.stan",
       data = data_list,
       seed = 1)

# 乱数の生成
# --- 上記をベクトル化したもの
mcmc_result_vec <-
  stan(file = "stan/3-2/3-2-2-simple-lm-vec.stan",
       data = data_list,
       seed = 1)

# 結果の表示
# --- 乱数シードを固定していないので若干異なる
# --- betaが2.47なので気温が1度上がると2.47売上が増加することを示す
mcmc_result %>% print(probs = c(0.025, 0.5, 0.975))
mcmc_result_vec %>% print(probs = c(0.025, 0.5, 0.975))


# 3 事後分布の確認 --------------------------------------------------------------

# ＜ポイント＞
# - 推定されたパラメータの事後分布とトレースプロットを作成する
#   --- パラメータの範囲と収束を確認する

# MCMCサンプルの抽出
mcmc_sample <- mcmc_result %>% rstan::extract(permuted = FALSE)

# トレースプロットと事後分布
mcmc_sample %>% mcmc_combo(pars = c("Intercept", "beta", "sigma"))
