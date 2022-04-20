# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 3 モデルを用いた予測
# Date    : 2022/4/19
# Page    : P173 - P179
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - Stanを用いた単回帰モデルで予測を行う


# ＜目次＞
# 0 準備
# 1 分析データ確認
# 2 モデル構築とMCMCの実行
# 3 モデル出力データの確認
# 4 予測分布のプロット


# 0 準備 ------------------------------------------------------------------------

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


# 1 データの整理 ----------------------------------------------------------------

# ＜ポイント＞
# - 今回はモデルに加えて予測も行うため予測共データも必要


# データ確認
file_beer_sales_2 %>% print()

# 予測用データの作成
# --- 気温を11度から30度まで変化させて、その時の売り上げを予測する
temperature_pred <- 11:30
temperature_pred



# 2 MCMCの実行 -----------------------------------------------------------------

# ＜ポイント＞
# - 予測用データもStanデータに加える


# Stanデータ作成
data_list_pred <-
  list(sales = file_beer_sales_2$sales,
       temperature = file_beer_sales_2$temperature,
       N = nrow(file_beer_sales_2),
       N_pred = length(temperature_pred),
       temperature_pred = temperature_pred)

# MCMCの実行
mcmc_result_pred <-
  stan(file = "stan/3-3/3-3-1-simple-lm-pred.stan",
       data = data_list_pred,
       seed = 1)


# 3 モデル出力データの確認 -------------------------------------------------------------

# ＜ポイント＞
# - モデルパラメータに加えて予測データについても平均値/標準誤差などが出力される
#   --- MCMCサンプリングの結果


# 結果表示
mcmc_result_pred %>% print(probs = c(0.025, 0.5, 0.975))

# MCMCサンプルの抽出
mcmc_sample_pred <-
  rstan::extract(mcmc_result_pred, permuted = FALSE)

# データ確認
# --- parameters = Intercept
# --- parameters = beta
# --- parameters = sigma
# --- parameters = mu_pred[1]-mu_pred[20]
mcmc_sample_pred %>% head(2)


# 4 予測分布のプロット -------------------------------------------------------------

# ＜ポイント＞
# - 予測の推定値は予測区間として出力される
#   --- 通常の線形回帰の場合は予測値が一意に定まる（又はモデルの標準誤差から予測区間を導く）


# プロット作成
# --- 気温が11度～30度まで1度ずつ変えたの時の95%予測区間
# --- 正規表現を用いてパラメタ名を指定（regex_pars引数）
mcmc_sample_pred %>%
  mcmc_intervals(regex_pars = "sales_pred.",
                 prob = 0.8,
                 prob_outer = 0.95)

# プロット作成
# --- 95%区間の比較
mcmc_sample_pred %>%
  mcmc_intervals(pars = c("mu_pred[1]", "sales_pred[1]"),
                 prob = 0.8,
                 prob_outer = 0.95)

# プロット作成
# --- 気温が11度と30度の時の、売り上げの予測分布
mcmc_sample_pred %>%
  mcmc_areas(pars = c("sales_pred[1]", "sales_pred[20]"),
             prob = 0.6,
             prob_outer = 0.99)
