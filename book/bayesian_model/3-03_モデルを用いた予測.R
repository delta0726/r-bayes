#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-3 モデルを用いた予測
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P173 - P179
#***************************************************************************************


# ＜テーマ＞
# - Stanを用いた単回帰モデルで予測を行う


# ＜目次＞
# 0 準備
# 1 データの整理
# 2 MCMCの実行
# 3 予測分布のプロット


# 0 準備 ------------------------------------------------------------------------

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


# 1 データの整理 ----------------------------------------------------------------

# パラメータ設定
# --- 気温を11度から30度まで変化させて、その時の売り上げを予測する
temperature_pred <-11:30
temperature_pred

# データ準備
data_list_pred <-
  list(N = nrow(file_beer_sales_2),
       sales = file_beer_sales_2$sales,
       temperature = file_beer_sales_2$temperature,
       N_pred = length(temperature_pred),
       temperature_pred = temperature_pred)


# 2 MCMCの実行 -----------------------------------------------------------------

# MCMCの実行
mcmc_result_pred <-
  stan(file = "book/bayesian_model/stan/3-3-1-simple-lm-pred.stan",
       data = data_list_pred,
       seed = 1)

# 結果表示
mcmc_result_pred %>% print(probs = c(0.025, 0.5, 0.975))


# 3 予測分布のプロット -------------------------------------------------------------

# MCMCサンプルの抽出
mcmc_sample_pred <-
  rstan::extract(mcmc_result_pred, permuted = FALSE)

# データ構造
mcmc_sample_pred %>% glimpse()

# プロット作成
# --- 気温が11度～30度まで1度ずつ変えたの時の95%予測区間
# --- 正規表現を用いてパラメタ名を指定
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
