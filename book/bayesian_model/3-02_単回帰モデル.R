#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-2 単回帰モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P167 - P172
#***************************************************************************************


# ＜テーマ＞
# - Stanを用いた単回帰モデルの推定


# ＜目次＞
# 1 準備
# 2 MCMCの実行
# 3 事後分布の図示


# 1 準備 --------------------------------------------------------------------------

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
file_beer_sales_2 %>% glimpse()

# プロット確認
file_beer_sales_2 %>%
  ggplot(aes(x = temperature, y = sales)) +
    geom_point() +
    labs(title = "Relationship between beer sales and temperature")


# 2 MCMCの実行 -------------------------------------------------------------------

# データ作成
data_list <-
  list(sales       = file_beer_sales_2$sales,
       temperature = file_beer_sales_2$temperature,
       N           = nrow(file_beer_sales_2))

# 乱数の生成
# --- 正規分布を想定
mcmc_result_not_vec <-
  stan(file = "book/bayesian_model/stan/3-2-1-simple-lm.stan",
       data = data_list,
       seed = 1)

# 乱数の生成
# --- 蒸気をベクトル化したもの
mcmc_result_vec <-
  stan(file = "book/bayesian_model/stan/3-2-2-simple-lm-vec.stan",
       data = data_list,
       seed = 1)

# 結果の表示
mcmc_result_not_vec %>% print(probs = c(0.025, 0.5, 0.975))
mcmc_result_vec %>% print(probs = c(0.025, 0.5, 0.975))


# 3 事後分布の図示 --------------------------------------------------------------

# MCMCサンプルの抽出
mcmc_sample <- mcmc_result %>% rstan::extract(permuted = FALSE)

# トレースプロットと事後分布
mcmc_sample %>% mcmc_combo(pars = c("Intercept", "beta", "sigma"))




