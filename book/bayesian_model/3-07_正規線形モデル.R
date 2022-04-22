# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 7 正規線形モデル
# Date    : 2022/4/23
# Page    : P207 - P211
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 正規線形モデルとは量的データ/質的データの説明変数と正規分布に従う応答変数を持つモデルを指す
# - 簡単構造を組み合わせて複雑なモデルを構築する考え方を学ぶ


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 正規線形モデルの推定
# 3 デザイン行列の確認


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)

# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データロード
sales_climate <- read_csv("csv/3-7-1-beer-sales-4.csv")


# 1 データ確認 --------------------------------------------------------------------

# ＜ポイント＞
# - 質的データのweatherと量的データのtemperatureが含まれる


# データ確認
sales_climate %>% print()
sales_climate$weather %>% table()

# データの要約
sales_climate %>% summary()

# 図示
sales_climate %>%
  ggplot(mapping = aes(x = temperature, y = sales)) +
    geom_point(aes(color = weather)) +
    facet_wrap(~weather, nrow = 1) +
    labs(title = "Relationship between beer sales and temperature")


# 2 正規線形モデルの推定 -------------------------------------------------------------

# ＜ポイント＞
# - brm()ではデータの種類に限らずフォーミュラで変数を繋いでを定義する


# モデル構築
# --- 正規分布を使う
lm_brms <-
  brm(formula = sales ~ weather + temperature,
      family = gaussian(),
      data = sales_climate,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# MCMCの結果確認
lm_brms %>% print()

# 回帰直線
lm_brms %>%
  marginal_effects(effects = "temperature:weather") %>%
  plot(points = TRUE)


# 3 デザイン行列の確認 -------------------------------------------------------------

# ＜ポイント＞
# - 量的データはそのままの系列で質的データはダミー変数として系列が作られている


# デザイン行列の作成
formula_lm <- formula(sales ~ weather + temperature)
design_mat <- model.matrix(formula_lm, sales_climate)

# 確認
design_mat %>% head()
