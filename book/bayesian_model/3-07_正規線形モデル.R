#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-7 正規線形モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/13
# Page      : P207 - P211
#***************************************************************************************


# ＜テーマ＞
# - 応答変数が正規分布に従うモデル(正規線形モデル)の確認
# - 簡単構造を組み合わせて複雑なモデルを構築する考え方を学ぶ


# ＜目次＞
# 0 準備
# 1 正規線形モデルの推定
# 2 正規線形モデルのデザイン行列


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
sales_climate <- read_csv("book/bayesian_model/csv/3-7-1-beer-sales-4.csv")

# データ確認
sales_climate %>% print()
sales_climate$weather %>% table()

# データの要約
sales_climate %>% summary()

# 図示
sales_climate %>%
  ggplot(mapping = aes(x = temperature, y = sales)) +
    geom_point(aes(color = weather)) +
    labs(title = "Relationship between beer sales and temperature")


# 1 正規線形モデルの推定 -------------------------------------------------------------

# 正規線形モデルを作る
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
  marginal_effects(effects = "temperature:weather")
  plot(points = TRUE)


# 2 正規線形モデルのデザイン行列 -------------------------------------------------------------

# デザイン行列の作成
formula_lm <- formula(sales ~ weather + temperature)
design_mat <- model.matrix(formula_lm, sales_climate)

# 確認
design_mat %>% print()
