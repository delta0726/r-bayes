#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-6 ダミー変数と分散分析モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P201 - P206
#***************************************************************************************


# ＜テーマ＞
# - ダミー変数について学んだ上で分散分析モデルをbrmsやrstanで推定する


# ＜目次＞
# 0 準備
# 1 分散分析モデルの推定
# 2 分散分析モデルのデザイン行列


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データロード
sales_weather <- read_csv("book/bayesian_model/csv/3-6-1-beer-sales-3.csv")

# データ確認
sales_weather %>% print()
sales_weather$weather %>% table()

# データの要約
sales_weather %>% summary()

# プロット作成
sales_weather %>%
  ggplot(mapping = aes(x = weather, y = sales)) +
    geom_violin() +
    geom_point(aes(color = weather)) +
    labs(title = "Relationship between beer sales and temperture")


# 1 分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る
# --- 正規分布を使う
anova_brms <-
  brm(formula = sales ~ weather,
      family = gaussian(),
      data = sales_weather,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# MCMCの結果確認
anova_brms %>% print()

# プロット作成
# --- 推定された天気別の平均売上
anova_brms %>%
  marginal_effects() %>%
  plot(points = FALSE)


# 2 分散分析モデルのデザイン行列 ----------------------------------------------------------

# デザイン行列の作成
formula_anova <- formula(sales ~ weather)
design_mat <- model.matrix(formula_anova, sales_weather)

# データ作成
# --- N：サンプルサイズ
# --- K：デザイン行列の列数
# --- Y：応答変数
# --- X：デザイン行列
data_list <-
  list(
  N = nrow(sales_weather),
  K = 3,
  Y = sales_weather$sales,
  X = design_mat)

# データ確認
data_list %>% print()


# 3 brmsを使わない分散分析モデルの推定 -----------------------------------------------------

# rstanで分散分析モデルを実行
anova_stan <-
  stan(file = "book/bayesian_model/stan/3-4-1-lm-design-matrix.stan",
       data = data_list,
       seed = 1)

# 結果確認
anova_stan %>% print(probs = c(0.025, 0.5, 0.975))

# 結果比較
anova_brms %>% print(probs = c(0.025, 0.5, 0.975))





