# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 7 回帰分析の悩みどころ（交絡）
# Date    : 2022/05/10
# Page    : P112 - P113
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 交絡とはモデルの外側に応答変数と説明変数の両方に影響を与える変数が存在することがある状況
#   --- 交絡変数を考慮しないモデルでも予測分布の算出は可能
#   --- データに対する背景知識を得た時点でモデルに交絡変数を反映するのが望ましい（カテゴリカル変数として導入するなど）


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 モデル構築


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(gridExtra)
library(magrittr)
library(rstan)


# データロード
d <- read_csv(file = 'chap07/csv/data-50m.txt')


# 1 データ確認 ------------------------------------------------------------------

# ＜ポイント＞
# - 50m走における速度(Y)と体重(X)の関係を考える
#   --- 交絡変数として年齢(Z)が存在する


# データ変換
d_plot <- d %>% mutate(Age = as.factor(Age))

# プロット作成
# --- 性別情報なし
p1 <-
  d_plot %>%
    ggplot(aes(x=Weight, y=Y)) +
    geom_point(shape=1, size=2) +
    theme_bw(base_size=18)

# プロット作成
# --- 性別情報あり
p2 <-
  d_plot %>%
    ggplot(aes(x=Weight, y=Y, shape=Age)) +
    geom_point(size=2) +
    scale_shape_manual(values=c(3, 1, 2, 4, 5, 6)) +
    theme_bw(base_size=18)

# プロット比較
grid.arrange(p1, p2, nrow = 1)


# 2 モデル構築 --------------------------------------------------------------------

# Stan用データの作成
data <- list(N = nrow(d), Age = d$Age, Weight = d$Weight, Y = d$Y)

# 学習
fit <- stan(file = 'chap07/stan/model7-5.stan', data = data, seed = 1234)

# 確認
fit %>% print()
