# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 6-1 統計モデリングの視点からの確率分布の紹介（一様分布）
# Date    : 2022/05/08
# Page    : P81
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 無情報事前分布として使われる
#   --- Stanではパラメータの事前分布を明示的に設定しない場合、とりうる範囲で一様分布が自動的に設定される


# ＜目次＞
# 1 準備
# 2 プロット作成


# 1 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)

# データ作成
d <- tibble(X = c(-Inf, -2, 1), Xend = c(-2, 1, Inf), Y = c(0, 1 / 3, 0), Yend = c(0, 1 / 3, 0))
d %>% print()


# 2 プロット作成 --------------------------------------------------------------------------

ggplot() +
  geom_segment(data = d, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 2) +
  geom_point(data = data.frame(X = c(-2, 1), Y = c(0, 0)), aes(X, Y), size = 3, shape = 21, fill = 'white') +
  geom_point(data = data.frame(X = c(-2, 1), Y = c(1 / 3, 1 / 3)), aes(X, Y), size = 3, shape = 21, fill = 'black') +
  labs(x = 'y', y = 'density') +
  xlim(-3, 3) +
  theme_bw(base_size = 18)
