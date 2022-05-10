# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 6-16 統計モデリングの視点からの確率分布の紹介（ラプラス分布）
# Date    : 2022/05/11
# Page    : P101 - P102
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 平均を中心に指数分布を左右に貼り合わせたような分布
# - 指数分布と同様に正規分布と比べると確率密度が小さくなるスピードがかなり遅く裾の長い分布となる


# ＜活用シーン＞
# - 変数選択を想定した回帰係数の事前分布
# --- 平均を中心に鋭いピークがあるので左右に振れるハードルが高くなる


# ＜目次＞
# 1 準備
# 2 プロット作成


# 1 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(extraDistr)

# データ作成
d_para <- data.frame(mu = c(0, 0), sigma = c(1, 3), gr = letters[1:2])


# 2 プロット作成 -------------------------------------------------------------------------

# ラベル作成
my_labs <- parse(text = sprintf('mu=="%.0f"~~sigma=="%.0f"', d_para$mu, d_para$sigma))

# プロット作成
ggplot(data.frame(X = c(-6, 6)), aes(x = X)) +
  mapply(
    function(mu, sigma, co) stat_function(fun = dlaplace, args = list(mu = mu, sigma = sigma), aes_q(linetype = co)),
    d_para$mu, d_para$sigma, d_para$gr
  ) +
  scale_linetype_manual('parameter', values = c('solid', '12'), labels = my_labs) +
  labs(x = 'y', y = 'density') +
  scale_x_continuous(limit = c(-6, 6)) +
  theme_bw(base_size = 18) +
  theme(legend.key.width = grid::unit(2.5, 'line'))
