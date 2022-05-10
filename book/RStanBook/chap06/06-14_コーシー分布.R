# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 6-14 統計モデリングの視点からの確率分布の紹介（コーシー分布）
# Date    : 2022/05/11
# Page    : P99 - P100
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - コーシー分布は正規分布と同様には平均と標準偏差をパラメータに持つ左右対称の釣鐘上の分布
#   --- 正規分布よりはるかに裾が広いため外れ値に対して頑健（モデリングに重宝する性質）


# ＜活用シーン＞
# - 外れ値を含むデータ（テストの点数など）
# - 変化点検出


# ＜目次＞
# 1 準備
# 2 プロット作成


# 1 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)

# データ作成
d_para <- data.frame(mu = c(0, 0), sigma = c(1, 3), gr = letters[1:2])


# 2 プロット作成 -------------------------------------------------------------------------

# ラベル作成
my_labs <- parse(text = sprintf('mu=="%.0f"~~sigma=="%.0f"', d_para$mu, d_para$sigma))

# プロット作成
ggplot(data.frame(X = c(-6, 6)), aes(x = X)) +
  mapply(
    function(mu, sigma, co) stat_function(fun = dcauchy, args = list(location = mu, scale = sigma), aes_q(linetype = co)),
    d_para$mu, d_para$sigma, d_para$gr
  ) +
  scale_linetype_manual('parameter', values = c('solid', '12'), labels = my_labs) +
  labs(x = 'y', y = 'density') +
  scale_x_continuous(limit = c(-6, 6)) +
  theme_bw(base_size = 18) +
  theme(legend.key.width = grid::unit(2.5, 'line'))
