# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 6-11 統計モデリングの視点からの確率分布の紹介（正規分布）
# Date    : 2022/05/11
# Page    : P80 - P102
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 正規分布は平均と標準偏差をパラメータに持つ左右対称の釣鐘上の分布


# ＜活用シーン＞
# - 正規分布は世の中で遭遇するデータの多くに当てはめられている（生成プロセスが明確でない場合に使用することも多い）
#   --- 連続値で範囲が無限大(非負制約を持たない)のデータを想定している
#   --- 外れ値に弱いため予め予想できる場合はコーシー分布やStudentのt分布で代用する


# ＜目次＞
# 1 準備
# 2 プロット作成


# 1 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)


# データ作成
d_para <- data.frame(mu = c(0, 2), sigma = c(1, 0.5), gr = letters[1:2])
my_labs <- parse(text = sprintf('mu=="%.0f"~~sigma=="%.1f"', d_para$mu, d_para$sigma))


# 2 プロット作成 --------------------------------------------------------------------------

data.frame(X = c(-4, 4)) %>%
  ggplot(aes(x = X)) +
    mapply(
      function(mu, sigma, co) stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), aes_q(linetype = co)),
      d_para$mu, d_para$sigma, d_para$gr
    ) +
    scale_linetype_manual('parameter', values = c('solid', '52', '12'), labels = my_labs) +
    labs(x = 'y', y = 'density') +
    theme_bw(base_size = 18) +
    theme(legend.key.width = grid::unit(2.5, 'line'))
