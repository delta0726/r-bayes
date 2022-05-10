# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 6-12 統計モデリングの視点からの確率分布の紹介（対数正規分布）
# Date    : 2022/05/11
# Page    : P96
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 対数正規分布は正規分布を対数変換したことによって得られる分布
#   --- 正規分布と同様に平均と標準偏差をパラメータとする


# ＜活用シーン＞
# - 確率変数が正の値で右裾が長いデータを想定している
# --- 人間の体重、年収、企業の時価総額
# --- 指数分布やガンマ分布より裾が長く、コーシー分布より裾が短い


# ＜目次＞
# 1 準備
# 2 プロット作成


# 1 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)


# データ作成
d_para <- data.frame(mu = c(0, 0, 2), sigma = c(1, 0.4, 0.4), gr = letters[1:3])


# 2 プロット作成 --------------------------------------------------------------------------

# ラベル定義
my_labs <- parse(text = sprintf('mu=="%.0f"~~sigma=="%.1f"', d_para$mu, d_para$sigma))

# プロット作成
ggplot(data.frame(X = c(0, 10)), aes(x = X)) +
  mapply(
    function(mu, sigma, co) stat_function(fun = dlnorm, args = list(meanlog = mu, sdlog = sigma), aes_q(linetype = co)),
    d_para$mu, d_para$sigma, d_para$gr
  ) +
  scale_linetype_manual('parameter', values = c('solid', '52', '12'), labels = my_labs) +
  labs(x = 'y', y = 'density') +
  theme_bw(base_size = 18) +
  theme(legend.key.width = grid::unit(2.5, 'line'))


