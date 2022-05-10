# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 6-15 統計モデリングの視点からの確率分布の紹介（Studentのt分布）
# Date    : 2022/05/11
# Page    : P100
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - t分布は平均と標準偏差に加えて自由度のパラメータを持つ
# --- 自由度1のt分布はコーシー分布と一致し、自由度無限大のt分布は正規分布と一致する
# --- 自由度2-8程度のt分布が重宝される


# ＜活用シーン＞
# - 外れ値を含むモデルに使用される（自由度に応じて分布を変更ので使い勝手が良い）
# - 回帰係数の弱情報事前分布として使われることもある


# ＜目次＞
# 1 準備
# 2 プロット作成


# 1 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)

# データ作成
d_para <- data.frame(nu = c(1, 4), mu = c(0, 0), sigma = c(1, 1), gr = letters[1:2])


# 2 プロット作成 --------------------------------------------------------------------------

# ラベル作成
my_labs <- parse(text = c(
  sprintf('nu=="%d"~~mu=="%.0f"~~sigma=="%.0f"', d_para$nu, d_para$mu, d_para$sigma),
  'nu==infinity~~mu==0~~sigma==1')
)

# プロット作成
ggplot(data.frame(X = c(-6, 6)), aes(x = X)) +
  theme_bw(base_size = 18) +
  theme(legend.key.width = grid::unit(2.5, 'line')) +
  mapply(
    function(nu, co) stat_function(fun = dt, args = list(df = nu), aes_q(linetype = co)),
    d_para$nu, d_para$gr
  ) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), aes_q(linetype = 'C')) +
  scale_linetype_manual('parameter', values = c('52', 'solid', '12'), labels = my_labs) +
  labs(x = 'y', y = 'density') +
  scale_x_continuous(limit = c(-6, 6))
