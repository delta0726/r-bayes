# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 5 状態空間モデル
# Theme   : 2-1 ホワイトノイズとランダムウォーク
# Date    : 2022/4/26
# Page    : P275 - P288
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 時系列モデルの前提知識であるホワイトノイズとランダムウォークを確認する


# ＜目次＞
# 0 準備
# 1 ホワイトノイズとランダムウォーク
# 2 ランダムウォークのシミュレーション


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# 1 ホワイトノイズとランダムウォーク -------------------------------------------------------

# ＜ポイント＞
# - ホワイトノイズは正規乱数を時系列方向で見ることにより定義される（iid系列）
#   --- ｢期待値がゼロ｣｢分散が一定｣｢自己相関がゼロ｣という特徴を持つ
#   --- ランダムウォークはiid系列を累和として定義される


# 正規ホワイトノイズ
set.seed(1)
wn <- rnorm(n = 100, mean = 0, sd = 1)

# ランダムウォーク
rw <- wn %>% cumsum()

# グラフを作る
p_wn_1 <- wn %>% ts() %>% autoplot(main = "ホワイトノイズ")
p_rw_1 <- rw %>% ts() %>% autoplot(main = "ランダムウォーク")

# 2つのグラフをまとめる
# --- 累和が上昇トレンドなのは偶然
grid.arrange(p_wn_1, p_rw_1)


# 2 ランダムウォークのシミュレーション ------------------------------------------------------

# ＜ポイント＞
# - 複数パターンをシミュレーションで作成すると上昇/下落の様々なパターンが生成される
# - 正規分布のようにゼロ付近の系列が多くなる
#   ---ホワイトノイズは平均値が一定なので平均回帰により長期的にはゼロ付近に集まる


# オブジェクト生成
# --- wn_mat：正規ホワイトノイズ用
# --- rw_mat：ランダムウォーク用
wn_mat <- matrix(nrow = 100, ncol = 20)
rw_mat <- matrix(nrow = 100, ncol = 20)

# データ作成
# --- 列単位で20系列を作成
set.seed(1)
for(i in 1:20){
  wn <- rnorm(n = 100, mean = 0, sd = 1)
  wn_mat[,i] <- wn
  rw_mat[,i] <- cumsum(wn)
}

# プロット作成
p_wn_2 <-
  wn_mat %>%
    ts() %>%
    autoplot( facets = F, main = "ホワイトノイズ") +
    theme(legend.position = 'none')

p_rw_2 <-
  rw_mat %>%
    ts() %>%
    autoplot(facets = F, main = "ランダムウォーク") +
    theme(legend.position = 'none') # 凡例を消す

# プロット表示
grid.arrange(p_wn_2, p_rw_2)
