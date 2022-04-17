# ***********************************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 2 RとStanによるデータ分析
# Theme   : 3 ggplot2によるデータの可視化
# Date    : 2022/4/17
# Page    : P102 - P110
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# ***********************************************************************************************


# ＜概要＞
# - ggplot2を用いたデータの可視化について確認する


# ＜目次＞
# 0 準備
# 1 ヒストグラムとカーネル密度推定
# 2 グラフの重ね合わせと一覧表示
# 3 箱ひげ図とバイオリンプロット
# 4 散布図
# 5 折れ線グラフ


# 0 準備 ---------------------------------------------------------------------

# ライブラリの読み込み
library(tidyverse)
library(magrittr)
library(gridExtra)
library(ggfortify)


# データ取り込み
fish <- read_csv("csv/2-2-1-fish.csv")
birds <- read_csv("csv/2-1-1-birds.csv")

# データ概要
fish %>% glimpse()
birds %>% glimpse()

# データ確認
fish %>% print()
birds %>% print()


# 1 ヒストグラムとカーネル密度推定 -------------------------------------------------

# ヒストグラム
fish %>%
  ggplot(mapping = aes(x = length)) +
    geom_histogram(alpha = 0.5, bins = 20) +
    labs(title = "Histogram")

# カーネル密度推定
fish %>%
  ggplot(mapping = aes(x = length)) +
    geom_density(size = 1.5) +
    labs(title = "Kernel Density Plot")


# 2 グラフの重ね合わせと一覧表示 ----------------------------------------------------

# グラフの重ね合わせ
fish %>%
  ggplot(mapping = aes(x = length, y = ..density..)) +
    geom_histogram(alpha = 0.5, bins = 20) +
    geom_density(size = 1.5) +
    labs(title = "Histogram + Density")

# ヒストグラム
p_hist <-
  fish %>%
    ggplot(mapping = aes(x = length)) +
    geom_histogram(alpha = 0.5, bins = 20) +
    labs(title = "ヒストグラム")

# カーネル密度推定
p_density <-
  fish %>%
    ggplot(mapping = aes(x = length)) +
    geom_density(size = 1.5) +
    labs(title = "カーネル密度推定")

# グラフを並べて表示
grid.arrange(p_hist, p_density, ncol = 2)


# 3 箱ひげ図とバイオリンプロット --------------------------------------------------

# データ確認
# --- irisデータ
iris %>% head(n = 3)

# 箱ひげ図
p_box <-
  iris %>%
    ggplot(mapping = aes(x = Species, y = Petal.Length)) +
    geom_boxplot() +
    labs(title = "Box Plot")

# バイオリンプロット
p_violin <-
  iris %>%
    ggplot(mapping = aes(x = Species, y = Petal.Length)) +
    geom_violin() +
    labs(title = "Violin Plot")

# グラフの表示
grid.arrange(p_box, p_violin, ncol = 2)


# 4 散布図 ---------------------------------------------------------------------

# 散布図
iris %>%
  ggplot(aes(x = Petal.Width, y = Petal.Length)) +
  geom_point()

# 色分けした散布図
iris %>%
  ggplot(aes(x=Petal.Width, y=Petal.Length, color=Species)) +
  geom_point() 


# 5 折れ線グラフ ------------------------------------------------------------------

# ナイル川流量データ
# --- tsクラス
Nile

# データフレームに変換
nile_data_frame <-
  data.frame(year = 1871:1970,
             Nile = as.numeric(Nile))

# データ確認
nile_data_frame %>% head(n = 3)

# 折れ線グラフ
nile_data_frame %>%
  ggplot(aes(x = year, y = Nile)) +
  geom_line()


# tsオブジェクトを楽に描画する方法
Nile %>% autoplot()
