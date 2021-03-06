#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 2-2 データの要約
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/9
# Page      : P93 - P101
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 ヒストグラムの作成
# 2 カーネル密度推定
# 3 中央値とパーセント点
# 4 ピアソンの積率相関係数


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)


# データ取り込み
fish <- read_csv("csv/2-2-1-fish.csv")
birds <- read_csv("csv/2-1-1-birds.csv")

# データ概要
fish %>% glimpse()
birds %>% glimpse()

# データ確認
fish %>% print()
birds %>% print()


# 1 ヒストグラムの作成 ----------------------------------------------------------

# ＜ポイント＞
# - データを離散的な階級値で区切って頻度を計算したもの
# - データ分布の形状が大まかに分かる


# ヒストグラム
fish$length %>% hist()


# 2 カーネル密度推定 ------------------------------------------------------------

# ＜ポイント＞
# - データの位置を縦棒で示して各データ店を中心に正規分布の確率密度関数を描いて足し合わせたもの
#   --- データが密集しているところに高い密度が描かれる
#   --- ｢カーネル｣という言葉が使われているが、事後分布のカーネルとの関係はない


# カーネル密度推定
kernel_density <- fish$length %>% density()

# データ構造
kernel_density %>% glimpse()

# プロット作成
# --- 密度プロット
kernel_density %>% plot()


# バンド幅の変更 *****************************************

# カーネル密度推定
# --- adjust倍に変更
kernel_density_quarter <- fish$length %>% density(adjust = 0.25)
kernel_density_quadruple <- fish$length %>% density(adjust = 4)

# プロット作成
# --- 密度プロット
kernel_density %>%
  plot(lwd = 2,
       xlab = "",
       ylim = c(0, 0.26),
       main = "Change width of band")
lines(kernel_density_quarter, col = 2)
lines(kernel_density_quadruple, col = 4)

# 凡例を追加
legend("topleft",       # 凡例の位置
       col = c(1,2,4),  # 線の色
       lwd = 1,         # 線の太さ
       bty = "n",       # 凡例の囲み線を消す
       legend = c("Standard", "band=1/4", "band=*4"))


# 3 中央値とパーセント点 -------------------------------------------------------------

# 等差数列の作成
# --- 0-1000
suuretu <- 0:1000
suuretu %>% print()

# 長さの確認
suuretu %>% length()

# 中央値
# --- 関数
# --- 分位点
suuretu %>% median()
suuretu %>% quantile(probs = c(0.5))

# 四分位点
suuretu %>% quantile(probs = c(0.25, 0.75))

# 95%区間
suuretu %>% quantile(probs = c(0.025, 0.975))


# 4 ピアソンの積率相関係数 ---------------------------------------------------------

# 相関係数
# --- 体の大きさ vs 羽の大きさ
birds %$% cor(body_length, feather_length)


# 5 自己相関係数とコレログラム -----------------------------------------------------------

# ＜ポイント＞
# - 時系列データで過去データとの相関を取ったものを自己相関という
#   --- 正の自己相関：前回大きければ、今回も大きくなる
#   --- 負の自己相関：前回大きければ、今回は小さくなる


# データ確認
# --- ナイル川の流量データ
Nile

# 標本自己共分散
# --- 自己共分散を計算(デフォルトは自己相関)
Nile %>%
  acf(type = "covariance",
      plot = F,
      lag.max = 5)

# 標本自己相関
Nile %>%
  acf(plot = F,
      lag.max = 5)

# コレログラム
Nile %>% acf()
