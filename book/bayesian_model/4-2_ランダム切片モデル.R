# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 4 一般化線形混合モデル
# Theme   : 2 ランダム切片モデル
# Date    : 2022/4/23
# Page    : P254 - P259
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 前回は個体ごとにランダム効果を定義したが、グループごとにランダム効果を定義する


# ＜目次＞
# 0 準備
# 1 brmsによるGLMMの推定
# 2 brmsによるGLMMの推定
# 3 ランダム切片モデルの回帰曲線の図示


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)

# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データロード
fish_num_climate_3 <- read_csv("csv/4-2-1-fish-num-3.csv")


# 1 データ確認 ------------------------------------------------------------------

# ＜ポイント＞
# - グループ情報としてhumanが追加されている
#   --- 同一人物が別の時間に釣りをしたという想定する


# データ確認
fish_num_climate_3 %>% print()
fish_num_climate_3 %>% select(-temperature) %>% map(table)

# データ確認
# --- Aさんの釣果
fish_num_climate_3 %>% filter(human == "A")

# データ要約
fish_num_climate_3 %>% summary()


# 2 brmsによるGLMMの推定 -------------------------------------------------------------

# ＜ポイント＞
# - 釣果数が釣り人の能力に依存することを想定してhumanをグループに設定する
#- あくまで天気や気温が釣果数に与える影響を調べるのが目的（個人の能力評価ではない）
#  --- 個人の能力評価であればhumanをダミー変数にして固定効果として評価したほうが適切


# brmsによるGLMMの推定
# --- 切片がグループごとに異なるモデル（humanごとに釣果の最低水準が異なることを想定）
# --- 固定効果：weather, temperature   ランダム効果 ：human
glmm_pois_brms_human <-
  brm(formula = fish_num ~ weather + temperature + (1|human),
      family = poisson(),
      data = fish_num_climate_3,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sd")))

# 収束の確認
# --- 混合モデルなど複雑なモデルになると収束確認の重要性が増す
glmm_pois_brms_human %>% stanplot(type = "rhat")

# 結果の表示
# --- ランダム効果のバラツキ(sd)は0.65と推定
glmm_pois_brms_human %>% print()

# プロット作成
# --- 事後分布とトレースプロット
glmm_pois_brms_human %>% plot()

# 各々の調査者の影響の大きさ
glmm_pois_brms_human %>% ranef()



# 3 ランダム切片モデルの回帰曲線の図示 ----------------------------------------------------

# ＜ポイント＞
# - グループごとにモデルが定義されるためプロットもモデルごとに出力される


# 調査者ごとにグラフを分けて、回帰曲線を描く
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

# プロット作成
# --- グループごとにプロットが作成される
glmm_pois_brms_human %>%
  conditional_effects(effects = "temperature:weather",
                      re_formula = NULL,
                      conditions = conditions) %>%
  plot(points = TRUE)
