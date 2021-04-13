#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 4-2 ランダム切片モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P254 - P259
#***************************************************************************************


# ＜テーマ＞
# -


# ＜目次＞
# 0 準備
# 1 brmsによるGLMMの推定
# 2 ランダム切片モデルの回帰曲線の図示


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
fish_num_climate_3 <- read_csv("book/bayesian_model/csv/4-2-1-fish-num-3.csv")

# データ確認
fish_num_climate_3 %>% print()
fish_num_climate_3 %>% map(table)

# データ要約
fish_num_climate_3 %>% summary()


# 1 brmsによるGLMMの推定 -------------------------------------------------------------

# brmsによるGLMMの推定
glmm_pois_brms_human <-
  brm(formula = fish_num ~ weather + temperature + (1|human),
      family = poisson(),
      data = fish_num_climate_3,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sd")))

# 収束の確認
glmm_pois_brms_human %>% stanplot(type = "rhat")

# 結果の表示
glmm_pois_brms_human %>% print()

# プロット作成
# --- 事後分布とトレースプロット
glmm_pois_brms_human %>% plot()

# 各々の調査者の影響の大きさ
glmm_pois_brms_human %>% ranef()



# 2 ランダム切片モデルの回帰曲線の図示 ----------------------------------------------------

# 調査者ごとにグラフを分けて、回帰曲線を描く
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

# プロット作成
glmm_pois_brms_human %>%
  marginal_effects(effects = "temperature:weather",
                   re_formula = NULL,
                   conditions = conditions) %>%
  plot(points = TRUE)
