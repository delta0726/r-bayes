#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 4-3 ランダム係数モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P260 - P266
#***************************************************************************************


# ＜テーマ＞
# -


# ＜目次＞
# 0 準備




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
fish_num_climate_4 <- read.csv("book/bayesian_model/csv/4-3-1-fish-num-4.csv")
# データ確認
fish_num_climate_4 %>% print()
fish_num_climate_4 %>% map(table)

# データ要約
fish_num_climate_4 %>% summary()


# 1 交互作用を用いたモデル化 -------------------------------------------------------

# 交互作用を組み込んだポアソン回帰モデル
glm_pois_brms_interaction <-
  brm(formula = fish_num ~ temperature * human,
      family = poisson(),
      data = fish_num_climate_4,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# 推定結果
glm_pois_brms_interaction %>% print()

# 参考：収束の確認
glm_pois_brms_interaction %>% stanplot(type = "rhat")


# 回帰曲線を描く
# データの分割
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

# 図示
glm_pois_brms_interaction %>%
  marginal_effects(effects = "temperature",
                   conditions = conditions) %>%
  plot(points = TRUE)


# 2 brmsによるランダム係数モデルの推定 --------------------------------------------------------

# ランダム係数モデル
glmm_pois_brms_keisu <-
  brm(formula = fish_num ~ temperature + (temperature||human),
      family = poisson(),
      data = fish_num_climate_4,
      seed = 1,
      iter = 6000,
      warmup = 5000,
      control = list(adapt_delta = 0.97, max_treedepth = 15))


# 推定結果
glmm_pois_brms_keisu %>% print()

# 参考：トレースプロットなど
glmm_pois_brms_keisu %>% plot()

# 参考：弱情報事前分布
glmm_pois_brms_keisu %>% prior_summary()

# 参考：収束の確認
glmm_pois_brms_keisu %>% stanplot(type = "rhat")

# 回帰曲線を描く
# データの分割
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

# 図示
glmm_pois_brms_keisu %>%
  marginal_effects(re_formula = NULL,
                   effects = "temperature",
                   conditions = conditions) %>%
  plot(points = TRUE)



