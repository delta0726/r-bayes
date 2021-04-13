#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-8 ポアソン回帰モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/14
# Page      : P212 - P219
#***************************************************************************************


# ＜テーマ＞
# - 一般化線形モデルの応用としてポイアソン回帰モデルを確認する
#   --- 事前分布とリンク関数を変更することで対応


# ＜目次＞
# 0 準備
# 1 ポアソン回帰モデルの推定
# 2 brmsを用いない実装の方法
# 3 デザイン行列を使ったモデルの推定


# 0 準備 --------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データロード
fish_num_climate <- read_csv("book/bayesian_model/csv/3-8-1-fish-num-1.csv")

# データ確認
# --- fish_numは整数値
fish_num_climate %>% print()
fish_num_climate$fish_num %>% table()
fish_num_climate$weather %>% table()

# データの要約
fish_num_climate %>% summary()

# プロット作成
fish_num_climate %>%
  ggplot(mapping = aes(x = temperature, y = fish_num)) +
    geom_point(aes(color = weather)) +
    labs(title = "Relationship between fishing and weather")


# 1 ポアソン回帰モデルの推定 ----------------------------------------------------------

# ポアソン回帰モデルを作る
# --- family：ポアソン分布を使う
# --- prior ：無情報事前分布にする
glm_pois_brms <-
  brm(formula = fish_num ~ weather + temperature,
      family = poisson(),
      data = fish_num_climate,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# MCMCの結果の確認
glm_pois_brms %>% print()

# 参考
exp(-0.59)
exp(0.08)


# 2 ポアソン回帰の回帰曲線 ------------------------------------------------------------

# プロット作成
# --- 95%ベイズ信用区間付きのグラフ
glm_pois_brms %>%
  conditional_effects(effects = "temperature:weather") %>%
  plot(points = TRUE)


# プロット作成
# --- 99%ベイズ予測区間付きのグラフ
set.seed(1)
glm_pois_brms %>%
  conditional_effects(method = "predict",
                      effects = "temperature:weather",
                      prob = 0.99) %>%
  plot(points = TRUE)


# 2 brmsを用いない実装の方法 -----------------------------------------------------------


# デザイン行列の作成
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate)

# 確認
design_mat %>% as_tibble()

# Stanデータの作成
data_list_1 <-
  list(N = nrow(fish_num_climate),
       fish_num = fish_num_climate$fish_num,
       temp = fish_num_climate$temperature,
       sunny = as.numeric(design_mat[, "weathersunny"]))

# サンプリング
# --- 自分で変換処理を入れる
glm_pois_stan_exp <-
  stan(file = "book/bayesian_model/stan/3-8-1-glm-pois-1.stan",
       data = data_list_1,
       seed = 1)

# 結果確認
glm_pois_stan_exp %>% print(probs = c(0.025, 0.5, 0.975))


# サンプリング
# --- poisson_log関数を使用
glm_pois_stan <-
  stan(file = "book/bayesian_model/stan/3-8-2-glm-pois-2.stan",
       data = data_list_1,
       seed = 1)

# 結果確認
glm_pois_stan %>% print(probs = c(0.025, 0.5, 0.975))


# 3 デザイン行列を使ったモデルの推定 --------------------------------------------------------

# Stanデータの作成
data_list_2 <-
  list(N = nrow(fish_num_climate),
       K = 3,
       Y = fish_num_climate$fish_num,
       X = design_mat)

# 確認
data_list_2 %>% print()

# MCMCの実行
glm_pois_stan_design_mat <-
  stan(file = "book/bayesian_model/stan/3-8-3-glm-pois-design-matrix.stan",
       data = data_list_2,
       seed = 1)

# 参考：結果の表示
glm_pois_stan_design_mat %>% print(probs = c(0.025, 0.5, 0.975))



