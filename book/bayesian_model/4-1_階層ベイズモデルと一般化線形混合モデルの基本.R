#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 4-1 階層ベイズモデルと一般化線形混合モデルの基本
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P245 - P253
#***************************************************************************************


# ＜テーマ＞
# -


# ＜目次＞
# 0 準備
# 1 モデル構築
# 2 StanによるGLMMの推定



# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(bayesplot)
library(brms)

# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# 分析対象のデータ
fish_num_climate_2 <- read_csv("book/bayesian_model/csv/4-1-1-fish-num-2.csv")

# データ修正
# --- idをファクターに変換
fish_num_climate_2 <- fish_num_climate_2 %>% mutate(id = as.factor(id))
fish_num_climate_2 %>% print()


# 1 モデル構築 -----------------------------------------------------------------

# ポアソン回帰モデル
# --- family：ポアソン分布
# --- prior ：無情報事前分布
glm_pois_brms <-
  brm(formula = fish_num ~ weather + temperature,
      family = poisson(),
      data = fish_num_climate_2,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# 結果確認
glm_pois_brms %>% print()

# プロット作成
# --- 当てはめ値と99%予測区間の計算
set.seed(1)
glm_pois_brms %>%
  conditional_effects(method = "predict",
                      effects = "temperature:weather",
                      prob = 0.99) %>%
  plot(points = T)



# 2 StanによるGLMMの推定 --------------------------------------------------------

# ダミー変数を作る
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate_2)
sunny_dummy <- as.numeric(design_mat[, "weathersunny"])

# Stanデータの作成
data_list_1 <-
  list(N = nrow(fish_num_climate_2),
       fish_num = fish_num_climate_2$fish_num,
       temp = fish_num_climate_2$temperature,
       sunny = sunny_dummy)

# 結果確認
data_list_1 %>% print()

# MCMCの実行
glmm_pois_stan <-
  stan(file = "book/bayesian_model/stan/4-1-1-glmm-pois.stan",
       data = data_list_1,
       seed = 1)

# 収束の確認
glmm_pois_stan %>% rhat() %>% mcmc_rhat()

# プロット作成
# --- 事後分布とトレースプロット
glmm_pois_stan %>%
  rstan::extract(permuted = FALSE) %>%
  mcmc_combo(pars = c("Intercept", "b_sunny", "b_temp", "sigma_r", "lp__"))

# 結果の表示
glmm_pois_stan %>%
  print(pars = c("Intercept", "b_sunny", "b_temp", "sigma_r"),
        probs = c(0.025, 0.5, 0.975))


# brmsによるGLMMの推定 -------------------------------------------------------------

# brmsによるGLMMの推定
# --- ランダム効果：(1|id)
# --- family    ：ポアソン分布
# --- prior     ：無情報事前分布
glmm_pois_brms <-
  brm(formula = fish_num ~ weather + temperature + (1|id),
      family = poisson(),
      data = fish_num_climate_2,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sd")))

# 結果の表示
glmm_pois_brms %>% print()

# プロット作成
# --- 事後分布とトレースプロット
glmm_pois_brms %>% plot()

# コード出力
# --- stancode
glmm_pois_brms %>% stancode()


