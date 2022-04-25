# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 4 一般化線形混合モデル
# Theme   : 3 ランダム係数モデル
# Date    : 2022/4/26
# Page    : P260 - P266
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - ランダム係数モデルはランダム効果が加わることで他の説明変数の固定効果の強さが増減する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 交互作用を用いた一般化線モデル
# 3 ランダム係数モデル


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
fish_num_climate_4 <- read.csv("csv/4-3-1-fish-num-4.csv")


# 1 データ確認 ----------------------------------------------------------------------

# ＜ポイント＞
# - グループ情報としてhumanが追加されている
#   --- 同一人物が別の時間に釣りをしたという想定する
#   --- サンプル数が94しかない点に注意（Jさんのみ4回）


# データ確認
fish_num_climate_4 %>% as_tibble()
fish_num_climate_4 %>% map(table)

# データ要約
fish_num_climate_4 %>% summary()


# 2 交互作用を用いた一般化線モデル -----------------------------------------------------

# ＜ポイント＞
# - ポアソン回帰モデルに交互作用を用いることで適切なモデリングが可能
#   --- humanをダミー変数として固定効果で処理する（ランダム効果は使用しない）
#   --- Jさん(10)のみ回帰曲線の方向が逆になっている点に注意


# モデル構築
# --- 交互作用を組み込んだポアソン回帰モデル
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


# データ作成
# --- プロット用
conditions <- tibble(human = c("A","B","C","D","E","F","G","H","I","J"))

# プロット作成
# --- Jさん(10)のみ回帰曲線の方向が逆になっている
# --- 気温が低いときにたまたまたくさん釣れたため（ダミー変数により個人の効果は適切に表現している）
glm_pois_brms_interaction %>%
  conditional_effects(effects = "temperature",
                      conditions = conditions) %>%
  plot(points = TRUE)


# 3 ランダム係数モデル ---------------------------------------------------------------

# ＜ポイント＞
# - ランダム効果を用いて上記のモデリングを行う
#   --- 固定効果はtemeratureのみなので全体として似たような傾向を示すようになる
#   --- 全体から説得力を借用している（縮約）
#   --- 分析対象により縮約を利用するのが適切でないケースもある（固定効果として扱うべきもの）


# モデル構築
# --- ランダム係数モデル
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

# 確認
# --- トレースプロットなど
# --- 弱情報事前分布
# --- 収束の確認
glmm_pois_brms_keisu %>% plot()
glmm_pois_brms_keisu %>% prior_summary()
glmm_pois_brms_keisu %>% stanplot(type = "rhat")

# データ作成
# --- プロット用
conditions <- tibble(human = c("A","B","C","D","E","F","G","H","I","J"))

# プロット作成
# --- Jさん(10)も他と同じ傾向を示すようになった
glmm_pois_brms_keisu %>%
  marginal_effects(re_formula = NULL,
                   effects = "temperature",
                   conditions = conditions) %>%
  plot(points = TRUE)
