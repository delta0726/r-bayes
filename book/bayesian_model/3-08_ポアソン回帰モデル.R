# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 8 ポアソン回帰モデル
# Date    : 2022/4/23
# Page    : P212 - P219
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 一般化線形モデルの応用としてポイアソン回帰モデルを確認する
#   --- 事前分布とリンク関数を変更することで対応
#   --- ポアソン分布は離散型の正の整数値を想定した分布（パラメータは強度を示すλのみ）


# ＜スマートな実装＞
# - brmsを用いると最初から最大限スマートな実装を行うことができる
# - rstanを用いる場合は線形予測子(フォーミュラ)をデザイン行列のままモデルに渡すのがポイント


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 ポアソン回帰モデルの推定
# 3 推定されたモデルの解釈
# 4 回帰曲線のプロット
# 5 rstanを用いた実装の方法


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
fish_num_climate <- read_csv("csv/3-8-1-fish-num-1.csv")


# 1 データ確認 --------------------------------------------------------------------

# ＜ポイント＞
# - 被説明変数のfish_numは釣魚数を示すのでゼロ以上の整数値


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
    facet_wrap(~weather, nrow = 2) +
    labs(title = "Relationship between fishing and weather")


# 2 ポアソン回帰モデルの推定 ----------------------------------------------------------

# ＜ポイント＞
# - ポアソン分布を想定するためfamilyをpoisson()に変更する（リング関数はlog）
# - 正規分布と異なりデータのばらつきの大きさを推定する必要がないため無情報事前分布を使う


# ポアソン回帰モデルを作る
# --- family：ポアソン分布を使う
# --- prior ：無情報事前分布にする
glm_pois_brms <-
  brm(formula = fish_num ~ weather + temperature,
      family = poisson(link = "log"),
      data = fish_num_climate,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# MCMCの結果の確認
glm_pois_brms %>% print()


# 3 推定されたモデルの解釈 ----------------------------------------------------------

# ＜ポイント＞
# - リンク関数に対数関数を用いているため、係数の解釈が変わる点に注意する


# 回帰係数の解釈（weathersunny）
# --- weathersunnyが-0.59となっている（天気が晴れになると釣魚数がexp(-0.59)倍になる）
# --- exp(-0.59)は約0.55なので、晴れになると曇りのときの0.55倍に減少することを意味する
exp(-0.59)

# 回帰係数の解釈（ temperature）
# --- temperatureが0.08となっている（気温が1度上がると釣魚数がexp(0.08)倍になる）
# --- exp(0.08)は約1.08なので、気温が1度上がると1.08倍に増加することを意味する
exp(0.08)


# 4 回帰曲線のプロット ------------------------------------------------------------

# 回帰曲線のプロット
# --- 95%ベイズ信用区間付きのグラフ
glm_pois_brms %>%
  conditional_effects(effects = "temperature:weather") %>%
  plot(points = TRUE)

# 予測区間のプロット
# --- 予測区間がキザキザしているのは乱数を使っているため
set.seed(1)
glm_pois_brms %>%
  conditional_effects(method = "predict",
                      effects = "temperature:weather",
                      prob = 0.99) %>%
  plot(points = TRUE)


# 5 rstanを用いた実装の方法 -----------------------------------------------------------

# ＜ポイント＞
# - パターン1⇒パターン2⇒パターン3の順に実装がスマートになっている
# - 線形予測子(フォーミュラ定義)にデザイン行列を使用するとコード変換なしに線形予測子の変更に対応できる
#   --- 確率分布とリンク関数はパラメータなので、デザイン行列を使用することでコードを定型化できる


# デザイン行列の作成
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate)

# 確認
design_mat %>% as_tibble()

# Stanデータの作成1
# --- 変数をリスト要素をごとに定義
# --- パターン1/パターン2で使用
data_list_1 <-
  list(N = nrow(fish_num_climate),
       fish_num = fish_num_climate$fish_num,
       temp = fish_num_climate$temperature,
       sunny = as.numeric(design_mat[, "weathersunny"]))

# Stanデータの作成
# --- デザイン行列をそのままインプット
# --- パターン3で使用
data_list_2 <-
  list(N = nrow(fish_num_climate),
       K = 3,
       Y = fish_num_climate$fish_num,
       X = design_mat)


# パターン1 ==================================

# モデル構築
# --- フォーミュラを対数変換して確率分布にpoissonを使用
glm_pois_stan_exp <-
  stan(file = "stan/3-8/3-8-1-glm-pois-1.stan",
       data = data_list_1,
       seed = 1)

# 確認
glm_pois_stan_exp %>% print(probs = c(0.025, 0.5, 0.975))


# パターン2 ==================================

# モデル構築2
# --- フォーミュラ変換はせずに確率分布にpoisson_logを使用
glm_pois_stan <-
  stan(file = "stan/3-8/3-8-2-glm-pois-2.stan",
       data = data_list_1,
       seed = 1)

# 確認
glm_pois_stan %>% print(probs = c(0.025, 0.5, 0.975))


# パターン3 ==================================

# MCMCの実行
# --- デザイン行列によるデータ定義に確率分布にpoisson_logを使用
glm_pois_stan_design_mat <-
  stan(file = "stan/3-8/3-8-3-glm-pois-design-matrix.stan",
       data = data_list_2,
       seed = 1)

# 確認
glm_pois_stan_design_mat %>% print(probs = c(0.025, 0.5, 0.975))
