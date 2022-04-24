# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 4 一般化線形混合モデル
# Theme   : 1 階層ベイズモデルと一般化線形混合モデルの基本
# Date    : 2022/4/23
# Page    : P245 - P253
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 階層ベイズモデルの具体例として過分散が生じているデータに対する一般化線形混合モデルを推定する
#   --- 階層ベイズモデルは階層構造を持つモデル
#   --- 上位層の確率変数の実現値が階層の確率分布の母数(確率分布のパラメータ)となる


# ＜用語整理＞
# - 一般化線形混合モデル(GLMM)とは一般化線形モデルにランダム効果を加えて混合モデルにしたもの
#   --- 混合モデルとは｢固定効果｣と｢ランダム効果｣の両方を使うモデル
#   --- 固定効果とはパラメータが定数であるモデルで定義される効果（通常の回帰分析）
#   --- ランダム効果とはパラメータが平均ゼロの同時正規分布に基づいてランダムに変化する効果


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 ポアソン回帰モデル構築
# 3 StanによるGLMMの推定
# 4 brmsによるGLMMの推定
# 5 正規線形モデルを拡張する場合の注意


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
fish_num_climate_2 <- read_csv("csv/4-1-1-fish-num-2.csv")


# 1 データ加工 ------------------------------------------------------------------

# ＜ポイント＞
# - 階層モデルでは個体情報を使用するのでidをファクターに変換する
# - 今回の釣獲尾数データは｢天気｣と｢気温｣のみのデータしかない
#   --- 釣獲尾数には｢経験値｣｢道具｣などの要素も影響しそうだが観測データに含まれない
#   --- 本章では｢観測されていないモノが理由で釣獲尾数が変化する｣ことを想定する


# データ修正
# --- idをファクターに変換
fish_num_climate_2 <- fish_num_climate_2 %>% mutate(id = as.factor(id))

# データ確認
fish_num_climate_2 %>% print()
fish_num_climate_2 %>% glimpse()

# プロット作成
fish_num_climate_2 %>%
  ggplot(aes(x = temperature, y = fish_num, color = weather)) +
    geom_point() +
    facet_wrap(~weather, nrow = 1)


# 2 ポアソン回帰モデル構築 ----------------------------------------------------------

# ＜ポイント＞
# - ｢観測されていないモノが理由で釣獲尾数が変化する状況｣ではポアソン分布は適切ではない
#   --- そこでGLMM(一般化線形混合モデル)を使うことを検討する
#   --- ここではポアソン回帰モデルを強引に適用する（発生する問題を確認する）


# モデル構築
# --- family：ポアソン分布
# --- prior ：無情報事前分布
glm_pois_brms <-
  brm(formula = fish_num ~ weather + temperature,
      family = poisson(),
      data = fish_num_climate_2,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# 確認
glm_pois_brms %>% print()

# プロット作成
# --- 予測区間(99%)よりも外側に多くのデータが位置している
# --- ポアソン分布は期待値と分散を1つのパラメータ(λ)で表現している（未知の要素が分散に影響している）
set.seed(1)
glm_pois_brms %>%
  conditional_effects(method = "predict",
                      effects = "temperature:weather",
                      prob = 0.99) %>%
  plot(points = T)


# 3 StanによるGLMMの推定 --------------------------------------------------------

# ＜ポイント＞
# - stanファイルのフォーミュラ等にランダム効果を追加する
#   --- サンプルごとにrとlambdaのパラメータが推定される


# デザイン行列の定義
# --- ダミー変数を取得するために使用
design_mat <-
  formula(fish_num ~ weather + temperature) %>%
    model.matrix(fish_num_climate_2)

# 確認
design_mat %>% head()

# ダミー変数の取得
sunny_dummy <- design_mat[, "weathersunny"] %>% as.numeric()


# Stanデータの作成
data_list_1 <-
  list(N = nrow(fish_num_climate_2),
       fish_num = fish_num_climate_2$fish_num,
       temp = fish_num_climate_2$temperature,
       sunny = sunny_dummy)

# MCMCの実行
glmm_pois_stan <-
  stan(file = "stan/4-1/4-1-1-glmm-pois.stan",
       data = data_list_1,
       seed = 1)

# 収束の確認
# --- サンプルごとのパラメータが出力されるのでrハットの一覧のみを表示する
# --- 収束には問題ない
glmm_pois_stan %>% rhat() %>% mcmc_rhat()

# プロット作成
# --- 事後分布とトレースプロット
glmm_pois_stan %>%
  rstan::extract(permuted = FALSE) %>%
  mcmc_combo(pars = c("Intercept", "b_sunny", "b_temp", "sigma_r", "lp__"))

# 結果の表示
# --- パラメータが非常に多いので表示対象を限定して出力
glmm_pois_stan %>%
  print(pars = c("Intercept", "b_sunny", "b_temp", "sigma_r"),
        probs = c(0.025, 0.5, 0.975))


# 4 brmsによるGLMMの推定 -------------------------------------------------------------

# ＜ポイント＞
# - brmsではフォーミュラでランダム効果を表現する
#   --- (1|id)：左側の1は切片、右側はグループ名
#   --- 個別サンプルのidごとにランダム効果の切片が加わることを意味する
#   --- この記法は他のＲライブラリでも共通


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
# --- Group-Level Effects: ランダム効果のパラメータ（今回はidという個体情報がグループ）
# --- Population-Level Effects: 固定効果のパラメータ
glmm_pois_brms %>% print()

# プロット作成
# --- 事後分布とトレースプロット
glmm_pois_brms %>% plot()

# コード出力
# --- stancode
glmm_pois_brms %>% stancode()


# 5 正規線形モデルを拡張する場合の注意 --------------------------------------------------------

# ＜ポイント＞
# - ポアソン分布は期待値と分散を1つのパラメータ(λ)で表現するという性質から過分散が置きやすい
#   --- 過分散とは期待値と比較して分散が大きくなる現象
#   --- 過分散の対策としてポアソン回帰モデルにランダム効果を入れる方法は機能しやすい傾向になる

# - 正規分布はパラメータが期待値(μ)と分散(σ^2)の2つで表現される
#   --- ポアソン分布のような｢期待値と比較して分散が大きくなる｣ことを考慮しなくてもよい
#   --- 正規線形モデルに過分散の対応としてランダム効果を導入する必要はない（別の理由で導入するケースがある）
