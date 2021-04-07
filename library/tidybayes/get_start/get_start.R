# ***********************************************************************************************
# Library   : tidybayes
# Title     : Using tidy data with Bayesian models
# Created by: Owner
# Created on: 2021/04/08
# URL       : https://mjskay.github.io/tidybayes/articles/tidybayes.html
# ***********************************************************************************************


# ＜概要＞
# - {tidybayes}はAGSやStanなどの汎用モデリング機能で整頓されたデータを操作することを目的とする
# - デル出力を視覚化するためにggdistを使用（tidybayesの姉妹パッケージ）


# ＜目次＞
# 0 準備
# 1 Stanの取り込み
# 2 モデル用のデータ準備
# 3 サンプリングの実行
# 4 データ構造とデータ抽出
# 5 データ要約と信頼区間の算出
# 6 統計値と信頼区間のプロット
# 7 複数の信頼区間の指定
# 8 その他の要約統計量
# 9 要約統計量の加工
# 10 事後分布の予測
# 11 平均の事後分布による事後予測
# 12 不確実性のあるフィットカーブの作成
# 13 他のライブラリとの互換性
# 13-3 MCMCglmm


# 0 準備  ----------------------------------------------------------------

# ライブラリ
library(magrittr)
library(tidyverse)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(emmeans)
library(broom)
library(rstan)
library(rstanarm)
library(brms)
library(bayesplot)
library(MCMCglmm)
library(RColorBrewer)

theme_set(theme_tidybayes() + panel_border())


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# パラメータ設定
n = 10
n_condition = 5

# データ作成
set.seed(5)
ABC <-
  tibble(condition = rep(c("A", "B", "C", "D", "E"), n),
         response = rnorm(n * 5, c(0, 1, 2, 1, -1), sd = 0.5))

# データ確認
ABC %>% print()
ABC %>% glimpse()

# プロット作成
ABC %>%
  ggplot(aes(x = response, y = fct_rev(condition))) +
  geom_point(alpha = 0.5) +
  ylab("condition")


# 1 Stanの取り込み ----------------------------------------------------------------

# モデルロード
# --- ファイル読み込み
ABC_stan <- stan_model(file = "library/tidybayes/get_start/stan/model.stan")


# # 文字列指定
# stancode <- 'data {
#   int<lower=1> n;
#   int<lower=1> n_condition;
#   int<lower=1, upper=n_condition> condition[n];
#   real response[n];
# }
# parameters {
#   real overall_mean;
#   vector[n_condition] condition_zoffset;
#   real<lower=0> response_sd;
#   real<lower=0> condition_mean_sd;
# }
# transformed parameters {
#   vector[n_condition] condition_mean;
#   condition_mean = overall_mean + condition_zoffset * condition_mean_sd;
# }
# model {
#   response_sd ~ cauchy(0, 1);         // => half-cauchy(0, 1)
#   condition_mean_sd ~ cauchy(0, 1);   // => half-cauchy(0, 1)
#   overall_mean ~ normal(0, 5);
#   condition_zoffset ~ normal(0, 1);   // => condition_mean ~ normal(overall_mean, condition_mean_sd)
#   for (i in 1:n) {
#     response[i] ~ normal(condition_mean[condition[i]], response_sd);
#   }
# }'
#
# # モデルロード
# # --- 文字列読み込み
# ABC_stan <- stan_model(model_code = stancode, verbose = TRUE)


# 2 モデル用のデータ準備 ---------------------------------------------------------

# ＜ポイント＞
# - データフレームからJAGSやStanなどのサンプラーで使用可能な形式にデータを生成する
#   --- compose_data()はそれらの操作を自動化する

# ＜出力項目＞
# - condition   ：各観測の条件を示す整数のベクトル
# - n_condition ：条件の数
# - response:   ：観測のベクトル
# - n           ：観測数


# 元データ確認
ABC %>% print()
ABC$condition %>% table()

# データ変換
# --- conditionが数字に変換されている
ABC %>% compose_data()


# 3 サンプリングの実行 ---------------------------------------------------------

# サンプリング
m <-
  ABC_stan %>%
    sampling(data = compose_data(ABC),
             control = list(adapt_delta = 0.99))

# データ確認
m %>% print()
m %>% names()
m %>% glimpse()

# データ確認
# --- 項目選択
m %>%
  print(pars = c("overall_mean", "condition_mean_sd",
                 "condition_mean", "response_sd"))


# 4 データ構造とデータ抽出 ---------------------------------------------------------

# データ構造
m %>% rstan::extract() %>% glimpse()

# データ抽出
m %>%
  spread_draws(condition_mean[condition])

# データ抽出
# --- 元データのカテゴリ情報を再付与
m %>%
  recover_types(ABC) %>%
  spread_draws(condition_mean[condition])


# 5 データ要約と信頼区間の算出 ------------------------------------------------------

# データ抽出
# --- overall_mean
# --- response_sd
m %>%
  spread_draws(overall_mean, response_sd)

# 信頼区間の算出
# --- 変数の中央値と95％の信頼区間
m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi(overall_mean, response_sd)

m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi(overall_mean, response_sd) %>%
  glimpse()

# 信頼区間の算出
# --- 簡易表示
m %>%
  spread_draws(overall_mean, response_sd) %>%
  median_qi()

# 信頼区間の算出
# --- カテゴリごと
# --- グループを引数で指定
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi()

# 信頼区間の算出
# --- カテゴリごと
# --- dplyr::group_by()を使用
m %>%
  spread_draws(condition_mean[condition]) %>%
  group_by(condition) %>%
  median_qi(condition_mean)


# 6 統計値と信頼区間のプロット ------------------------------------------------------

# ＜ポイント＞
# 中央値と間隔のプロットにはggdist::geom_pointinterval()を使用
# --- ggplot2::geom_pointrange()に似ているが、複数の間隔に適切なデフォルトが設定
# --- 集計値でなく元データに対してもプロット可能

# プロット作成
# --- 事前に集計した統計値を使用
# --- 中央値 + 信頼区間
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi() %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval()

# プロット作成
# --- 元データを集計してプロット
# --- 標準偏差の66％と95％を出力
m %>%
  spread_draws(condition_mean[condition]) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_pointinterval()

# プロット作成
# --- 分布をバイオリンプロットで表示
m %>%
  spread_draws(condition_mean[condition]) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_eye()

# プロット作成
# --- 分布で表示
m %>%
  spread_draws(condition_mean[condition]) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean)) +
  stat_halfeye(.width = c(.90, .5))

# プロット作成
# --- 全体で範囲を指定して色付け
m %>%
  spread_draws(condition_mean[condition]) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean, fill = stat(abs(x) < .8))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))


# 7 複数の信頼区間の指定 ------------------------------------------------------

# 信頼区間を複数指定
# --- 5level * 3Interval
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .8, .5))

# プロット作成
# --- 複数の信頼区間
# --- 太さを.width引数でコントロール
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .66)) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean,
             xmin = .lower, xmax = .upper, size = -.width)) +
  geom_pointinterval()

# プロット作成
# --- 複数の信頼区間（上記と同じ図）
# --- geom_pointinterval()はsize = -.widthを初期値として持っている
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .66)) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval()

# プロット作成
# --- 信頼区間を増やしてもグラフの太さで表現される
m %>%
  spread_draws(condition_mean[condition]) %>%
  median_qi(.width = c(.95, .8, .5)) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = condition_mean,
             xmin = .lower, xmax = .upper)) +
  geom_pointinterval(interval_size_range = c(0.5, 2))

# ドットプロット作成
# --- 密度プロットは見やすいが厳密性が薄い
# --- 正確に分布を知りたい場合はドットプロットの方が適切
m %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(x = condition_mean, y = fct_rev(condition))) +
  stat_dots(quantiles = 100)


# 8 混合分布の要約統計量 ---------------------------------------------------------

# データ作成
set.seed(123)
multimodal_draws <-
  tibble(x = c(rnorm(5000, 0, 1),
               rnorm(2500, 4, 1)))

# データ確認
# --- カテゴリ付与はない
multimodal_draws %>% print()

# hdi分析
# --- 80％の確率レベルで複数の間隔が得られる
multimodal_draws %>%
  mode_hdi(x, .width = .80)

# プロット作成
multimodal_draws %>%
  ggplot(aes(x = x)) +
  stat_slab(aes(y = 0)) +
  stat_pointinterval(aes(y = -0.5), point_interval = median_qi, .width = c(.95, .80)) +
  annotate("text", label = "median, 80% and 95% quantile intervals",
           x = 0, y = -0.65, hjust = 0, vjust = 0.3) +
  stat_pointinterval(aes(y = -0.25), point_interval = mode_hdi, .width = c(.95, .80)) +
  annotate("text", label = "mode, 80% and 95% highest-density intervals",
           x = 0, y = -0.35, hjust = 0, vjust = 0.3)


# 9 要約統計量の加工 ---------------------------------------------------------

# 平均の差
# --- カテゴリ平均 - 全体平均
m %>%
  spread_draws(overall_mean, condition_mean[condition]) %>%
  mutate(condition_offset = condition_mean - overall_mean) %>%
  median_qi(condition_offset)


# 10 事後分布の予測 ----------------------------------------------------------

# 事後分布のプロット
# --- 条件平均と残差標準偏差を組み合わせてモデルから予測分布を生成
m %>%
  spread_draws(condition_mean[condition], response_sd) %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd)) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = y_rep)) +
  stat_slab()


m %>%
  spread_draws(condition_mean[condition], response_sd) %>%
  mutate(y_rep = rnorm(n(), condition_mean, response_sd)) %>%
  median_qi(y_rep, .width = c(.95, .8, .5)) %>%
  mutate(condition = factor(condition)) %>%
  ggplot(aes(y = fct_rev(condition), x = y_rep)) +
  geom_interval(aes(xmin = .lower, xmax = .upper)) + #auto-sets aes(color = fct_rev(ordered(.width)))
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()


# 11 平均の事後分布による事後予測 -----------------------------------------------

# 正規乱数の追加
# --- n()=4000
reps <-
  m %>%
    spread_draws(condition_mean[condition], response_sd) %>%
    mutate(y_rep = rnorm(n(), condition_mean, response_sd))

# プロット作成
ABC %>%
  ggplot(aes(y = condition)) +
  stat_interval(aes(x = y_rep), .width = c(.95, .8, .5), data = reps) +
  stat_pointinterval(aes(x = condition_mean), .width = c(.95, .66), position = position_nudge(y = -0.3), data = draws) +
  geom_point(aes(x = response)) +
  scale_color_brewer()


#N.B. the syntax for compare_levels is experimental and may change
m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()


m %>%
  gather_draws(overall_mean, condition_mean[condition]) %>%
  median_qi()


m %>%
  spread_draws(overall_mean, condition_mean[condition]) %>%
  mutate(condition_offset = condition_mean - overall_mean) %>%
  gather_variables() %>%
  median_qi()

m %>%
  tidy_draws() %>%
  head(10)


m %>%
  tidy_draws() %>%
  gather_variables() %>%
  head(10)


m %>%
  spread_draws(`condition_.*`[condition], regex = TRUE) %>%
  head(10)


# 12 不確実性のあるフィットカーブの作成 -----------------------------------------------

# モデル作成
m_mpg <-
  brm(mpg ~ hp * cyl, data = mtcars,
      file = "library/tidybayes/get_start/models/tidybayes_m_mpg.rds")


# プロット作成
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_fitted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

# プロット作成
# --- 妥当な数のフィットラインをサンプリングして、それらをオーバープロットすることもできる
# --- たとえば100
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_fitted_draws(m_mpg, n = 100) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = .1) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2")


# 13 他のライブラリとの互換性 -----------------------------------------------

# データ確認
ABC %>% print()
ABC$condition %>% table()

# モデル作成
# --- カテゴリを説明変数として線形回帰
m_linear <- lm(response ~ condition, data = ABC)

# 条件付き平均の算出
# --- OLS
linear_results <-
  m_linear %>%
    emmeans(~ condition) %>%
    tidy(conf.int = TRUE) %>%
    mutate(model = "OLS") %>%
    select(condition, condition, estimate, conf.low, conf.high, model)

# 条件付き平均の算出
# --- ベイズ
bayes_results <-
  m %>%
    recover_types(ABC) %>%
    spread_draws(condition_mean[condition]) %>%
    median_qi(estimate = condition_mean) %>%
    to_broom_names() %>%
    mutate(model = "Bayes") %>%
    select(condition, condition, estimate, conf.low, conf.high, model)

# 確認
linear_results %>% print()
bayes_results %>% print()

# プロット作成
linear_results %>%
  bind_rows(bayes_results) %>%
  mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(y = condition, x = estimate, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointinterval(position = position_dodge(width = .3))

# プロット作成
# --- {dotwhisker}
linear_results %>%
  bind_rows(bayes_results) %>%
  rename(term = condition) %>%
  dotwhisker::dwplot()



m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  head(10)


m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  unspread_draws(condition_mean[condition]) %>%
  head(10)



m %>%
  spread_draws(condition_mean[condition]) %>%
  compare_levels(condition_mean, by = condition) %>%
  unspread_draws(condition_mean[condition], drop_indices = TRUE) %>%
  bayesplot::mcmc_areas()

m_rst = stan_glm(response ~ condition, data = ABC)

m_rst %>%
  emmeans( ~ condition) %>%
  gather_emmeans_draws() %>%
  median_qi()


m_rst %>%
  emmeans( ~ condition) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  median_qi()


m_rst %>%
  emmeans( ~ condition) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye()


m_rst %>%
  emmeans(pairwise ~ condition) %>%
  gather_emmeans_draws() %>%
  median_qi()


# 13-3 MCMCglmm -----------------------------------------------------------

# モデル構築
m_glmm <- MCMCglmm(response ~ condition, data = as.data.frame(ABC))

# プロット
m_glmm %>%
  emmeans( ~ condition, data = ABC) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye()



