# ***********************************************************************************************
# Library   : tidybayes
# Title     : Extracting and visualizing tidy draws from brms models
# Created by: Owner
# Created on: 2021/04/09
# URL       : https://mjskay.github.io/tidybayes/articles/tidy-brms.html
# ***********************************************************************************************


# ＜ポイント＞
# - モデル変数の事後分布/近似/brms::brm()の予測を抽出して視覚化する方法について説明
#   --- tidybayesおよびggdistパッケージを使用


# ＜備考＞
# - {rstanarm}のバージョンも{brms}と同じ内容


# ＜目次＞




# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)


# プロットのテーマ設定
theme_set(theme_tidybayes() + panel_border())

# Stanの準備
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# 1 データ作成 -------------------------------------------------------------------

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
ABC$condition %>% table()

# プロット作成
# --- 分布の平均値が異なる
ABC %>%
  ggplot(aes(y = condition, x = response)) +
  geom_point()


# 2 モデル作成 -------------------------------------------------------------------

# モデル構築
m = brm(
  response ~ (1|condition),
  data = ABC,
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = sd),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99),
  file = "library/tidybayes/article/models/tidy-brms_m.rds"
)

# モデル確認
m %>% print()
m %>% names()
m %>% glimpse()


# 3 モデル結果の抽出 ---------------------------------------------------------------

# ＜ポイント＞
# - spread_draws()を用いるとイテレーションにモデル情報を追加することができる


# モデル変数名の取得
m %>% get_variables()

# イテレーションデータ抽出
# --- condition ：カテゴリ
# --- term      ：切片
m %>% spread_draws(r_condition[condition, term])

# イテレーションデータ抽出
#
m %>% spread_draws(r_condition[c, t])

m %>% spread_draws(r_condition[condition,])


# 4 点集計と区間集計 ---------------------------------------------------------------

m %>%
  spread_draws(b_Intercept, sigma) %>%
  head(10)

m %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi(b_Intercept, sigma)

m %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi()

m %>%
  gather_draws(b_Intercept, sigma) %>%
  median_qi()


m %>%
  spread_draws(r_condition[condition,]) %>%
  median_qi()

m %>%
  spread_draws(r_condition[condition,]) %>%
  group_by(condition) %>%   # this line not necessary (done by spread_draws)
  median_qi(r_condition)


m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  head(10)

m %>%
  spread_draws(`b_Intercept`, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  median_qi(condition_mean)

m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition)


# 5 点集計と区間集計のプロット ---------------------------------------------------------------

# 単純な区間推定
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()

# 複数の信頼区間
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .66)) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()

# 信頼区間 + 密度プロット
# --- conditionごとの結果のみ
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()

# 信頼区間 + 密度プロット
# --- conditionごと + 全体
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean, fill = stat(abs(x) < .8))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))


# 6 事後分布のフィット ---------------------------------------------------------------

# 事後分布の計算
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  head(10)

# 信頼区間のプロット
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  ggplot(aes(x = .value, y = condition)) +
  stat_pointinterval(.width = c(.66, .95))

# ドットプロット(密度)
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  ggplot(aes(x = .value, y = condition)) +
  stat_dots(quantiles = 100)

# 密度プロット
ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  stat_slab()

# 複数の信頼区間
ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(y = condition, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = response), data = ABC) +
  scale_color_brewer()

# Kruschke-style
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m, dpar = c("mu", "sigma")) %>%
  sample_draws(30) %>%
  ggplot(aes(y = condition)) +
  stat_dist_slab(aes(dist = "norm", arg1 = mu, arg2 = sigma),
    slab_color = "gray65", alpha = 1/10, fill = NA
  ) +
  geom_point(aes(x = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2)


# 7 予測カーブ -----------------------------------------------------

# モデル構築
m_mpg <-
  brm(mpg ~ hp * cyl, data = mtcars,
      file = "library/tidybayes/article/models/tidy-brms_m_mpg.rds")

# プロット作成
# --- 散布図 + ベイズ回帰
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
# --- 散布図 + ベイズ回帰(イテレーションごと)
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_fitted_draws(m_mpg, n = 100) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = .1) +
  geom_point(data = mtcars) +
  scale_color_brewer(palette = "Dark2")

# プロット作成
# --- 散布図 + 信頼区間
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl), fill = ordered(cyl))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")

# ファセット
mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 101)) %>%
  add_predicted_draws(m_mpg) %>%
  ggplot(aes(x = hp, y = mpg)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = brewer.pal(5, "Blues")[[5]]) +
  geom_point(data = mtcars) +
  scale_fill_brewer() +
  facet_grid(. ~ cyl, space = "free_x", scales = "free_x")


