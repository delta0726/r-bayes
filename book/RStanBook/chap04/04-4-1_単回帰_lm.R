# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 2 Stan入門編
# Chapter : 4-4 StanとRStanをはじめよう（単回帰_lm）
# Date    : 2022/05/06
# Page    : P34 - P38
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 単回帰を通してStanとRstanの使い方を確認する
#   --- 実際にはRからStanを実行してMCMCサンプルを得てベイズ信頼区間やベイズ予測区間を計算する
#   --- 単回帰は単純なモデルなのでlmとstanの結果にほぼ差がないことを確認する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 線形回帰モデルの構築
# 3 モデルによる予測
# 4 信頼区間のプロット
# 5 予測区間のプロット


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)


# データロード
d <- read_csv(file = 'chap04/csv/data-salary.txt')


# 1 データ確認 -----------------------------------------------------------------------------

# データ確認
# --- 年収(Y)と年齢(X)のデータ
d %>% print()

# 散布図
d %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point(shape = 1, size = 3) +
  theme_bw(base_size = 18)


# 2 線形回帰モデルの構築 --------------------------------------------------------------------

# モデル構築
res_lm <- lm(Y ~ X, data = d)

# サマリー
# --- 切片が-119.7、傾きが21.9となる
res_lm %>% summary()


# 3 モデルによる予測 ------------------------------------------------------------------------

# 予測用データ
X_new <- data.frame(X = 23:60)

# 予測
# --- 信頼区間を追加
# --- 予測区間を追加
conf_95 <- res_lm %>% predict(X_new, interval = 'confidence', level = 0.95)
pred_95 <- res_lm %>% predict(X_new, interval = 'prediction', level = 0.95)

# 確認
conf_95 %>% head()
pred_95 %>% head()


# 4 信頼区間のプロット ----------------------------------------------------------------------

# データ作成
conf_95 <-
  res_lm %>%
    predict(X_new, interval = 'confidence', level = 0.95) %>%
    as_tibble() %>%
    bind_cols(X_new)

conf_50 <-
  res_lm %>%
    predict(X_new, interval = 'confidence', level = 0.50) %>%
    as_tibble() %>%
    bind_cols(X_new)

# プロット作成
p <-
  ggplot() +
    geom_line(data = conf_50, aes(x = X, y = fit), size = 1) +
    geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
    scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
    geom_ribbon(data = conf_95, aes(x = X, ymin = lwr, ymax = upr), alpha = 1 / 6) +
    geom_ribbon(data = conf_50, aes(x = X, ymin = lwr, ymax = upr), alpha = 2 / 6) +
    labs(x = 'X', y = 'Y') +
    coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
    theme_bw(base_size = 18)

# 表示
print(p)


# 5 予測区間のプロット ----------------------------------------------------------------------=

# データ作成
pred_95 <-
  res_lm %>%
    predict(X_new, interval = 'prediction', level = 0.95) %>%
    as_tibble() %>%
    bind_cols(X_new)

pred_50 <-
  res_lm %>%
    predict(X_new, interval = 'prediction', level = 0.50) %>%
    as_tibble() %>%
    bind_cols(X_new)

# プロット作成
p <-
  ggplot() +
    geom_line(data = pred_50, aes(x = X, y = fit), size = 1) +
    geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
    scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
    geom_ribbon(data = pred_95, aes(x = X, ymin = lwr, ymax = upr), alpha = 1 / 6) +
    geom_ribbon(data = pred_50, aes(x = X, ymin = lwr, ymax = upr), alpha = 2 / 6) +
    labs(x = 'X', y = 'Y') +
    coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
    theme_bw(base_size = 18)

# 表示
print(p)
