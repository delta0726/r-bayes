# ***********************************************************************************************
# Function  : bayesian
# Function  : bayesian()
# Created by: Owner
# Created on: 2021/04/08
# URL       : https://hsbadr.github.io/bayesian/reference/bayesian.html
# ***********************************************************************************************


# ＜概要＞
# - tidymodelsでベイズモデルを定義するための関数


# ＜構文＞
# bayesian(
#   mode = "regression",
#   engine = "brms",
#   formula.override = NULL,
#   family = NULL,
#   prior = NULL,
#   sample_prior = NULL,
#   knots = NULL,
#   stanvars = NULL,
#   fit = NULL,
#   inits = NULL,
#   chains = NULL,
#   iter = NULL,
#   warmup = NULL,
#   thin = NULL,
#   cores = NULL,
#   threads = NULL,
#   algorithm = NULL,
#   backend = NULL,
#   stan_args = NULL,
#   control = NULL,
#   save_pars = NULL,
#   save_model = NULL,
#   file = NULL,
#   file_refit = NULL,
#   normalize = NULL,
#   future = NULL,
#   seed = NULL,
#   silent = NULL
# )


# ＜引数＞
#


# ＜詳細＞
#


# ＜目次＞
# 0 準備
# 1 データ変換
# 2 エンジン

# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(bayesian)


# データ作成
df <- data.frame(
  plot = factor(paste0("p", rep(1:8, times = 2))),
  site = factor(paste0("s", rep(1:4, each = 2, times = 2)))
)

# データ確認
df %>% print()
df %>% map(table)


# 2 エンジン ------------------------------------------------------------------

# エンジン定義
bayesian() %>%
  set_engine("brms") %>%
  translate()