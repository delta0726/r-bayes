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
# - 省略


# ＜詳細＞
# - エンジンは{brms}を用いる
# - 分類と回帰の両方に対応


# ＜目次＞
# 0 準備
# 1 アイテム確認
# 2 モデル定義


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(bayesian)


# 1 アイテム確認 -----------------------------------------------------------------

# モデル定義
bayesian()

# モデル情報
show_model_info("bayesian")

# エンジン
# --- {brams}
bayesian() %>%
  set_engine("brms") %>%
  translate()


# 2 モデル定義 -------------------------------------------------------------------

# モデル定義
bayesian_mod <-
  bayesian() %>%
    set_engine("brms") %>%
    fit(rating ~ treat + period + carry + (1 | subject), 
        data = inhaler)

# 確認
bayesian_mod$fit %>% summary()

