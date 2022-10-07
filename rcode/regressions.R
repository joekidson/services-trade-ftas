# run regressions
library(penppml)
library(ggplot2)
library(knitr)
library(DT)

# load data from data_cleaning.R
# df_reg <- read.csv("data/regression_data_2005_2016_allservices_servicesFTAs.csv") %>%
#   select(!c(id, agreement, year_inforce))
df_reg <- df_final %>%
  select(!c(id, agreement, year_inforce))

# run unpenalised regression
reg0 <- hdfeppml(data = df_reg,
                 dep = "service_trade",
                 fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")))

# run penalised regression
regpen <- penhdfeppml(data = df_reg,
                    dep = "service_trade",
                    fixed = list(c("exp", "time"), 
                                 c("imp", "time"),
                                 c("exp", "imp")),
                    penalty = "lasso",
                    lambda = 0.0001)

# compare results
results <- data.frame(regpen$beta) %>%
  rownames_to_column("Provision")

datatable(results, fillContainer = FALSE)

# ----------- IGNORE

# example with pre-loaded data
selected <- countries$iso[countries$region %in% c("Americas")]
trade2 <- trade[(trade$exp %in% selected) & (trade$imp %in% selected), -(5:6)]

# unpenalised
reg1 <- hdfeppml(data = trade2,
                 dep = "export",
                 fixed = list(c("exp", "time"), 
                              c("imp", "time"),
                              c("exp", "imp")))

results <- data.frame(prov = rownames(reg1$coefficients), b = reg1$coefficients, se = 0)
results$se[!is.na(reg1$coefficients)] <- reg1$se
results

# penalised
lambdas <- c(0.05, 0.025, 0.01, 0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005, 0.00025, 0.0001, 0)

reg2 <- mlfitppml(data = trade2,
                  dep = "export",
                  fixed = list(c("exp", "time"), 
                               c("imp", "time"),
                               c("exp", "imp")),
                  penalty = "lasso",
                  lambdas = lambdas)

results <- as.data.frame(reg2$beta_pre)
names(results) <- lambdas
results$provision <- row.names(results)
results <- reshape2::melt(results, id.vars = "provision", variable.name = "lambda", 
                          value.name = "coefficient")
results$lambda <- as.numeric(as.character(results$lambda))

ggplot2::ggplot(data = results, mapping = ggplot2::aes(x = lambda, y = coefficient, col = provision)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::scale_x_reverse(expand = ggplot2::expansion(add = c(0, 0.015))) +
  ggplot2::theme_classic() +
  directlabels::geom_dl(ggplot2::aes(label = provision), 
                        method = list(directlabels::dl.trans(x = x + 0.5), "last.bumpup")) +
  ggplot2::labs(x = "Penalty parameter (lambda)", y = "Coefficient", 
                title = "Figure 1: Regularization path for lasso")


# penalised simple
reg3 <- penhdfeppml(data = trade2,
                    dep = "export",
                    fixed = list(c("exp", "time"), 
                                 c("imp", "time"),
                                 c("exp", "imp")),
                    penalty = "lasso",
                    lambda = 0.005)

reg3$beta

results3 <- data.frame(reg3$beta) %>%
  rownames_to_column("Provision")


# output table
kable(list(results3[1:8, ], results3[9:16, ]), 
      format = "html",
      col.names = c("Provision", "Coefficient"), 
      caption = "Table 3: Unpenalized PPML results",
      row.names = FALSE,
      digits = 4)


datatable(results3, fillContainer = FALSE)
