# BIOS 667 Hands-On Session 1: From raw longitudinal data to valid inference
# Local (offline) version of the browser session:
#   https://daviddaiweizhang.github.io/unc-bios664/
#
# How to use this script:
#   - "FOLLOW ALONG" chunks are ready to run.
#   - "YOUR TURN" chunks describe what to write; try them yourself first.
#     The worked solution follows each one, marked "SOLUTION".
#
# Requirements: R (4.x) with dplyr, tidyr, ggplot2, nlme (installed below).

## Setup ---------------------------------------------------------------------

pkgs <- c("dplyr", "tidyr", "ggplot2", "nlme")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)

## ===========================================================================
## PROBLEM 1: the dental growth data (Sections 1-6)
## ===========================================================================

## Section 1: Load and reshape the data --------------------------------------

# FOLLOW ALONG: download the dental data if it is not already present.
# The first 27 lines of the file are a comment header, and the data is WIDE:
# one row per child, one column per age.
data_url  <- "https://content.sph.harvard.edu/fitzmaur/ala2e/dental.txt"
data_file <- "dental.txt"
if (!file.exists(data_file)) download.file(data_url, data_file)

dental_raw <- read.table(data_file, header = FALSE, skip = 27,
                         col.names = c("id", "gender",
                                       "age8", "age10", "age12", "age14"))
head(dental_raw)
table(dental_raw$gender)

# YOUR TURN (Task 1): reshape dental_raw from wide to long with
# pivot_longer(): gather the four measurement columns (age8, age10, age12,
# age14), names into a column "visit", values into a column "distance".
# Assign the result to `dental_long`.

# Replace NULL with your code:
dental_long <- NULL

head(dental_long, 8)


# SOLUTION (Task 1):
dental_long <- dental_raw |>
  pivot_longer(cols = starts_with("age"),
               names_to = "visit",
               values_to = "distance")
head(dental_long, 8)

# FOLLOW ALONG: tidy YOUR dental_long into the analysis data, with cosmetic
# steps added: turn "age8" into the number 8, give gender readable labels.
dental <- dental_long |>
  mutate(age = as.numeric(gsub("age", "", visit)),
         gender = factor(gender, levels = c("F", "M"),
                         labels = c("Girl", "Boy")))
nrow(dental)   # 27 children x 4 occasions = 108 rows

## Section 2: See the data before modeling it --------------------------------

# FOLLOW ALONG: the spaghetti plot. group = id connects the points belonging to
# the same child, the visual analog of "rows sharing an id are not independent";
# stat_summary(fun = mean) lays one mean curve per gender over the trajectories.
ggplot(dental, aes(x = age, y = distance, group = id, color = gender)) +
  geom_line(alpha = 0.4) +
  stat_summary(aes(group = gender), fun = mean,
               geom = "line", linewidth = 1.3) +
  labs(title = "Dental growth: individual trajectories and group means",
       x = "Age (years)", y = "Distance (mm)") +
  theme_minimal()

## Section 3: Quantify the correlation ---------------------------------------

# YOUR TURN (Task 3): compute the correlation matrix of the four
# measurement columns of dental_raw, rounded to 2 decimal places.
# Assign it to `cor_mat`.

# Replace NULL with your code:
cor_mat <- NULL

cor_mat


# SOLUTION (Task 3):
cor_mat <- round(cor(dental_raw[, c("age8", "age10", "age12", "age14")]), 2)
cor_mat

# FOLLOW ALONG: correlation heatmap (printed-matrix orientation).
cor_mat <- round(cor(dental_raw[, c("age8", "age10", "age12", "age14")]), 2)
cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("row", "col", "r")

ggplot(cor_long, aes(x = col, y = row, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 4) +
  scale_y_discrete(limits = rev(levels(cor_long$row))) +
  scale_fill_gradient(low = "white", high = "#4B9CD3", limits = c(0, 1)) +
  labs(title = "Correlation of dental distance across ages",
       x = NULL, y = NULL) +
  theme_minimal()

## Section 4: Why OLS is not enough -----------------------------------------

## --- Task 4: fit both models ------------------------------------------------

# YOUR TURN (Task 4):
#  1. Fit the mean model  distance ~ age + gender  by OLS with lm().
#     Call the fit `ols`.
#  2. Fit the SAME mean model with gls() from nlme, adding a
#     compound-symmetry correlation within each child (grouping: id).
#     Call the fit `gls_cs`.
#  3. Print the GLS coefficient table: summary(gls_cs)$tTable
# Careful: pass the grouping formula as the NAMED form argument,
# corCompSymm(form = ~ 1 | id); without form = it silently fits NO
# working correlation.

# Replace each NULL with your code:
ols <- NULL
gls_cs <- NULL
# then print the coefficient table (works once NULL is replaced):
# summary(gls_cs)$tTable


# SOLUTION (Task 4):
ols <- lm(distance ~ age + gender, data = dental)
gls_cs <- gls(distance ~ age + gender, data = dental,
              correlation = corCompSymm(form = ~ 1 | id))
summary(gls_cs)$tTable

# FOLLOW ALONG: standard errors side by side.
# Gender (between-child) SE grows ~1.7x; age (within-child) SE shrinks ~0.6x.
se_ols <- coef(summary(ols))[, "Std. Error"]
se_gls <- summary(gls_cs)$tTable[, "Std.Error"]
round(cbind(OLS = se_ols, GLS_CS = se_gls, ratio = se_gls / se_ols), 3)

# FOLLOW ALONG: the estimated within-child correlation (about 0.61).
coef(gls_cs$modelStruct$corStruct, unconstrained = FALSE)


## Section 5: ML vs REML, and a real test ------------------------------------

# YOUR TURN (Task 5):
#  1. Fit the additive model    distance ~ age + gender  with gls(),
#     compound symmetry within child, and the method that makes mean-model
#     likelihoods comparable. Call it `m_add`.
#  2. Fit the interaction model distance ~ age * gender  the same way.
#     Call it `m_int`.

# Replace each NULL with your code:
m_add <- NULL
m_int <- NULL
# then compare the fits by likelihood ratio test (works once NULL is replaced):
# anova(m_add, m_int)


# SOLUTION (Task 5): the method is ML.
m_add <- gls(distance ~ age + gender, data = dental,
             correlation = corCompSymm(form = ~ 1 | id), method = "ML")
m_int <- gls(distance ~ age * gender, data = dental,
             correlation = corCompSymm(form = ~ 1 | id), method = "ML")
anova(m_add, m_int)   # LRT ~6.2 on 1 df, p = 0.013: growth rates differ

# FOLLOW ALONG: refit the chosen model with REML (the default) to report.
m_final <- gls(distance ~ age * gender, data = dental,
               correlation = corCompSymm(form = ~ 1 | id))
round(summary(m_final)$tTable, 3)

# FOLLOW ALONG: ML vs REML residual SD on the additive model.
# ML is biased downward (no correction for estimating beta).
c(ML = gls(distance ~ age + gender, data = dental,
           correlation = corCompSymm(form = ~ 1 | id), method = "ML")$sigma,
  REML = gls_cs$sigma)

## Section 6: A live missing-data experiment ---------------------------------

# FOLLOW ALONG: apply the dropout rule. The age-14 visit is skipped for
# every child whose age-12 distance was 26 mm or more. Dropout depends only
# on an OBSERVED response, so the mechanism is MAR.
wide <- dental_raw
wide$dropout <- wide$age12 >= 26
table(dropout = wide$dropout)

# YOUR TURN (Task 6):
#  1. Compute the age-14 mean over ALL children in `wide`.
#  2. Compute the age-14 mean over the COMPLETERS only (dropout == FALSE).
# Replace each NULL with your code:
mean_all <- NULL
mean_completers <- NULL
# then print both, rounded to 2 decimals (works once NULL is replaced);
# which is smaller, and why?
# round(c(all_children = mean_all, completers_only = mean_completers), 2)


# SOLUTION (Task 6): completers-only is biased low (~24.9 vs ~26.1),
# because the dropout rule removed exactly the largest children.
mean_all <- mean(wide$age14)
mean_completers <- mean(wide$age14[!wide$dropout])
round(c(all_children = mean_all, completers_only = mean_completers), 2)

# FOLLOW ALONG: likelihood-based analysis of the observed rows. The
# dropouts' age 8-12 records enter the fit and, through the within-child
# correlation and the growth model, carry information about where those
# children were headed at age 14.
obs <- dental |>
  mutate(dropout = id %in% wide$id[wide$dropout]) |>
  filter(!(dropout & age == 14))

fit_obs <- gls(distance ~ age * gender, data = obs,
               correlation = corCompSymm(form = ~ 1 | id))

newdat <- data.frame(age = 14,
                     gender = factor(c("Girl", "Boy"),
                                     levels = c("Girl", "Boy")))
pred <- predict(fit_obs, newdat)
model_based <- sum(pred * prop.table(table(dental_raw$gender))[c("F", "M")])

round(c(true_mean       = mean(wide$age14),
        completers_only = mean(wide$age14[!wide$dropout]),
        model_based     = model_based), 2)


