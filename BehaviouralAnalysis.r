#  RESET ENVIRONMENT ----
rm(list = ls())
setwd("G:\\OneDrive - University Of Cambridge\\Work\\PhD Psychology\\Experiment 4 - AnG and the Visual Perspective of Retrieval\\MainStudy\\Results\\Analyses\\Behavioural")
cat("\014")
options(scipen = 999) # suppress scientific notation
# Make summary() produce type III contrasts using deviation coding, which 
# provides a more ANOVA-like interpretation of mixed effect model interactions 
# see https://www.alexanderdemos.org/Class8.html
options(contrasts = c("contr.sum","contr.poly"))

#  LOAD PACKAGES ----
pkgNames <- c("moments", 
              "tidyverse", 
              "ggpubr", 
              "rstatix", 
              "reshape2", 
              "psycho", 
              "ez",
              "afex",
              "emmeans",
              "multcomp",
              "MASS",
              "dplyr",
              "ggplot2",
              "geoR",
              "lme4",
              "brms",
              "car",
              "moments",
              "statmod",
              "DHARMa",
              "AICcmodavg",
              "lmerTest", 
              "glmmTMB",
              "MuMIn", 
              "rstanarm",
              "performance",
              "AID",
              "BayesFactor", 
              "nparLD",
              "dfoptim",
              "optimx", 
              "data.table",
              "Barnard",
              "MuMIn",
              "sjstats",
              "pwr",
              "ppcor",
              "nlme",
              "influence.ME",
              "buildmer",
              "remotes",
              "simr",
              "ggdist")

for (pkgName in pkgNames)
{
   if(!require(pkgName, character.only = TRUE))
   {
      install.packages(pkgName)
   }
   library(pkgName, character.only = TRUE)
}

#  CUSTOM FUNCTIONS / OPERATORS ----
# negate %in%
'%!in%' <- function(x, y) { !('%in%'(x, y)) }

# get Z-score
z.score <- function(data) { (data - mean(data)) / sd(data) }

#  READ DATA ----
data_Behav <- read.csv("Data_Behav.csv", 
                       header = TRUE, 
                       sep = ",")

data_OA_CogTests <- read.csv("OA_CogTests.csv",
                             header = TRUE, 
                             sep = ",")

# subset by condition
wide_data_demo <- data_Behav |> 
   subset(select = c(ID, grp, Age, Education_Yrs, Sex, ScanTestInterval))

wide_data_OA_CogTests <- data_OA_CogTests |>
   subset(select = c(ID, Recognition_memory_composite , Trails_A, Trails_B, Trails_corrected, Digit_Span_Total))

wide_data_MS <- data_Behav |> 
   subset(select = c(ID, grp, Ego_Stay_MS, Ego_Switch_MS, Allo_Stay_MS, Allo_Switch_MS))

wide_data_DistErr <- data_Behav |> 
   subset(select = c(ID, grp, Ego_Stay_DistErr, Ego_Switch_DistErr, Allo_Stay_DistErr, Allo_Switch_DistErr))

wide_data_RT <- data_Behav |>  
   subset(select = c(ID, grp, Ego_Stay_RT, Ego_Switch_RT, Allo_Stay_RT, Allo_Switch_RT)) |>
   na.omit()

# reshape to long format
long_all_MS <- wide_data_MS |> 
   pivot_longer(
      cols = 3:6, 
      names_to = c("perspective", "switchStatus"), 
      names_sep = "_") |>
   convert_as_factor(ID, grp, perspective, switchStatus)

long_all_DistErr <- wide_data_DistErr |> 
   pivot_longer(
      cols = 3:6, 
      names_to = c("perspective", "switchStatus"), 
      names_sep = "_") |>
   convert_as_factor(ID, grp, perspective, switchStatus)

long_all_RT <- wide_data_RT |> 
   pivot_longer(
      cols = 3:6, 
      names_to = c("perspective", "switchStatus"), 
      names_sep = "_") |>
   convert_as_factor(ID, grp, perspective, switchStatus)

newGrpOrder <- c("YA", "OA")
long_all_MS$grp <- factor(long_all_MS$grp, 
                          levels = newGrpOrder)

long_all_RT$grp <- factor(long_all_RT$grp,
                          levels = newGrpOrder)

newPerspectiveNames <- c("Third person", "First person")
levels(long_all_MS$perspective) <- newPerspectiveNames
levels(long_all_RT$perspective) <- newPerspectiveNames

newPerspectiveOrder <- rev(newPerspectiveNames)
long_all_MS$perspective <- factor(long_all_MS$perspective, 
                                  levels = newPerspectiveOrder)
long_all_RT$perspective <- factor(long_all_RT$perspective, 
                                  levels = newPerspectiveOrder)

# OA MS dataset including cognitive tests
long_OA_MS <- long_all_MS |> subset(grp %in% "OA")
long_OA_MS$recMem <- wide_data_OA_CogTests$Recognition_memory_composite |>
   rep(each = 4)
long_OA_MS$trailsDiff <- wide_data_OA_CogTests$Trails_corrected |>
   rep(each = 4)

long_OA_MS$TrailsA <- wide_data_OA_CogTests$Trails_A |>
  rep(each = 4)
long_OA_MS$trailsB <- wide_data_OA_CogTests$Trails_B |>
  rep(each = 4)
long_OA_MS$dSpan <- data_OA_CogTests$Digit_Span_Total |>
   rep(each = 4)

#  DATA VISUALIZATION ----
#  see https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
bwSmth = 1.4 # adjust smoothness of density plots

# YAs
plot_YA_MS <- wide_data_MS |> 
   subset(grp %in% "YA") |>
   melt(id = c("ID", "grp")) |>
   group_by(variable) |>
   mutate(mean = mean(value)) |>
   ungroup() |>
   ggplot(aes(x = variable, y = value)) + 
   ggdist::stat_halfeye(
      adjust = bwSmth, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
   geom_boxplot(
      width = .25, 
      outlier.shape = NA
   ) +
   geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
         seed = 1, width = .1
      )
   ) + 
   coord_cartesian(xlim = c(1.2, NA), clip = "off")

plot_YA_DistErr <- wide_data_DistErr |> 
   subset(grp %in% "YA") |>
   melt(id = c("ID", "grp")) |>
   group_by(variable) |>
   mutate(mean = mean(value)) |>
   ungroup() |>
   ggplot(aes(x = variable, y = value)) + 
   ggdist::stat_halfeye(
      adjust = bwSmth, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
   geom_boxplot(
      width = .25, 
      outlier.shape = NA
   ) +
   geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
         seed = 1, width = .1
      )
   ) + 
   coord_cartesian(xlim = c(1.2, NA), clip = "off")

plot_YA_RT <- wide_data_RT |> 
   subset(grp %in% "YA") |>
   melt(id = c("ID", "grp")) |>
   group_by(variable) |>
   mutate(mean = mean(value)) |>
   ungroup() |>
   ggplot(aes(x = variable, y = value)) + 
   ggdist::stat_halfeye(
      adjust = bwSmth, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
   geom_boxplot(
      width = .25, 
      outlier.shape = NA
   ) +
   geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
         seed = 1, width = .1
      )
   ) + 
   coord_cartesian(xlim = c(1.2, NA), clip = "off")

# OAs
plots_OA_MS <- wide_data_MS |> 
   subset(grp %in% "OA") |>
   melt(id = c("ID", "grp")) |>
   group_by(variable) |>
   mutate(mean = mean(value)) |>
   ungroup() |>
   ggplot(aes(x = variable, y = value)) + 
   ggdist::stat_halfeye(
      adjust = bwSmth, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
   geom_boxplot(
      width = .25, 
      outlier.shape = NA
   ) +
   geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
         seed = 1, width = .1
      )
   ) + 
   coord_cartesian(xlim = c(1.2, NA), clip = "off")

plots_OA_DistErr <- wide_data_DistErr |> 
   subset(grp %in% "OA") |>
   melt(id = c("ID", "grp")) |>
   group_by(variable) |>
   mutate(mean = mean(value)) |>
   ungroup() |>
   ggplot(aes(x = variable, y = value)) + 
   ggdist::stat_halfeye(
      adjust = bwSmth, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
   geom_boxplot(
      width = .25, 
      outlier.shape = NA
   ) +
   geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
         seed = 1, width = .1
      )
   ) + 
   coord_cartesian(xlim = c(1.2, NA), clip = "off")

plots_OA_RT <- wide_data_RT |> 
   subset(grp %in% "OA") |>
   melt(id = c("ID", "grp")) |>
   group_by(variable) |>
   mutate(mean = mean(value)) |>
   ungroup() |>
   ggplot(aes(x = variable, y = value)) + 
   ggdist::stat_halfeye(
      adjust = bwSmth, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
   geom_boxplot(
      width = .25, 
      outlier.shape = NA
   ) +
   geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
         seed = 1, width = .1
      )
   ) + 
   coord_cartesian(xlim = c(1.2, NA), clip = "off")

#  OUTLIER IDENTIFICATION ----
# all
otlrs_all_MS <- long_all_MS |> 
   group_by(grp, perspective, switchStatus) |> 
   identify_outliers(value)
length(unique(otlrs_all_MS$ID))

otlrs_all_DistErr <- long_all_DistErr |> 
   group_by(grp,perspective, switchStatus) |> 
   identify_outliers(value)
length(unique(otlrs_all_DistErr$ID))

otlrs_all_RT <- long_all_RT |> 
   group_by(grp,perspective, switchStatus) |> 
   identify_outliers(value)
length(unique(otlrs_all_RT$ID))

#  DEMOGRAPHIC TESTS ----
counts <- wide_data_demo |> 
   group_by(grp) |> 
   mutate(male = sum(Sex == "M"), 
          fem = sum(Sex == "F"),
          .keep = "none") |>
   distinct() |>
   as.data.frame()

ft_Sex <- fisher.test(counts[, -1],
                      alternative = "two.sided")

bt_Sex <- barnard.test(counts[1, 2], counts[1, 3], # more powerful than ft for 2x2 contingency tables
                       counts[2, 2], counts[2, 3], 
                       pooled = FALSE)

tt_age <- t.test(x = wide_data_demo |> 
                    subset(grp == "YA", Age),
                 y = wide_data_demo |> 
                    subset(grp == "OA", Age),
                 paired = FALSE,
                 alternative = "two.sided",
                 var.equal = FALSE)

tt_Education <- t.test(x = wide_data_demo |> 
                    subset(grp == "YA", Education_Yrs),
                 y = wide_data_demo |> 
                    subset(grp == "OA", Education_Yrs),
                 paired = FALSE,
                 alternative = "two.sided",
                 var.equal = FALSE)

tt_ScanTestInterval <- t.test(x = wide_data_demo |> 
                                 subset(grp == "YA", ScanTestInterval),
                              y = wide_data_demo |>
                                 subset(grp == "OA", ScanTestInterval),
                              paired = FALSE,
                              alternative = "two.sided",
                              var.equal = FALSE)

#  MODEL SELECTION ----
# Aggregate data used as models on raw data failed to converge
#
# Backward elimination of random effects (using LRTs) to find maximal model supported by
# the data (Matuschek et al., 2017). All fixed factors must be retained as aim is 
# confirmatory hypothesis testing rather than prediction.

# maximal feasible model for aggregate data includes random slopes for each WS fixed factor (interactions not possible)
maximalFormula = value ~ grp * perspective * switchStatus + (1 + perspective + switchStatus | ID);
# minimal model must include a by-subject random intercept to account for repeated measures
minimalFormula = value ~ grp * perspective * switchStatus + (1 | ID) # pins all fixed factors and enforces a by-subject random intercept at minimum to account for repeated measures

# model selection for MS
mm_Gauss_MS <- buildmer(formula = maximalFormula,
                        data = long_all_MS,
                        family = gaussian(),
                        buildmerControl = list(direction = 'backward',
                                               crit = 'LRT',
                                               elim = LRTalpha(0.20),
                                               include = minimalFormula,
                                               ddf = 'Satterthwaite'))

mm_Gauss_MS_fin <- lmer(formula = value ~ grp * perspective * switchStatus + (1 | ID),
                        data = long_all_MS)

# resid vs fitted
mm_Gauss_MS_fin |>
   plot(type = c("p", "smooth"), 
        col.line = 2)
# scale-location
mm_Gauss_MS_fin |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(2, 2))
# resid density
mm_Gauss_MS_fin |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
mm_Gauss_MS_fin |> 
   residuals() |>
   qqPlot()

summary(mm_Gauss_MS_fin)
confint(mm_Gauss_MS_fin)

# follow up interaction analysis
#https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html
# perspective : switchStatus | grp
emmip(mm_Gauss_MS_fin, perspective ~ switchStatus | grp)

emm_Gauss_MS_fin <- emmeans(object = mm_Gauss_MS_fin, 
                      specs = pairwise ~ grp : switchStatus | perspective,
                      lmer.df = "satterthwaite")
emm_Gauss_MS_fin$emmeans |> pairs(adjust = "Tukey") 
emm_Gauss_MS_fin |> plot(comparisons = TRUE)

# grp : perspective | switchStatus
emm_Gauss_MS_fin <- emmeans(object = mm_Gauss_MS_fin, 
                      specs = pairwise ~ grp : perspective | switchStatus,
                      lmer.df = "satterthwaite")
emm_Gauss_MS_fin$emmeans |> pairs(adjust = "Tukey") 
emm_Gauss_MS_fin |> plot(comparisons = TRUE)

# model selection for RT (starting with a Gaussian dist)
mm_Gauss_RT <- buildmer(formula = maximalFormula,
                        data = long_all_RT,
                        family = gaussian(),
                        buildmerControl = list(direction = 'backward',
                                               crit = 'LRT',
                                               elim = LRTalpha(0.20),
                                               include = minimalFormula,
                                               #REML = FALSE,
                                               ddf = 'Satterthwaite'))

mm_Gauss_RT_fin <- lmer(formula = grp * perspective * switchStatus + (1 | ID),
                        data = long_all_RT)

# resid vs fitted
mm_Gauss_RT_fin |>
   plot(type = c("p", "smooth"), 
        col.line = 2)
# scale-location
mm_Gauss_RT_fin |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(2, 2))
# resid density
mm_Gauss_RT_fin |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
mm_Gauss_RT_fin |> 
   residuals() |>
   qqPlot()

summary(mm_Gauss_RT_fin)

## model selection for RT (trying different response distributions)
# Gamma dist and modelling a linear (identity link) relationship
mm_GammaIdent_RT <- buildmer(formula = maximalFormula,
                             data = long_all_RT,
                             family = Gamma(link = "identity"),
                             buildmerControl = list(direction = 'backward',
                                                    crit = 'LRT',
                                                    elim = LRTalpha(0.20),
                                                    include = minimalFormula,
                                                    ddf = 'Wald'))

mm_GammaIdent_RT_fin <- glmer(formula = formula(mm_GammaIdent_RT),
                              data = long_all_RT,
                              family = Gamma(link = "identity"))

# resid vs fitted
mm_GammaIdent_RT_fin |>
   plot(type = c("p", "smooth"), 
        col.line = 2)
# scale-location
mm_GammaIdent_RT_fin |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(2, 2))
# resid density
mm_GammaIdent_RT_fin |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
mm_GammaIdent_RT_fin |> 
   residuals() |>
   qqPlot()

summary(mm_GammaIdent_RT_fin)
#https://www.r-bloggers.com/2015/06/confidence-intervals-for-prediction-in-glmms/

mm_InvGaussIdent_RT <- buildmer(formula = maximalFormula,
                                data = long_all_RT,
                                family = inverse.gaussian(link = "identity"),
                                buildmerControl = list(direction = 'backward',
                                                       crit = 'LRT',
                                                       elim = LRTalpha(0.20),
                                                       include = minimalFormula,
                                                       ddf = 'Wald'))

mm_InvGaussIdent_RT_fin <- glmer(formula = value ~ grp * perspective * switchStatus + (1 | ID),
                                 data = long_all_RT,
                                 family = inverse.gaussian(link = "identity"),
                                 control = glmerControl(optimizer = "bobyqa"))

# resid vs fitted
mm_InvGaussIdent_RT_fin |>
   plot(type = c("p", "smooth"), 
        col.line = 2)
# scale-location
mm_InvGaussIdent_RT_fin |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(2, 2))
# resid density
mm_InvGaussIdent_RT_fin |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
mm_InvGaussIdent_RT_fin |> 
   residuals() |>
   qqPlot()

summary(mm_InvGaussIdent_RT_fin)

# RT model comparison 
anova(mm_Gauss_RT_fin,
      mm_GammaIdent_RT_fin,
      mm_InvGaussIdent_RT_fin)

#  SENSITIVITY ANALYSES ----
# removing outliers from MS model
mm_Gauss_MS_noOtlrs <- lmer(formula = value ~ grp * perspective * switchStatus + (1 | ID),
                            data = long_all_MS |>
                               subset(ID %!in% otlrs_all_MS$ID))

n_MS_Otlr_Subs <- otlrs_all_MS$ID |> 
   as.vector() |> 
   unique() |> 
   length()

summary(mm_Gauss_MS_noOtlrs)

# adding measures of general memory ability and executive function to model in OAs
mm_MS_OAs_incRecMem <- lmer(formula = value ~ recMem + perspective * switchStatus + (1 | ID),
                              data = long_OA_MS)

summary(mm_MS_OAs_incRecMem)
emmeans(object = mm_MS_OAs_incRecMem, 
        specs = pairwise ~ recMem,
        lmer.df = "satterthwaite")

mm_MS_OAs_incTrailsDiff <- lmer(formula = value ~ trailsDiff + recMem + perspective * switchStatus + (1 | ID),
                            data = long_OA_MS)

summary(mm_MS_OAs_incTrailsDiff)

emmip(mm_MS_OAs_incTrailsDiff, perspective ~ switchStatus | trailsDiff)
emm_MS_OAs_incTrailsDiff <- emmeans(object = mm_MS_OAs_incTrailsDiff, 
                            specs = pairwise ~ trailsDiff : switchStatus | perspective,
                            lmer.df = "satterthwaite")
emm_MS_OAs_incTrailsDiff$emmeans |> pairs(adjust = "bonf") 

mm_MS_OAs_incDSpan <- lmer(formula = value ~ dSpan * perspective * switchStatus + (1 | ID),
                            data = long_OA_MS)

# removing outliers from RT model
mm_GammaIdent_RT_noOtlrs <- glmer(formula = formula(mm_GammaIdent_RT),
                              data = long_all_RT |>
                                 subset(ID %!in% otlrs_all_RT$ID),
                              family = Gamma(link = "identity"))

n_RT_Otlr_Subs <- otlrs_all_RT$ID |> 
   as.vector() |> 
   unique() |> 
   length()

summary(mm_GammaIdent_RT_noOtlrs)

# # check for influence of outliers
# infl_mm_all_MS <- influence(mm_all_MS, obs = TRUE)
# plot(infl_mm_all_MS, which = "cook") # cook's distance
# abline(v = (4 / (46 * 4)), # cutoff value based on 4 / n (obs.) rule of thumb
#        col = "red", 
#        lwd = 3, 
#        lty = 2)
# 
# mm_all_MS_otlrsRmvd <- lmer( 
#    formula = value ~ grp * perspective * switchStatus + (1 | ID),
#    data = long_all_MS |> 
#       subset(ID %!in% otlrs_all_MS$ID),
#    control = lmerControl(optimizer = "nloptwrap", 
#                          optCtrl = list(maxfun = 5e4)))
# par(mfrow=c(2, 2))
# plot(mm_all_MS_otlrsRmvd, # resid vs fitted
#      type = c("p", "smooth"), 
#      col.line = 2)
# plot(mm_all_MS_otlrsRmvd, # scale-location
#      sqrt(abs(resid(.))) ~ fitted(.),
#      type = c("p", "smooth"),
#      col.line = 2)
# plot(density(residuals(mm_all_MS_otlrsRmvd))) # resid density
# qqPlot(residuals(mm_all_MS_otlrsRmvd)) # resid qq
# summary(mm_all_MS_otlrsRmvd)
# 
# #  additional models were fit using nlme:
# # (1) lme without weighted errors
# lmeHomoVar_all_MS <- lme(fixed = value ~ grp * perspective * switchStatus,
#                   random = ~1 | ID,
#                   data = long_all_MS,
#                   method = "ML")
# summary(lmeHomoVar_all_MS)
# plot(lmeHomoVar_all_MS)
# 
# # (2) lme allowing different error variances for both age groups
# lmeHeteroVar_all_MS <- lme(fixed = value ~ grp * perspective * switchStatus,
#                            random = ~1 | ID,
#                            weight = varIdent(form = ~1 | grp),
#                            data = long_all_MS,
#                            method = "ML")
# summary(lmeHeteroVar_all_MS)
# plot(lmeHeteroVar_all_MS)
# summary(lmeHeteroVar_all_MS)
# 
# anova(lmeHomoVar_all_MS, lmeHeteroVar_all_MS)
# 
# plot(mm_all_MS)

# #get confidence intervals using confint([model])
# mm_all_RT_initMod <- lmer(
#    formula = value ~ grp * perspective * switchStatus + (1 + perspective + switchStatus | ID),
#    data = long_all_RT,
#    control = lmerControl(optimizer = "bobyqa", 
#                          optCtrl = list(maxfun = 5e4)))
# par(mfrow=c(2, 2))
# plot(mm_all_RT_initMod, # resid vs fitted
#      type = c("p", "smooth"), 
#      col.line = 2)
# plot(mm_all_RT_initMod, # scale-location
#      sqrt(abs(resid(.))) ~ fitted(.),
#      type = c("p", "smooth"),
#      col.line = 2)
# plot(density(residuals(mm_all_RT_initMod))) # resid density
# qqPlot(residuals(mm_all_RT_initMod)) # resid qq
# summary(mm_all_RT_initMod)
# 
# RT_mods <- step(object = mm_all_RT_initMod,
#      ddf = "Satterthwaite",
#      reduce.fixed = FALSE,
#      reduce.random = TRUE)
# 
# mm_all_RT_finMod <- get_model(RT_mods)
# par(mfrow=c(2, 2))
# plot(mm_all_RT_finMod, # resid vs fitted
#      type = c("p", "smooth"), 
#      col.line = 2)
# plot(mm_all_RT_finMod, # scale-location
#      sqrt(abs(resid(.))) ~ fitted(.),
#      type = c("p", "smooth"),
#      col.line = 2)
# plot(density(residuals(mm_all_RT_finMod))) # resid density
# qqPlot(residuals(mm_all_RT_finMod)) # resid qq
# summary(mm_all_RT_finMod)

# Mixed Models for each group separately ----
# YAs
# mm_YA_MS <- lmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_MS |> 
#       subset(grp %in% "YA"),
#    control = lmerControl(optimizer = "bobyqa", 
#                          optCtrl = list(maxfun = 5e4)
#    )
# )
# par(mfrow=c(2, 1))
# plot(mm_YA_MS)
# plot(density(residuals(mm_YA_MS)))
# qqPlot(residuals(mm_YA_MS))
# summary(mm_YA_MS)
# 
# mm_YA_DistErr <- lmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_DistErr |> 
#       subset(grp %in% "YA"),
#    control = lmerControl(optimizer = "nloptwrap", 
#                          optCtrl = list(maxfun = 5e4)
#    )
# )
# par(mfrow=c(2, 1))
# plot(mm_YA_DistErr)
# plot(density(residuals(mm_YA_DistErr)))
# qqPlot(residuals(mm_YA_DistErr))
# summary(mm_YA_DistErr)
# 
# mm_YA_RT <- lmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_RT |> 
#       subset(grp %in% "YA"),
#    control = lmerControl(optimizer = "nloptwrap", 
#                          optCtrl = list(maxfun = 5e4)
#    )
# )
# par(mfrow=c(2, 1))
# plot(mm_YA_RT)
# plot(density(residuals(mm_YA_RT)))
# qqPlot(residuals(mm_YA_RT))
# summary(mm_YA_RT)
# 
# mm_Gamma_YA_RT <- glmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_RT |> 
#       subset(grp %in% "YA"),
#    family = Gamma(link = "identity"),
#    control = glmerControl(optimizer = "bobyqa", 
#                           optCtrl = list(maxfun = 5e4)))
# par(mfrow=c(3, 1))
# plot(mm_Gamma_YA_RT)
# plot(density(residuals(mm_Gamma_YA_RT)))
# qqPlot(residuals(mm_Gamma_YA_RT))
# summary(mm_Gamma_YA_RT)
# 
# # OAs
# mm_OA_MS <- lmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_MS |> subset(grp %in% "OA"),
#    control = lmerControl(optimizer = "nloptwrap", 
#                          optCtrl = list(maxfun = 5e4)
#    )
# )
# par(mfrow=c(2, 1))
# plot(mm_OA_MS)
# plot(density(residuals(mm_OA_MS)))
# qqPlot(residuals(mm_OA_MS))
# summary(mm_OA_MS)
# 
# effectsize::eta_squared(mm_OA_MS)
# 
# mm_OA_DistErr <- lmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_DistErr |> 
#       subset(grp %in% "OA"),
#    control = lmerControl(optimizer = "nloptwrap", 
#                          optCtrl = list(maxfun = 5e4)
#    )
# )
# par(mfrow=c(2, 1))
# plot(mm_OA_DistErr)
# plot(density(residuals(mm_OA_DistErr)))
# qqPlot(residuals(mm_OA_DistErr))
# summary(mm_OA_DistErr)
# 
# mm_OA_RT <- lmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_RT |> 
#       subset(grp %in% "OA"),
#    control = lmerControl(optimizer = "nloptwrap", 
#                          optCtrl = list(maxfun = 5e4)
#    )
# )
# par(mfrow=c(2, 1))
# plot(mm_OA_RT)
# plot(density(residuals(mm_OA_RT)))
# qqPlot(residuals(mm_OA_RT))
# summary(mm_OA_RT)
# 
# mm_Gamma_OA_RT <- glmer(
#    formula = value ~ perspective * switchStatus + (1 | ID),
#    data = long_all_RT |> 
#       subset(grp %in% "OA"),
#    family = Gamma(link = "identity"),
#    control = glmerControl(optimizer = "bobyqa", 
#                           optCtrl = list(maxfun = 5e4)))
# par(mfrow=c(3, 1))
# plot(mm_Gamma_OA_RT)
# plot(density(residuals(mm_Gamma_OA_RT)))
# qqPlot(residuals(mm_Gamma_OA_RT))
# summary(mm_Gamma_OA_RT)

# Result plots ----
long_all_MS |> 
   group_by(grp, 
            perspective, 
            switchStatus) |> 
   summarise(n = n(), 
             mean = mean(value), 
             sd = sd(value), 
             se = sd / sqrt(n)) |>
   ggplot(mapping = aes(x = perspective, 
                        y = mean, 
                        fill = switchStatus)) +
   geom_bar(stat = "identity",
            position = position_dodge(width = 0.9)) +
   geom_errorbar(mapping = aes(ymin = mean - se, 
                               ymax = mean + se),
                 position = position_dodge(width = 0.9),
                 width = 0.5,
                 size = 0.5) +
   #geom_signif(comparisons = list(c("Allo", "Ego")), 
   #            map_signif_level = TRUE) +
   coord_cartesian(ylim = c(0.5, 1.0)) + 
   facet_wrap(. ~ grp, 
              ncol = 2,
              strip.position = "top") +
   theme_classic() + 
   theme(axis.text = element_text(size = 14),
         axis.title = element_text(size = 14),
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_blank(),
         strip.text = element_text(size = 18),
         strip.text.y = element_blank()) + 
   scale_fill_manual(values=c("steelblue4", "steelblue2"), 
                     name = "Switch status") +
   xlab("Test perspective") +
   ylab("Mean MS") 

long_all_RT |> 
   group_by(grp, 
            perspective, 
            switchStatus) |> 
   summarise(n = n(), 
             mean = mean(value), 
             sd = sd(value), 
             se = sd / sqrt(n)) |>
   ggplot(mapping = aes(x = perspective, 
                        y = mean, 
                        fill = switchStatus)) +
   geom_bar(stat = "identity",
            position = position_dodge(width = 0.9)) +
   geom_errorbar(mapping = aes(ymin = mean - se, 
                               ymax = mean + se),
                 position = position_dodge(width = 0.9),
                 width = 0.5,
                 size = 0.5) +
   coord_cartesian(ylim = c(0, 15)) + 
   facet_wrap(. ~ grp, 
              ncol = 2,
              strip.position = "top") +
   theme_classic() + 
   theme(axis.text = element_text(size = 14),
         axis.title = element_text(size = 14),
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_blank(),
         strip.text = element_text(size = 18),
         strip.text.y = element_blank()) + 
   scale_fill_manual(values=c("steelblue4", "steelblue2"), 
                     name = "Switch status") +
   xlab("Test perspective") +
   ylab("Mean RT (s)") +
   scale_y_continuous(expand = c(0, 0))

# YA LMM on MS
mm_YAs_MS <- lmer(formula = value ~  perspective * switchStatus + (1 | ID),
                  data = long_all_MS |> 
                    subset(grp %in% "YA"))

# resid vs fitted
mm_YAs_MS |>
   plot(type = c("p", "smooth"), 
        col.line = 2)
# scale-location
mm_YAs_MS |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(2, 2))
# resid density
mm_YAs_MS |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
mm_YAs_MS |> 
   residuals() |>
   qqPlot()

summary(mm_YAs_MS)
confint(mm_YAs_MS)

emmip(mm_YAs_MS, perspective ~ switchStatus)

emm_YAs_MS <- emmeans(object = mm_YAs_MS, 
                     specs = pairwise ~ switchStatus | perspective,
                     lmer.df = "satterthwaite")
emm_YAs_MS$emmeans |> pairs(adjust = "none") 
emm_YAs_MS |> plot(comparisons = TRUE)

# OA LMM on MS
mm_OAs_MS <- lmer(formula = value ~  perspective * switchStatus + (1 | ID),
                  data = long_all_MS |> 
                     subset(grp %in% "OA"))

# resid vs fitted
mm_OAs_MS |>
   plot(type = c("p", "smooth"), 
        col.line = 2)
# scale-location
mm_OAs_MS |>
   plot(sqrt(abs(resid(.))) ~ fitted(.),
        type = c("p", "smooth"),
        col.line = 2)
par(mfrow = c(2, 2))
# resid density
mm_OAs_MS |> 
   residuals() |>
   density() |> 
   plot()
# resid qq
mm_OAs_MS |> 
   residuals() |>
   qqPlot()

summary(mm_OAs_MS)
confint(mm_OAs_MS)

emmip(mm_OAs_MS, perspective ~ switchStatus)

emm_OAs_MS <- emmeans(object = mm_OAs_MS, 
                      specs = pairwise ~ switchStatus | perspective,
                      lmer.df = "satterthwaite")
emm_OAs_MS$emmeans |> pairs(adjust = "none") 
emm_OAs_MS |> plot(comparisons = TRUE)



emms1 <- emmeans(object = mm_Gauss_MS_fin, ~ grp : perspective | switchStatus,
                 lmer.df = "satterthwaite")
pairs(emms1, by = c("perspective", "switchStatus"))

install.packages("modelbased")
library(modelbased)

estimate_contrasts(model = mm_Gauss_MS_fin, 
                   contrast = "grp", 
                   at = c("perspective", "switchStatus"), 
                   method = "pairwise", 
                   adjust = "none")

# POWER ----
nRepMeas <- 4
nGrps <- 2
nSubsPerGrp <- 64
nSubs <- nGrps * nSubsPerGrp
artificial_data <- expand.grid(MS = (1:4), ID = (1:nSubs))
artificial_data["grp"] <- c(rep(-0.5, nSubsPerGrp * nRepMeas), 
                            rep(0.5, nSubsPerGrp * nRepMeas)) |> 
   factor()
artificial_data["perspective"] <- rep(c(-0.5, -0.5, 0.5, 0.5), nSubs) |> 
   factor()
artificial_data["switchStatus"] <- rep(c(-0.5, 0.5, -0.5, 0.5), nSubs) |> 
   factor()

fixed_effects <- c(0.5, 0.05, -0.10, 0.05, 0.05, 0.025, -0.025, -0.025) # need one for each cond
random_variance <- list(0.1)

formula <- formula(MS ~ grp * perspective * switchStatus + (1 | ID))

artificial_glmer <- simr::makeGlmer(formula,
                                    fixef = fixed_effects,
                                    VarCorr = random_variance,
                                    family = "gaussian",
                                    data = artificial_data)
artificial_glmer |> summary()

power_simr <- simr::powerSim(fit = artificial_glmer,
                             test = fixed("grp:perspective", "s"),
                             nsim = 100)


# PARIETAL COORDINATES FOR VBM ANALYSIS ----
# Coordinates from a widely-cited review of functional neuromaging evidence of 
# precuneus function - https://academic.oup.com/brain/article/129/3/564/390904.
#
# Note that these coordinates are for left precuneus and in Talairach coordinates
imageryPrecCoords <- list(x = c(-10, -18, -4, -3, -8, -18, -17, -23, -18, -14, -1, -4),
                          y = c(-52, -70, -52, -74, -78, -53, -59, -61, -58, -70, -60, -82),
                          z = c(68, 69, 52, 34, 36, 54, 54, 51, 55, 28, 38, 40))
avgImageryPrecCoord <- lapply(imageryPrecCoords, mean) # MNI: -12, -68, 53

retrievalPrecCoords <- list(x = c(-14, -6, -2, 0, -8, -6, -12, 0),
                            y = c(-58, -68, -54, -66, -76, -76, -64, -64),
                            z = c(32, 36, 32, 33, 32, 28, 46, 44))
avgRetrievalPrecCoords <- lapply(retrievalPrecCoords, mean) # MNI: -6, -66, 35

selfPrecCoords <- list(x = c(-3, -6, 0, -4, -10, 0, 0, -6, -4, -6),
                       y = c(-47, -53, -56, -52, -48, -48, -66, -58, -64, -60),
                       z = c(31, 31, 56, 24, 64, 33, 34, 50, 32, 30))
avgSelfPrecCoords <- lapply(selfPrecCoords, mean) # MNI: -5, -58, 42

avgPrecCoords <- list(x = mean(c(avgImageryPrecCoord$x, # MNI: -8, -66, 45
                                 avgRetrievalPrecCoords$x,
                                 avgSelfPrecCoords$x)),
                      y = mean(c(avgImageryPrecCoord$y, 
                                 avgRetrievalPrecCoords$y,
                                 avgSelfPrecCoords$y)),
                      z = mean(c(avgImageryPrecCoord$z, 
                                 avgRetrievalPrecCoords$z,
                                 avgSelfPrecCoords$z)))
