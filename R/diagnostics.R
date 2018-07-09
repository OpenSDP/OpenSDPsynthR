# # # # # # Simulation diagnostics
# # # # #
# # # # # # Ever FRL percentage
# # #
# # simouts$stu_year %>% group_by(sid) %>%
# #   summarize(everFRL = ifelse(any(frpl == "1"), 1, 0)) %>%
# #   select(everFRL) %>% unlist %>% table
# #
# # simouts$stu_year %>% group_by(sid) %>%
# #   summarize(everELL = ifelse(any(ell == "1"), 1, 0)) %>%
# #   select(everELL) %>% unlist %>% table
# #
# # simouts$stu_year %>% group_by(sid) %>%
# #   summarize(everIEP = ifelse(any(iep == "1"), 1, 0)) %>%
# #   select(everIEP) %>% unlist %>% table
# #
# #
# # simouts$stu_year %>% group_by(sid) %>%
# #   summarize(everGifted = ifelse(any(gifted == "1"), 1, 0)) %>%
# #   select(everGifted) %>% unlist %>% table
# #
# # table(simouts$stu_year$enrollment_status)
# # table(simouts$stu_year$grade_advance)
#
# # ggplot(assess, aes(x = math_ss, y = math_ssb, group = frpl, color = frpl)) +
# #   geom_point(alpha = I(0.3)) + geom_smooth(se = FALSE) +
# #   geom_abline(slope = 1, intercept= 0)
# # ggplot(assess, aes(x = math_ssb, group = frpl, color = frpl)) +
# #   geom_density(alpha = I(0.3)) + facet_wrap(~age)
# # assess$math_ssc <- mapply(perturb_race, assess$math_ssb, assess$Race, assess$math_sd)
#
# #
# simoutsA <- simpop(nstu = 1000L, seed = 488234,
#                    control = sim_control(nschls = 3L, minyear=1990,
#                                          maxyear=2010))
#
# # Document that these race codes need to be replace din perturb functions and the like
# simoutsB <- simpop(nstu = 1000L, seed = 488234,
#                    control = sim_control(nschls = 3L, race_groups = c("Black", "White", "Hispanic"),
#                                          race_prob = c(0.3, 0.6, 0.1)))

# mod_sim <- do.call(gen_outcome_model, sim_control()$ps_sim_parameters)
# mod_sim$sim_model@resp$family$linkinv(unlist(ranef(mod_sim$sim_model)$clustID))

#
# simoutsC <- simpop(nstu = 1000L, seed = 488234,
#                    control = sim_control(nschls = 3L, n_cohorts = 5L))

# tm <- matrix(
#   c(800, 20, 5, 800),
#   nrow = 2,
#   byrow = TRUE,
#   dimnames = list(c("1", "0"), c("1", "0"))
# )
# mytm <- tm_convert(tm)
# myGL <- list(GROUPVARS = "ALL",
#              "ALL" = list(f = make_markov_series,
#                           pars = list(tm = mytm))
#              )
#
#
# simoutsD <- simpop(nstu = 1000L, seed = 488234,
#                    control = sim_control(nschls = 3L,
#                                          gifted_list = myGL))
#
#
#
#
# myGradSim <- list(
#     fixed = ~ 1 + math_ss + scale_gpa + gifted + iep + frpl + ell + male,
#     random_var = 0.8948,
#     cov_param = list(
#       dist_fun = c("rnorm", "rnorm", rep("rbinom", 5)),
#       var_type = rep("lvl1", 7),
#       opts = list(
#         list(mean = 0, sd = 1),
#         list(mean = 0, sd = 1),
#         list(size = 1, prob = 0.1),
#         list(size = 1, prob = 0.2),
#         list(size = 1, prob = 0.45),
#         list(size = 1, prob = 0.1),
#         list(size = 1, prob = 0.47)
#       )
#     ),
#     cor_vars = c(
#       0.5136, 0.453, -0.276, -0.309, -0.046, -0.033,
#       0.2890, -0.1404, -0.2674, -0.0352,-0.1992,
#       -0.1354, -0.2096, -0.0305, -0.0290,
#       0.1433, -0.0031, 0.1269,
#       0.0601, 0.0066,
#       0.0009
#     ),
#     fixed_param = c(
#       1.6816, 0.30764, 1.05872, -0.07352, -0.07959,
#       -0.331647,-0.22318254, 0.0590
#     ),
#     ngrps = nschls + 5,
#     unbalanceRange = c(100, 1500)
# )
#
# simouts <- simpop(nstu = 4000L, seed = 53232,
#                   control = sim_control(nschls = 12L,
#                                         grad_sim_parameters = ))
