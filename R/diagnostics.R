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
