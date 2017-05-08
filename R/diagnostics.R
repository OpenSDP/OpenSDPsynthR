# # # Simulation diagnostics
# #
# # # Ever FRL percentage
#
# simouts$stu_year %>% group_by(sid) %>%
#   summarize(everFRL = ifelse(any(frpl == "1"), 1, 0)) %>%
#   select(everFRL) %>% unlist %>% table
#
# simouts$stu_year %>% group_by(sid) %>%
#   summarize(everELL = ifelse(any(ell == "1"), 1, 0)) %>%
#   select(everELL) %>% unlist %>% table
#
# simouts$stu_year %>% group_by(sid) %>%
#   summarize(everIEP = ifelse(any(iep == "1"), 1, 0)) %>%
#   select(everIEP) %>% unlist %>% table
#
#
# simouts$stu_year %>% group_by(sid) %>%
#   summarize(everGifted = ifelse(any(gifted == "1"), 1, 0)) %>%
#   select(everGifted) %>% unlist %>% table
#
# table(simouts$stu_year$enrollment_status)
