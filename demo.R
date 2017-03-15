
# frpl
tm_f <- matrix(c(900, 200, 300, 2000), nrow = 2, byrow=TRUE,
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_f/rowSums(tm_f))

# gifted
tm_g <- matrix(c(2000, 200, 20, 400), nrow = 2, byrow=TRUE,
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_g/rowSums(tm_g))
# iep
tm_i <- matrix(c(3000, 200, 200, 3000), nrow = 2, byrow=TRUE,
               dimnames = list(statesNames, statesNames))
make_markov_series(20, tm = tm_i/rowSums(tm_i))


make_markov_series(20, tm = tm_grade/rowSums(tm_grade), t0 = "1")



stu_year %<>% group_by(ID) %>% arrange(ID, year) %>%
  mutate(frpl = make_markov_series(n(), tm = tm_f/rowSums(tm_f)),
         gifted = make_markov_series(n(), tm = tm_g/rowSums(tm_g)),
         iep = make_markov_series(n(), tm = tm_i/rowSums(tm_i)),
         grade_adv = make_markov_series(n(), tm = tm_grade/rowSums(tm_grade)))
