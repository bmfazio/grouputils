test_that("Every person is assigned at most one group", {
  conditions <- expand.grid(groups = 5:10,
                            people = seq(5, 40, 5),
                            size = 2:6,
                            choicefreq = c(0.1, 0.2, 0.5))
  conditions <- conditions[rep(1:nrow(conditions), each = 100),]

  set.seed(1249238)
  sims <- apply(conditions, 1, \(x) {
    max(apply(groupmake(sim_data(x[1], x[2], x[4]), x[3]), 2, sum))
  })

  expect_equal(sims, rep(1, nrow(conditions)), ignore_attr = TRUE)
})
