# Takes about 45 seconds to run
test_that("Everyone assigned at most one group, no group exceeds target size", {
  conditions <- expand.grid(groups = 5:10,
                            people = seq(5, 40, 5),
                            size = 2:6,
                            choicefreq = c(0.1, 0.2, 0.5))
  conditions <- conditions[rep(1:nrow(conditions), each = 100),]

  set.seed(1249238)
  sims <- apply(conditions, 1, \(x) {
    result <- groupmake(sim_data(x[1], x[2], x[4]), x[3])
    c(
      max_groups = max(apply(result, 2, sum)) <= 1,
      max_size = max(apply(result, 1, sum)) <= x[3]
    )
  })

  expect_true(all(sims))
})
