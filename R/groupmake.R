#' Assign people to groups while trying to consider their preferences
#'
#' This function is guaranteed to assign people to groups of `target` size until
#' either people or available groups run out. If a group has more people who
#' want to be in it than the `target` size, a random selection will be made.
#' Some people may end up being assigned to groups they did not show a preference
#' for as a result and this result is not deterministic.
#'
#' @param data A matrix encoding group preferences, see `sim_data` for format
#' @param target An integer giving the exact amount of people each group should have
#'
#' @return A 0/1 matrix representing the final group assignments.
#' @export
#'
#' @examples
#' library(grouputils)
#' set.seed(6)
#' data <- sim_data(8, 13, 0.2)
#' groupmake(data, 2)
groupmake <- \(data, target) {
  left_g <- seq_len(nrow(data))
  left_p <- seq_len(ncol(data))

  while(length(left_p)>0&length(left_g)>0) {
    size <- apply(data[left_g, left_p, drop = FALSE], 1, sum)

    if(any(size == target)) {
      pick <- sensamp(left_g[size == target], 1)
      keep <- intersect(left_p, which(data[pick, ] == 1))
    } else {
      if(any(size < target)) {
        fixed <- data
        fixed[left_g[size > target],
              which(apply(data[size < target,,drop = FALSE], 2, sum) > 0)] <- 0
        data <- data*fixed
        size <- apply(data[left_g, left_p, drop = FALSE], 1, sum)
      }
      if(any(size > target)) {
        pick <- sensamp(left_g[size == max(size)], 1)
        keep <- sensamp(intersect(left_p, which(data[pick,] == 1)), target)
      } else {
        pick <- sensamp(left_g[size == max(size)], 1)
        keep <- intersect(left_p, which(data[pick,] == 1))
        keep <- c(keep, sensamp(setdiff(left_p, keep), target - max(size)))
      }
    }
    left_p <- setdiff(left_p, keep)
    left_g <- setdiff(left_g, pick)
    data[pick,] <- 0
    data[pick,keep] <- 1
    data[-pick,keep] <- 0
  }
  data
}

#' Simulate a group preference matrix
#'
#' @param n_grp Number of available gorups
#' @param n_ppl Number of people to sort into groups
#' @param p_choice Per-person probability of being interested in a given topic
#'
#' @return A 0/1 matrix encoding preferences. Groups as rows and people as columns.
#' @export
#'
#' @examples
#' set.seed(6)
#' grouputils::sim_data(8, 13, 0.2)
sim_data <- \(n_grp, n_ppl, p_choice = 0.5) {
  num_choices <- stats::rbinom(n_ppl, n_grp-1, p_choice) + 1
  id_choices <- sapply(num_choices, \(x)sample(seq_len(n_grp), x))
  sapply(id_choices, \(x){y<-numeric(n_grp);y[x]<-1;y})
}
