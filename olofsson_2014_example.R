# olofsson example

conf_mat <- as.matrix(data.frame(
  deforestation = c(66,0,1,2),
  forest_gain	= c(0,55,0,1),
  stable_forest = c(5,8,153,9),
  stable_non_forest = c(4,12,11,313)))


mapped_areas <- c(200000, 150000, 3200000, 6450000)
mapped_areas_p <- mapped_areas / sum(mapped_areas)

# p_ij matrix
conf_mat_nij <- prop.table(conf_mat, 1)
conf_mat_pij <- matrix(mapped_areas_p, nrow = 4, ncol = 4) * conf_mat_nij

# overall accuracy
oa <- sum(diag(conf_mat_pij))
# ovarall accuracy variance
users <- diag(conf_mat_pij) / rowSums(conf_mat_pij)
oa_v <- sum(((mapped_areas_p^2 * users) * (1 - users)) / (rowSums(conf_mat) - 1))
# overall accuracy 95% CI
oa_95 <- 1.96 * sqrt(oa_v * oa)

# area estimates
areas_pk <- colSums(conf_mat_pij)
areas_est <- areas_pk * sum(mapped_areas)
# area estimate standard error
areas_std_err <- sqrt(
  colSums(mapped_areas_p^2 * 
            ((conf_mat_nij * (1 - conf_mat_nij)) /
               (rowSums(conf_mat) - 1)))
  )
# areas 95% CI
areas_95 <- 1.96 * (areas_std_err * sum(mapped_areas))
areas_95_p <- areas_95 / sum(mapped_areas)

