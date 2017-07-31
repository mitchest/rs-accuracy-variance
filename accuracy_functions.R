source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}



# metrics -----------------------------------------------------------------

get_conf_mat <- function(reps, true_id, pred_class, data) {
  table(as.character(pred_class[[reps]]),
        as.character(data[true_id[[reps]], "veg_cl_tm"]))
  # note danger here that it relies on always having at least one case for each class (that is, it relies on the alphabetical factor ordering to ensure confusion matrices are identical in structure)
}

percentage_agreement <- function(reps, true_id, pred_class, data) {
  # sum(as.character(data[true_id[[reps]], "veg_cl_tm"]) == as.character(pred_class[[reps]])) / length(pred_class[[reps]])
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  sum(diag(conf_mat)) / sum(conf_mat) # xtab method quicker?
}

cohens_kappa <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  if(dim(conf_mat)[1] != dim(conf_mat)[2]) {return(NA)}
  # props <- conf_mat / sum(conf_mat)
  # cor_prob <- sum(diag(props))
  # chance_prob <- sum( apply(props, 1, sum) * apply(props, 2, sum) )
  # below seems to be a touch quicker...
  cor_prob <- sum(diag(conf_mat)) / sum(conf_mat)
  chance_prob <- crossprod(colSums(conf_mat) / sum(conf_mat), rowSums(conf_mat) / sum(conf_mat))[1]
  (cor_prob - chance_prob)/(1 - chance_prob)
}

# entropy and purity stolen from {IntNMF} package
entropy <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  inner_sum <- apply(conf_mat, 1, function(x) {
    c_size <- sum(x)
    sum(x * ifelse(x != 0, log2(x/c_size), 0))
  })
  -sum(inner_sum)/(sum(conf_mat) * log2(ncol(conf_mat)))
}

purity <- function(reps, true_id, pred_class, data) {
  sum(apply(get_conf_mat(reps, true_id, pred_class, data), 1, max)) / length(pred_class[[reps]])
}

# disagreemetns kind of stolen from {diffeR} package
disagreement <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  if(dim(conf_mat)[1] != dim(conf_mat)[2]) {return(NA)}
  1 - (sum(diag(conf_mat)) / sum(conf_mat))
}

quantity_disagreement <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  if(dim(conf_mat)[1] != dim(conf_mat)[2]) {return(NA)}
  sum(abs(apply(conf_mat, 1, sum) - apply(conf_mat, 2, sum))) / 2 / sum(conf_mat)
}

allocation_disagreement <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  if(dim(conf_mat)[1] != dim(conf_mat)[2]) {return(NA)}
  disagreement(reps, true_id, pred_class, data) - quantity_disagreement(reps, true_id, pred_class, data)
}



# collect metrics ---------------------------------------------------------

collect_metric_results <- function(this_row, get_this, iter_n, data) {
  #sprint(get_this[this_row,])
  if (get_this$type[this_row] == "boot") {
    if (get_this$tt[this_row] %in% c("train", "test")) {
      true_id <- iter_n[["boot"]][[get_this$tt[this_row]]]
    } else {
      true_id <- rep(list(data$id), length(iter_n[["boot"]][[1]]))
    }
    return(
      data.frame(
        perc_agr = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = percentage_agreement,
            true_id,
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        kappa = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = cohens_kappa,
            true_id,
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        entropy = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = entropy,
            true_id,
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        purity = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = purity,
            true_id,
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        quant_dis = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = quantity_disagreement,
            true_id,
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        alloc_dis = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = allocation_disagreement,
            true_id,
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        type = get_this$type[this_row],
        method = get_this$method[this_row],
        scenario = get_this$scenario[this_row])
    )
  } else if (get_this$type[this_row] == "alldat") {
    return(data.frame(
      perc_agr = percentage_agreement(1, list(data$id), 
                                      list(iter_n[["alldat"]][[get_this$method[this_row]]]), data),
      kappa = cohens_kappa(1, list(data$id), 
                                      list(iter_n[["alldat"]][[get_this$method[this_row]]]), data),
      entropy = entropy(1, list(data$id), 
                        list(iter_n[["alldat"]][[get_this$method[this_row]]]), data),
      purity = purity(1, list(data$id), 
                      list(iter_n[["alldat"]][[get_this$method[this_row]]]), data),
      quant_dis = quantity_disagreement(1, list(data$id), 
                                        list(iter_n[["alldat"]][[get_this$method[this_row]]]), data),
      alloc_dis = allocation_disagreement(1, list(data$id), 
                                          list(iter_n[["alldat"]][[get_this$method[this_row]]]), data),
      type = get_this$type[this_row],
      method = get_this$method[this_row],
      scenario = get_this$scenario[this_row])
    )
  } else {
    if (get_this$tt[this_row] %in% c("train", "test")) {
      true_id <- iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]]
    } else {
      true_id <- rep(list(data$id), length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]))
    }
    return(
      data.frame(
        perc_agr = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = percentage_agreement,
            true_id,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        kappa = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = cohens_kappa,
            true_id,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        entropy = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = entropy,
            true_id,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        purity = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = purity,
            true_id,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        quantity_disagreement = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = quantity_disagreement,
            true_id,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        allocation_disagreement = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = allocation_disagreement,
            true_id,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        type = get_this$type[this_row],
        method = get_this$method[this_row],
        scenario = get_this$scenario[this_row])
    )
  }
}

collect_one_iteration <- function(iter_n, get_this, big_list, data) {
  print(paste0("Collecting iteration ", iter_n))
  print(Sys.time())
  rbindlist(lapply(
    X = 1:nrow(get_this),
    FUN = collect_metric_results,
    get_this,
    big_list[[iter_n]],
    data
  )) %>% mutate(iter_n = iter_n)
}



# plots -------------------------------------------------------------------

plot_pa_results <- function(x, data) {
  ggplot(data = data[data$iter_n %in% x,], aes(y = perc_agr)) +
    geom_boxplot(aes(x = type, colour = scenario, fill = method)) +
    scale_fill_manual(values = c("#fcbba1", "#fb6a4a", "#d4b9da", "#99d8c9", "#238b45")) +
    scale_colour_manual(values = c("#252525", "#e31a1c", "#3f007d"))
}

plot_train_test <- function(data, model_type, 
                            origins = c("all", "train", "test"),
                            structures = c("bootstrap", "random","block", "class", "class-space", "all-data"),
                            metrics = c("perc_agr", "kappa", "entropy", "purity", "quant_dis", "alloc_dis"),
                            quants = c(0.05,0.5,0.9), suffix = "") {
  plt <- data %>%
    filter(sample_origin %in% origins,
           sample_structure %in% structures,
           model == model_type,
           metric %in% metrics) %>%
    ggplot(., aes(y = value)) +
    geom_violin(aes(x = sample_origin, fill = sample_fraction), scale = "area", draw_quantiles = quants, lwd=0.25) +
    scale_fill_manual("Resampling design", values = c("#969696", "#969696", "#cb181d", "#fc9272", "#31a354")) +
    #scale_colour_manual("Sample type", values = c("#969696", "#fdae6b", "#d94801")) + 
    ylab("Metric value") + xlab("Stratification design") + theme_bw() +
    facet_grid(metric ~ sample_structure, scales = "free", space = "free", drop = T)
  ggsave(plot = plt, filename = paste0("plots/",model_type,suffix,".pdf"), device = "pdf", width = 20, height = 13)
}
