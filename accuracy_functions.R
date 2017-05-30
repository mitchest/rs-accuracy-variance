source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}



# metrics -----------------------------------------------------------------

get_conf_mat <- function(reps, true_id, pred_class, data) {
  table(as.character(pred_class[[reps]]),
        as.character(data[true_id[[reps]], "veg_cl_tm"]))
}

percentage_agreement <- function(reps, true_id, pred_class, data) {
  sum(as.character(data[true_id[[reps]], "veg_cl_tm"]) == as.character(pred_class[[reps]])) / length(pred_class[[reps]])
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

# disagreemetns kind of stolen from {diffR} package
disagreement <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  #if(!all(dim(test_mat) == 4)) {return(NA)}
  1 - (sum(diag(conf_mat)) / sum(conf_mat))
}

quantity_disagreement <- function(reps, true_id, pred_class, data) {
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  #if(!all(dim(test_mat) == 4)) {return(NA)}
  sum(abs(apply(conf_mat, 1, sum) - apply(conf_mat, 2, sum))) / 2 / sum(conf_mat)
}

allocation_disagreement <- function(reps, true_id, pred_class, data) {
  #if(!all(dim(test_mat) == 4)) {return(NA)}
  conf_mat <- get_conf_mat(reps, true_id, pred_class, data)
  disagreement(reps, true_id, pred_class, data) - quantity_disagreement(reps, true_id, pred_class, data)
}



# collect metrics ---------------------------------------------------------

collect_metric_results <- function(this_row, get_this, iter_n, data) {
  print(get_this[this_row,])
  if (get_this$type[this_row] == "boot") {
    return(
      data.frame(
        perc_agr = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = percentage_agreement,
            iter_n[["boot"]][[get_this$tt[this_row]]],
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        entropy = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = entropy,
            iter_n[["boot"]][[get_this$tt[this_row]]],
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        purity = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = purity,
            iter_n[["boot"]][[get_this$tt[this_row]]],
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        quant_dis = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = quantity_disagreement,
            iter_n[["boot"]][[get_this$tt[this_row]]],
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        alloc_dis = unlist(
          lapply(
            X = 1:length(iter_n[["boot"]][[1]]),
            FUN = allocation_disagreement,
            iter_n[["boot"]][[get_this$tt[this_row]]],
            iter_n[["boot"]][[get_this$method[this_row]]],
            data)),
        type = get_this$type[this_row],
        method = get_this$method[this_row],
        scenario = get_this$scenario[this_row])
    )
  } else {
    return(
      data.frame(
        perc_agr = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = percentage_agreement,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]],
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        entropy = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = entropy,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]],
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        purity = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = purity,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]],
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        quantity_disagreement = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = quantity_disagreement,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]],
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$method[this_row]]],
            data)),
        allocation_disagreement = unlist(
          lapply(
            X = 1:length(iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[1]]),
            FUN = allocation_disagreement,
            iter_n[[get_this$scenario[this_row]]][[get_this$type[this_row]]][[get_this$tt[this_row]]],
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
