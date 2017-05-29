source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

percentage_agreement <- function(reps, true_id, pred_class, data) {
  sum(as.character(data[true_id[[reps]], "veg_cl_tm"]) == as.character(pred_class[[reps]])) / length(pred_class[[reps]])
}

# entropy and purity stolen from {IntNMF} package
entropy <- function(reps, true_id, pred_class, data) {
  conf_mat <- table(as.character(pred_class[[reps]]),
                    as.character(data[true_id[[reps]], "veg_cl_tm"]))
  inner_sum <- apply(conf_mat, 1, function(x) {
    c_size <- sum(x)
    sum(x * ifelse(x != 0, log2(x/c_size), 0))
  })
  -sum(inner_sum)/(sum(conf_mat) * log2(ncol(conf_mat)))
}

purity <- function(reps, true_id, pred_class, data) {
  sum(apply(
    table(as.character(pred_class[[reps]]),
          as.character(data[true_id[[reps]], "veg_cl_tm"])),
    1, max)) / length(pred_class[[reps]])
}

collect_pa_results <- function(this_row, get_this, iter_n, data) {
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
        type = get_this$type[this_row],
        method = get_this$method[this_row],
        scenario = get_this$scenario[this_row])
    )
  }
}

collect_pa_iteration <- function(iter_n, get_this, big_list, data) {
  print(paste0("Collecting iteration ", iter_n))
  rbindlist(lapply(
    X = 1:nrow(get_this),
    FUN = collect_pa_results,
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
