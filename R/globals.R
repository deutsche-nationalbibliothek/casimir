utils::globalVariables(
  c("prec", "rec", "rprec", "f1", "prec_value", "rec_value",
    "prec_support", "rec_support", "n_docs",
    "gold", "suggested", "tp", "fp", "n_gold", "n_suggested", "label_weight",
    "fn", "delta_relevance", "relevance", "grp_id",
    "ideal", "support",
    "grp_names", "rank_gold", "rprec_deno",
    "f1_max", # for optimize_cutoff argument in compute_pr_curve
    "score", "doc_id", "value", "metric", "ndcg", "dcg", "idcg", "n_pred",
    "score_tp", "L", "lrap",
    "discount", "gain"))
