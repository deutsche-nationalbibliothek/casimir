options::define_options(
  "Warnings about data inconsistencies will be silenced.",
  ignore_inconsistencies = FALSE,
  "Display progress bars for iterated computations (like bootstrap CI or
  PR-Curves)",
  progress = FALSE,
  "Verbose reorting of computation steps for debugging",
  verbose = FALSE,
  "Perform replacement of dots in grouping columns. Disable for faster
   computation, if you can make sure that all columns used for grouping
   (doc_id, label_id, doc_groups, label_groups) do not contain dots",
  check_group_names = TRUE,
  "Should empty levels of factor variables be droped in grouped set retrieval
  computation?",
  drop_empty_groups = TRUE,
  "In macro averaged results (doc-avg, subj-avg) it may occur that some
   instances have no predictions or no gold-standard. In these cases calculating
   precision and recall may lead to zero divisions. CASIMiR standardly removes
   these missing values from macro averages, leading to a smaller `support`
   (count of instances that were averaged). Other implementations of macro
   averaged precision and recall default to 0 in these cases. This option
   will allow to control the default. Set any value between 0 and 1.",
  replace_zero_division_with = NULL
)
