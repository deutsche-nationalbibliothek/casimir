
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
  check_group_names = TRUE

)
