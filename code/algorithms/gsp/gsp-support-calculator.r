GspSupportCalculator <- function() {
    this <- list();
    class(this) <- append(class(this), "GspSupportCalculator");
    return(this);
}

calculate_support <- function (gspSupportCalculator, pattern, list_of_sequences) {
    UseMethod("calculate_support", gspSupportCalculator);
}

calculate_support.GspSupportCalculator <- function (gspSupportCalculator, pattern, list_of_sequences) {
  is_any_index_of_item_larger_than_index_of_prv_item <- function(index, prv_obj, obj, patterns_to_indexes) {
      is_any_larger <- F;
      res <- list();
      for (obj_element in obj$indexes) {
          v <- prv_obj$indexes[prv_obj$indexes < obj_element];
          if (length(v) > 0) {
              obj$indexes <- max(obj$indexes);
              prv_obj$indexes <- prv_obj$indexes[prv_obj$indexes < obj$indexes];
              res[[index]] <- obj;
              res[[index - 1]] <- prv_obj;
              if (is_any_larger) {
                return (T);
              }
          }
      }
      return(F);
  }
  
  number_of_sequences <- length(list_of_sequences);
  number_of_supporting_sequences <- 0;
  sequences_finder <- SequencesFinder();
  for (sequence in list_of_sequences) {
      support <- contains(sequences_finder, pattern, sequence);
      if (support) {
          number_of_supporting_sequences <- number_of_supporting_sequences + 1;
      }
  }
  return(number_of_supporting_sequences / number_of_sequences);
}