SequencesFinder <- function() {
  this <- list();
  class(this) <- append(class(this), "SequencesFinder");
  return(this);
}

divide_sequences_by_pattern <- function(sequences_finder, pattern, list_of_sequences) {
  UseMethod("divide_sequences_by_pattern", sequences_finder);
}

find_sequences_with_class_after_pattern <- function(sequences_finder, pattern, class, list_of_sequences) {
  UseMethod("find_sequences_with_class_after_pattern", sequences_finder);
}

find_sequences_with_class <- function (sequences_finder, class, list_of_sequences) {
  UseMethod("find_sequences_with_class", sequences_finder);
}

contains <- function(sequences_finder, candidate_pattern, pattern) {
    UseMethod("contains", sequences_finder);
}

divide_sequences_by_pattern.SequencesFinder <- function (sequences_finder, pattern, list_of_sequences) {
  sequences_without_pattern <- list();
  sequences_with_pattern <- list();
  sequences_with_pattern_id <- 1;
  sequences_without_pattern_id <- 1;
  for (sequence in list_of_sequences) {
    support <- contains(sequences_finder, pattern, sequence);
    if (support) {
      sequences_with_pattern[[sequences_with_pattern_id]] <- sequence;
      sequences_with_pattern_id <- sequences_with_pattern_id + 1;
    } else {
      sequences_without_pattern[[sequences_without_pattern_id]] <- sequence;
      sequences_without_pattern_id <- sequences_without_pattern_id + 1;
    }
  }
  result <- list(with_pattern = sequences_with_pattern, without_pattern = sequences_without_pattern);
  return (result);
}

find_sequences_with_class_after_pattern.SequencesFinder <- function (sequences_finder, pattern, class, list_of_sequences) {
  result <- list();
  result_id <- 1;
  pattern_item <- pattern$sequence$items[[length(pattern$sequence$items)]];
  for (sequence in list_of_sequences) {
    last_id <- NA;
    sequence_item_id <- 1;
    for (sequence_item in sequence$sequence$items) {
      is_containing <- pattern_item %in% sequence_item
      if (length(is_containing[is_containing == TRUE]) == length(pattern_item)) {
        if(is.na(last_id)){
          last_id <- sequence_item_id;
        }
      }
      sequence_item_id <- sequence_item_id + 1;
    }
    class_id <- 1;
    indexes <- which(sequence$sequence$classes == c(class));
    if (length(indexes[indexes >= last_id]) > 0) {
      result[[result_id]] <- sequence;
      result_id <- result_id + 1;
    }
  }
  return (result);
}

find_sequences_with_class.SequencesFinder <- function (sequences_finder, class, list_of_sequences) {
  result <- list();
  result_id <- 1;
  for (sequence in list_of_sequences) {
      is_containing <- c(class) %in% sequence$sequence$classes;
      if (length(is_containing[is_containing == TRUE]) > 0) {
          result[[result_id]] <- sequence;
          result_id <- result_id + 1;
      }
  }
  return (result);
}

contains.SequencesFinder <- function (sequences_finder, candidate_pattern, pattern) {
    find_all_indexes_of_pattern_in_sequence <- function (candidate_pattern, pattern) {
        result <- list();
        result_id <- 1;
        for (candidate_pattern_item in candidate_pattern$sequence$items) {
            index <- 1
            indexes <- c();
            for (pattern_item in pattern$sequence$items) {
                is_containing <- is.na(match(unlist(pattern_item), unlist(candidate_pattern_item)));
                if (length(is_containing[is_containing == FALSE]) == length(unlist(candidate_pattern_item))) {
                    indexes <- c(indexes, index);
                }
                index <- index + 1;
            }
            result[[result_id]] <- list(pattern = candidate_pattern, indexes = unique(indexes));
            result_id <- result_id + 1;
        }
        return(result);
    }

    check_support <- function(patterns_to_indexes) {
        if (length(patterns_to_indexes) == 0) {
            return(F);
        };
        if (length(patterns_to_indexes) == 1 && length(patterns_to_indexes[[1]]$indexes) >= 1) {
            return(T);
        };
        if (length(patterns_to_indexes) == 1 && is.null(patterns_to_indexes[[1]]$indexes)) {
            return(F);
        };
        are_all_elements_supported <- F;
        vector_of_indexes <- c(length(patterns_to_indexes) : 2);
        for (id in vector_of_indexes) {
            prv_pattern <- patterns_to_indexes[[id - 1]];
            pattern     <- patterns_to_indexes[[id]];
            if (is.null(pattern) && is.null(prv_pattern)) {
                return (F);
            }
            is_any_larger <- F;
            for (index in pattern$indexes) {
                v <- prv_pattern$indexes[prv_pattern$indexes < index];
                if (length(v) > 0) {
                    pattern$indexes <- max(pattern$indexes);
                    prv_pattern$indexes <- prv_pattern$indexes[prv_pattern$indexes < pattern$indexes];
                    patterns_to_indexes[[id]] <- pattern;
                    patterns_to_indexes[[id - 1]] <- prv_pattern;
                    is_any_larger <- T;
                }
            }
            if  (!is_any_larger) {
                return (F);
            }
        }
        return (T);
    }
    patterns_to_indexes <- find_all_indexes_of_pattern_in_sequence(candidate_pattern, pattern);
    support        <- check_support(patterns_to_indexes);
    return (support);
}