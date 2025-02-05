# reconciliation works with uneven numbers

    Code
      reconcile_clusterings_mapping(primary_cluster_assignment,
        alt_cluster_assignment, one_to_one = TRUE)
    Condition
      Error in `reconcile_clusterings_mapping()`:
      ! The package "RcppHungarian" is required.

# reconciliation errors for uneven lengths

    Code
      reconcile_clusterings_mapping(letters, letters[1:10])
    Condition
      Error in `reconcile_clusterings_mapping()`:
      ! The package "RcppHungarian" is required.

