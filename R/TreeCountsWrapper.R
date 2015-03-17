#' Extract counts along nodes in phyloseq tree, with counts aggregated over
#'  sample_feature
#' @title Extract counts along nodes in phyloseq tree, with counts aggregated
#'  over sample_feature
#'
#' @param phyloseq_object The phyloseq object
#' @param sample_feature The feature along which counts are aggregated over.
#'    Must be a column in the phyloseq sample_data.
#' @examples
#'    library("phyloseq")
#'    data("esophagus")
#'    TreeCountsWrapper(esophagus)
#'    data("GlobalPatterns")
#'    gpac <- subset_taxa(GlobalPatterns, Family=="Rhodobacteraceae")
#'    TreeCountsWrapper(gpac, "SampleType")
#'
#' @importFrom phyloseq phy_tree otu_table
#' @importFrom plyr colwise
TreeCountsWrapper <- function(phyloseq_object, sample_feature=NULL) {
    tree <- phyloseq::phy_tree(phyloseq_object)

    if(is.null(tree$node.label)) {
        tree$node.label <- 1:tree$Nnode
    }

    # Compute counts across the tree, aggregating over the sample_feature,
    # if they are provided, or just the original samples
    otus_df <- data.frame(phyloseq::otu_table(phyloseq_object))
    otus <- plyr::colwise(as.integer)(otus_df)
    if(is.null(sample_feature)) {
        aggr_var <- colnames(otus)
        names(aggr_var) <- aggr_var
    } else {
        aggr_var <- unlist(sample_data(phyloseq_object)[, sample_feature])
        aggr_var <- as.character(aggr_var)
    }
    otus$OTU <- rownames(otus_df)
    counts <- TreeCounts(tree, otus, aggr_var)
    return (counts)
}
