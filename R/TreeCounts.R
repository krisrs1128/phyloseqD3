#' Recursively calculate the counts within each sample type for each
#' internal node in a tree
#'
#' @title Recursively calculate the counts within each sample type for each
#'     internal node in a tree
#' @param phy_tree A phylo describing the relationships between nodes. The tip labels
#'     must correspond to the OTU column in otus.
#' @param otus An data frame (or data.table) specifying the counts of each OTU
#'     in each sample. The column names must be sample names, and there must
#'     be a column OTU giving the OTU or internal node label for that row.
#' @param aggr_var How should we aggregate counts? A vector mapping sample
#'     names (vector names) in the otus to groups (vector values), so
#'     we can sum counts over those groups
#'
#' @return tree_counts A data frame whose rows are either internal nodes or tips,
#'     column names are groups defined in aggr_var, and ij^th element is the
#'     number of OTUs descending from the i^th node within group j in aggr_var.
#'
#' @importFrom reshape2 melt
#' @importFrom data.table data.table setnames setkey
#' @importFrom plyr numcolwise
#' @importFrom dplyr filter
TreeCounts <- function(phy_tree, otus, aggr_var) {

    # Convert otus to data.table and extract edgelist for tree
    otus <- data.table::data.table(otus)
    names_map <- c(phy_tree$tip.label, phy_tree$node.label)
    phy_edgelist <- data.table::data.table(names_map[phy_tree$edge[, 1]], names_map[phy_tree$edge[, 2]])
    data.table::setnames(phy_edgelist, c("parent", "child"))

    # Compute sum over groups, for just the tips
    otus$All <- as.integer(rowSums(plyr::numcolwise(function(x) x)(otus)))
    m_tips <- reshape2::melt(otus[otus$OTU %in% phy_tree$tip.label, ],
                             id.vars="OTU", variable.name="group",
                             value.name="counts")
    setkey(m_tips, OTU, group)
    aggr_var["All"] <- "All"
    m_tips$group <- aggr_var[m_tips$group]

    children <- m_tips[,list(counts=sum(counts)),by=list(OTU, group)]

    # Initialize result with just tips
    result <- children
    cur_el <- dplyr::filter(phy_edgelist, child %in% unique(children$OTU))

    # Compute internal node counts by summing over immediate children
    while(nrow(cur_el) > 0) {

        # Identify parents of the current children nodes
        setnames(cur_el, c("parent", "OTU"))
        parents_and_children <- merge(cur_el, children, by="OTU")

        # Compute the parent counts by summing over children
        data.table::setkey(parents_and_children, parent, group)

        children <- parents_and_children[,list(counts=sum(counts)),by=list(parent, group)]
        data.table::setnames(children, c("OTU", "group", "counts"))
        # Append to final result
        result <- rbind(result, children)

        # Update by setting parents into children, ascend up tree
        cur_el <- dplyr::filter(phy_edgelist, child %in% children$OTU)
    }

    # Final aggregation over OTUs, since it's possible we didn't sum both
    # children when traversing up tree (since one child might have been a tip
    # while the other was internal)
    result <- data.table(result)
    data.table::setkey(result, OTU, group)
    result <- result[, list(counts=sum(counts)), by=list(OTU, group)]

    return (result)
}
