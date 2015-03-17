#' Plot the tree associated with a phyloseq object, with node sizes given by
#'  the counts aggregated over sample_feature
#'
#' @title Plot the tree associated with a phyloseq object, with node sizes given by
#'  the counts aggregated over sample_feature
#'
#' @param phyloseq_object The phyloseq object
#' @param sample_feature The feature along which counts are aggregated over.
#'    Must be a column in the phyloseq sample_data.
#' @param width The width of the desired tree, in pixels. Automatically resizes
#'    when refreshing the page in the browser.
#' @param height The height of the desired tree, in pixels. Automatically resizes
#'    when refreshing the page in the browser.
#' @param duration The transition time between events in the output figure.
#'    Defaults to .7 seconds.
#' @examples
#'    library("phyloseq")
#'    data("esophagus")
#'    phyloseqD3(esophagus)
#'    data("GlobalPatterns")
#'    gpac <- subset_taxa(GlobalPatterns, Family=="Rhodobacteraceae")
#'    phyloseqD3(gpac, "SampleType")
#'
#' @importFrom phyloseq phy_tree
#' @importFrom ape node.depth.edgelength
#' @importFrom htmlwidgets sizingPolicy createWidget
#'
#' @export
phyloseqD3 <- function(phyloseq_object, sample_feature, width=NULL, height=NULL,
                       duration=700) {

    # Extract tree and label the internal nodes if necessary
    counts <- TreeCountsWrapper(phyloseq_object, sample_feature)
    tree <- phyloseq::phy_tree(phyloseq_object)
    if(is.null(tree$node.label)) {
        tree$node.label <- 1:ape::Nnode(tree)
    }

    # Create the edgelist, and label with edgelengths and bootstrap p-values
    names_map <- c(c(tree$tip.label), c(tree$node.label))
    edgelist <- data.frame(tree$edge)
    colnames(edgelist) <- c("parent", "child")
    edgelist$length <- tree$edge.length
    edgelist$boot <- tree$boot

    # Create the desired json representation, by far the most time consuming
    # part of plotting, since this is done recursively in R
    max_tip <- length(tree$tip.label)
    tree_string <- TreeString("", names_map, edgelist, max_tip + 1, counts)

    # Scales are determined depending on the size of the tree
    max_depth <- max(ape::node.depth.edgelength(tree))
    max_group_counts <- max(counts$counts)

    # forward options using x
    x = list(tree_string=tree_string,
        duration=duration,
        max_depth=max_depth,
        max_group_counts=max_group_counts)

    # create widget
    widget <- htmlwidgets::createWidget(
        name = 'phyloseqD3',
        x = x,
        width = width,
        height = height,
        sizingPolicy = htmlwidgets::sizingPolicy(
            padding = 0,
            browser.fill = TRUE
        ),
        package = 'phyloseqD3'
    )
    return (widget)
}


#' Widget output function for use in Shiny
#'
#' @export
phyloseqD3Output <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'phyloseqD3', width, height, package = 'phyloseqD3')
}

#' Widget render function for use in Shiny
#'
#' @export
renderPhyloseqD3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, phyloseqD3Output, env, quoted = TRUE)
}
