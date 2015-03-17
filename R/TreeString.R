#' Generate a JSON string summarising a phyloseq tree and counts
#'
#' @param tree_string This function recursively builds the string representing
#'     the tree. It starts with "", and calls on an extended strings.
#' @param names_map A vector associating the numbers in the edgelist to names
#'     in the phylo.
#' @param edgelist An n x 3 data frame, with column names
#' @param parent An integer representing the parent node
#' @param child An integer representing the child node
#' @param length A double giving the length of each edge
#' @param cur_node This function recursively builds the string representing
#'     the tree. This starts with the index for the root node,  and calls
#'     on all the child nodes.
#' @param counts A data frame of counts, along every group.
#'
#' @return result_string A string giving a JSON representation of the tree.
#'     Similar in spirit to https://gist.github.com/mbostock/1093025
#'
#' @importFrom RJSONIO toJSON
#' @importFrom dplyr filter select
TreeString <- function(tree_string, names_map, edgelist, cur_node, counts) {

    # Extract children of current node index
    parent_and_children<- dplyr::filter(edgelist, parent==cur_node)
    n_children <- nrow(parent_and_children)
    cur_node_prefix <- paste0('{ \"name\": "', names_map[cur_node], '"')

    # Extract lengths and bootstrap values
    cur_length <- dplyr::filter(edgelist, child==cur_node)$length
    cur_boot <- dplyr::filter(edgelist, child==cur_node)$boot

    # Don't paste on "length:" when we're at the root, or "boot: ", when there
    # are no bootstrap values
    if(length(cur_length) > 0) {
        cur_node_prefix <- paste0(cur_node_prefix, ', \"length\": ', cur_length)
        if(length(cur_boot) > 0) {
            cur_node_prefix <- paste0(cur_node_prefix, ', \"boot\": ', cur_boot)
        }
    }

    # Extract counts string
    cur_counts <- dplyr::select(dplyr::filter(counts, OTU==names_map[cur_node]), group, counts)
    cur_counts_names <- cur_counts$group
    cur_counts <- cur_counts$counts
    names(cur_counts) <- cur_counts_names
    cur_counts_string <- RJSONIO::toJSON(cur_counts)

    # Append counts info onto string
    cur_node_prefix <- paste0(cur_node_prefix, ", \"counts\": ", cur_counts_string)

    # Base case: The current nodes is a terminal node, just give the name and
    # the length to that node in the form {name: "A", length: 1.2}
    if(n_children==0) {
        return (paste0(cur_node_prefix, '}'))
    } else {
        # Inductive step: The current node has children. Give it the form
        # {name: "A", length=1.2, branchset=[ TreeString(child1), ...,
        # TreeString(childk)]}, for each of the children
        children_strings <- vector(length=n_children)
        for(i in 1:n_children) {
            children_strings[i] <- TreeString(tree_string, names_map, edgelist,
                                              parent_and_children[i, "child"], counts)
        }

        # paste annotation to children strings
        children_strings <- paste0(children_strings, collapse=",")
        result_string <- paste0(cur_node_prefix, ", \"branchset\": [", children_strings, "]}")
        return (result_string)
    }
}
