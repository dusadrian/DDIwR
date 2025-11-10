#' @name makeDDITree
#'
#' @title Build a simplified DDI Codebook tree
#'
#' @description
#' Builds a tree-shaped representation of the DDI Codebook 2.6 element
#' hierarchy using only element names and their title descriptions.
#' The result is a nested list where each node is labeled as
#' "<element>: <title>". Leaf nodes are returned as empty lists so every
#' level consistently contains lists.
#'
#' @return A nested (tree) list of labels in the form "<element>: <title>",
#' plus a list of all elements.
#'
#' @param root Character, the root element to start from (default `"codeBook"`).
#'
#' @examples
#'
#' # Build the simplified tree as an R list
#' tree <- makeDDITree()
#'
#' @export
`makeDDITree` <- function(root = "codeBook") {
    DDIC <- get("DDIC", envir = cacheEnv)

    if (!is.list(DDIC) || is.null(DDIC[[root]])) {
        admisc::stopError(sprintf("Root element '%s' not found in DDIC.", root))
    }

    # Build children list with consistent list structure:
    # - Every child is added as a named list entry (label -> list of children)
    # - Leaves use an empty list as their value
    build_children <- function(name, visited) {
        spec <- DDIC[[name]]

        ch <- spec$children
        if (is.null(ch) || length(ch) == 0) {
            return(NULL)
        }

        child_names <- unique(unlist(ch, use.names = FALSE))
        if (length(child_names) == 0) {
            return(NULL)
        }

        res <- list()
        for (cn in child_names) {
            if (cn %in% visited || is.null(DDIC[[cn]])) {
                next
            }
            cspec <- DDIC[[cn]]
            label <- paste0(cn, ": ", cspec$title)
            grand <- build_children(cn, c(visited, name))
            if (is.null(grand)) grand <- list()
            tmp <- list(grand)
            names(tmp) <- label
            res <- c(res, tmp)
        }
        res
    }

    root_spec <- DDIC[[root]]
    root_label <- paste0(root, ": ", root_spec$title)
    tree <- list()
    tree[[root_label]] <- build_children(root, visited = character())
    tree$elements <- DDIC

    return(tree)
}
