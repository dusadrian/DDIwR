#' @name makeDDITree
#'
#' @title Build the DDI Codebook tree structure
#'
#' @description
#' Builds a tree-shaped representation of the DDI Codebook 2.6 element
#' hierarchy using the in-package structure definition.
#'
#' @return A nested list representing the element tree.
#'
#' @param root Character, the root element to start from (default `"codeBook"`).
#'
#' @examples
#'
#' # Build the tree as an R list
#' tree <- makeDDITree()
#'
#' @export
`makeDDITree` <- function(
    root = "codeBook"
) {
    DDIC <- get("DDIC", envir = cacheEnv)
    max_depth <- Inf

    # include element metadata like `title`,`optional`,
    # `repeatable`, `deprecated`, `recommended`, `type` (default TRUE).
    include_meta <- TRUE

    if (!is.list(DDIC) || is.null(DDIC[[root]])) {
        admisc::stopError(sprintf("Root element '%s' not found in DDIC.", root))
    }

    build_node <- function(name, depth, path_stack) {
        if (is.null(DDIC[[name]])) {
            # Unknown element in schema; return minimal node
            return(list(name = name))
        }

        spec <- DDIC[[name]]
        node <- list(name = name)

        if (isTRUE(include_meta)) {
            node$type <- spec$type
            node$title <- spec$title
            node$optional <- isTRUE(spec$optional)
            node$repeatable <- isTRUE(spec$repeatable)
            node$recommended <- isTRUE(spec$recommended)
            node$deprecated <- isTRUE(spec$deprecated)
        }

        if (!is.null(spec$attributes)) {
            # Keep a lightweight view of attributes: name + type (and optional)
            atts <- spec$attributes
            node$attributes <- lapply(names(atts), function(an) {
                list(
                    name = an,
                    type = atts[[an]]$type,
                    optional = isTRUE(atts[[an]]$optional),
                    recommended = isTRUE(atts[[an]]$recommended),
                    deprecated = isTRUE(atts[[an]]$deprecated)
                )
            })
        }

        if (depth >= max_depth) {
            return(node)
        }

        ch <- spec$children
        if (is.null(ch) || length(ch) == 0) {
            return(node)
        }

        if (is.null(ch$choice)) {
            child_names <- unlist(ch)
            if (length(child_names)) {
                node$children <- lapply(child_names, function(cn) {
                    if (cn %in% path_stack) {
                        return(list(name = cn, ref = TRUE))
                    }
                    build_node(cn, depth + 1, c(path_stack, name))
                })
            }
        } else {
            choice_names <- ch$choice
            node$choice <- lapply(choice_names, function(cn) {
                if (cn %in% path_stack) {
                    return(list(name = cn, ref = TRUE))
                }
                build_node(cn, depth + 1, c(path_stack, name))
            })
            # Also include any non-choice children (if present)
            non_choice <- setdiff(unlist(ch), choice_names)
            if (length(non_choice)) {
                node$children <- lapply(non_choice, function(cn) {
                    if (cn %in% path_stack) {
                        return(list(name = cn, ref = TRUE))
                    }
                    build_node(cn, depth + 1, c(path_stack, name))
                })
            }
        }

        return(node)
    }

    tree <- build_node(root, depth = 0, path_stack = character(0))

    return(tree)
}

