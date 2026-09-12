# Parent-specific XSD particles. The legacy children/optional/repeatable fields
# remain useful for discovery; contentModel is authoritative for validation.
ddiModelNames <- function(model) {
    if (identical(model$kind, "element")) {
        return(model$name)
    }

    unique(unlist(lapply(model$particles, ddiModelNames), use.names = FALSE))
}

# Bounds for one named child across a complete parent particle, not bounds for
# each alternative independently. In particular, a required choice does not
# require every alternative.
ddiModelBounds <- function(model, name) {
    if (identical(model$kind, "element")) {
        if (identical(model$name, name)) {
            return(c(model$min, model$max))
        }
        else {
            return(c(0, 0))
        }
    }

    bounds <- lapply(model$particles, ddiModelBounds, name = name)

    if (!length(bounds)) {
        return(c(0, 0))
    }

    bounds <- do.call(rbind, bounds)

    if (identical(model$kind, "choice")) {
        value <- c(min(bounds[, 1]), max(bounds[, 2]))
    }
    else {
        value <- colSums(bounds)
    }

    # 0 * Inf is zero occurrences here, not NaN.
    if (value[1] == 0 || model$min == 0) {
        minimum <- 0
    }
    else {
        minimum <- value[1] * model$min
    }

    if (value[2] == 0 || model$max == 0) {
        maximum <- 0
    }
    else {
        maximum <- value[2] * model$max
    }

    c(minimum, maximum)
}

# A partial match permits missing required children during incremental editing,
# while still enforcing maximum occurrences and choices. Full matches also
# enforce minima and sequence order. Memoized sets of offsets avoid choosing a
# branch greedily when a nested/repeated choice has several possible matches.
ddiModelMatches <- function(model, children, complete = TRUE) {
    count <- length(children)
    memo <- new.env(parent = emptyenv())

    matchParticle <- function(p, start, key) {
        cacheKey <- paste(key, start, sep = ":")

        if (exists(cacheKey, memo, inherits = FALSE)) {
            return(get(cacheKey, memo))
        }

        if (identical(p$kind, "element")) {
            available <- min(p$max, count - start)

            if (available > 0) {
                run <- children[start + seq_len(available)] == p$name
            }
            else {
                run <- logical()
            }

            maximum <- match(FALSE, run, nomatch = length(run) + 1L) - 1L

            if (complete) {
                minimum <- p$min
            }
            else {
                minimum <- 0
            }

            if (maximum >= minimum) {
                result <- start + seq.int(minimum, maximum)
            }
            else {
                result <- integer()
            }

            assign(cacheKey, result, memo)
            return(result)
        }

        body <- function(pos) {
            if (identical(p$kind, "choice")) {
                possible <- lapply(seq_along(p$particles), function(i) {
                    matchParticle(
                        p$particles[[i]],
                        pos,
                        paste0(key, ".", i)
                    )
                })

                return(unique(unlist(possible, use.names = FALSE)))
            }

            positions <- pos

            for (i in seq_along(p$particles)) {
                positions <- unique(unlist(lapply(positions, function(at) {
                    matchParticle(
                        p$particles[[i]],
                        at,
                        paste0(key, ".", i)
                    )
                }), use.names = FALSE))

                if (!length(positions)) {
                    break
                }
            }

            positions
        }

        if (complete) {
            minimum <- p$min
        }
        else {
            minimum <- 0
        }

        positions <- start

        if (minimum == 0) {
            accepted <- start
        }
        else {
            accepted <- integer()
        }

        repetitions <- 0

        while (length(positions) && repetitions < p$max) {
            nextPositions <- sort(unique(unlist(lapply(positions, body), use.names = FALSE)))
            repetitions <- repetitions + 1

            if (repetitions >= minimum) {
                accepted <- union(accepted, nextPositions)
            }

            if (identical(nextPositions, sort(positions))) {
                # Nullable particles reach a fixed point: further repetitions
                # can satisfy a minimum, but cannot consume any new input.
                if (p$max >= minimum) {
                    accepted <- union(accepted, nextPositions)
                }

                break
            }

            positions <- nextPositions
        }

        assign(cacheKey, accepted, memo)
        accepted
    }

    is.element(count, matchParticle(model, 0L, "root"))
}

# Sort singleton sequence slots as before, but preserve insertion order inside
# choices and repeated groups (sorting their members can change XML meaning).
ddiModelOrder <- function(model, children) {
    groups <- function(p) {
        if (p$kind == "element" || p$kind == "choice" || p$max > 1) {
            return(list(ddiModelNames(p)))
        }

        unlist(lapply(p$particles, groups), recursive = FALSE)
    }

    slots <- groups(model)

    ranks <- vapply(children, function(name) {
        found <- which(vapply(slots, function(slot) is.element(name, slot), logical(1)))

        if (length(found)) {
            found[1]
        }
        else {
            Inf
        }
    }, numeric(1))

    order(ranks, seq_along(children))
}

ddiModelProblems <- function(model, children, path, complete = TRUE) {
    allowed <- ddiModelNames(model)
    unknown <- setdiff(children, allowed)

    if (length(unknown)) {
        problems <- sprintf(
            "%s contains unexpected child %s.",
            path,
            unknown
        )
    }
    else {
        problems <- character()
    }

    for (name in allowed) {
        bounds <- ddiModelBounds(model, name)
        n <- sum(children == name)

        if (complete && n < bounds[1]) {
            problems <- c(
                problems,
                sprintf(
                    "%s expects the mandatory child %s (at least %s occurrence(s)).",
                    path,
                    name,
                    bounds[1]
                )
            )
        }

        if (n > bounds[2]) {
            problems <- c(
                problems,
                sprintf(
                    "%s permits at most %s occurrence(s) of child %s.",
                    path,
                    bounds[2],
                    name
                )
            )
        }
    }

    if (!length(problems) && !ddiModelMatches(model, children, complete)) {
        problems <- sprintf(
            "%s does not satisfy its child sequence/choice rules (%s).",
            path,
            paste(allowed, collapse = ", ")
        )
    }

    problems
}
