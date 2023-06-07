# List the associate species for each LC (as opposed to the prototypes).

edges <- g1 |> activate(edges) |> select (A, B, edge_latent_community) |> as_tibble()
# Edges with edge_latent_community == NA are inter-block.
out_edges <- edges |> filter(is.na(edge_latent_community)) # - at this point we have
# the associates, but we don't yet know which LC they are associated with ...

# Function to return the LC containing the A or B end of a dyad.
# All species are prototypes for one LC, except in the case that there is an empty block (LC7) -
# (so the associates of LC7 are not prototypes for any community).
EdgeEnd <- function(.x){alc <- g1 |> activate(nodes) |>
                        filter(name == .x) |>
                        select(latent_community) |>
                        as_tibble()
  return(alc$latent_community)
}

# Add the A-end and B-end LC:
out_edges <- out_edges |> mutate(A_lc = map_int(.x = A, EdgeEnd))
out_edges <- out_edges |> mutate(B_lc = map_int(.x = B, EdgeEnd))

# Now we're in a position to find which LC the species (nodes) are associated with:
associates_lc1 <- out_edges |> filter((A_lc == 1)|(B_lc == 1)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)

associates_lc2 <- out_edges |> filter((A_lc == 2)|(B_lc == 2)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)

associates_lc3 <- out_edges |> filter((A_lc == 3)|(B_lc == 3)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)

associates_lc4 <- out_edges |> filter((A_lc == 4)|(B_lc == 4)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)

associates_lc5 <- out_edges |> filter((A_lc == 5)|(B_lc == 5)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)

associates_lc6 <- out_edges |> filter((A_lc == 6)|(B_lc == 6)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)

associates_lc7 <- out_edges |> filter((A_lc == 7)|(B_lc == 7)) |>
  select(A, B) |>
  pivot_longer(c(A,B)) |>
  distinct(value)



