library(magrittr)
library(tidyverse)
library(reshape2)
library(Rcpp)
`%nin%` <- Negate(`%in%`)
sourceCpp("connectivity.cpp")

dists <- read.csv("distances.csv", header = TRUE, sep = ',') %>%
  acast(start ~ end, value.var = "miles", fill = -1)
stopifnot(isSymmetric(dists))

generate_routes <- function(start.camp = "MANYGLACIER", end.camp = start.camp,
                            miles.per.day = c(0, 12.2), n.days = 7,
                            exclude.camps = c("FLA", "PACKERSROOST", "LOGANPASS", "SIYEHBEND", "MOJ"))
{
  camps <- rownames(dists)
  start.camp <- match.arg(start.camp, camps, several.ok = FALSE)
  end.camp <- match.arg(end.camp, camps, several.ok = FALSE)
  exclude.camps <- match.arg(exclude.camps, camps, several.ok = TRUE)

  dist.tmp <- replace(dists, dists > miles.per.day[2], -1)

  dists.full <- connectivity(dist.tmp)
  rownames(dists.full) <- colnames(dists.full) <- camps
  stopifnot(isSymmetric(dists.full))

  dists.full.long <- dists.full %>%
    melt(varnames = c("start", "end")) %>%
    filter(value <= miles.per.day[2]) %>%
    filter(start != end) %>%
    filter(start %nin% exclude.camps & end %nin% exclude.camps) %>%
    select(start, value, end)

  out <- dists.full.long %>%
    filter(value >= miles.per.day[1]) %>%
    set_colnames(c("Start", "miles", "Night.1")) %>%
    filter(Start == start.camp) %>%
    mutate(Miles.1 = miles) %>%
    select(miles, Start, Miles.1, Night.1)
  for(i in 2:n.days)
  {
    out <- out %>%
      left_join(dists.full.long, by = set_names("start", paste0("Night.", i-1))) %>%
      filter(value >= miles.per.day[1]) %>%
      mutate(miles = miles + value) %>%
      set_colnames(replace(colnames(.), colnames(.) == "end", paste0("Night.", i))) %>%
      set_colnames(replace(colnames(.), colnames(.) == "value", paste0("Miles.", i)))
  }
  out %>%
    set_colnames(replace(colnames(.), colnames(.) == paste0("Night.", n.days), "End")) %>%
    filter(End == end.camp)
}

out <- generate_routes() %>%
  # go OUT this way
  filter(Night.1 == "GRN") %>%
  filter(Night.2 == "FIF") %>%
  # come BACK this way
  filter(Night.4 != "MANYGLACIER" & Night.5 != "MANYGLACIER") %>%
  filter(Night.6 %in% c("ELF", "ELH")) %T>%
  write.table("route_option1.csv", sep = ',', col.names = TRUE, row.names = FALSE)

out2 <- generate_routes(n.days = 6) %>%
  # go OUT this way
  filter(Night.1 == "GRN") %>%
  filter(Night.2 == "FIF") %>%
  # come BACK this way
  filter(Night.4 != "MANYGLACIER") %>%
  filter(Night.5 %in% c("ELF", "ELH")) %T>%
  write.table("route_option2.csv", sep = ',', col.names = TRUE, row.names = FALSE)

out3 <- generate_routes(exclude.camps = c("FLA", "PACKERSROOST", "LOGANPASS", "SIYEHBEND", "MOJ", "GOA")) %>%
  # go OUT this way
  filter(Night.1 == "GRN") %>%
  filter(Night.2 == "FIF") %>%
  # come BACK this way
  filter(Night.4 != "MANYGLACIER" & Night.5 != "MANYGLACIER") %>%
  filter(Night.6 %in% c("ELF", "ELH")) %T>%
  write.table("route_option3.csv", sep = ',', col.names = TRUE, row.names = FALSE)

out4 <- generate_routes(exclude.camps = c("FLA", "PACKERSROOST", "LOGANPASS", "SIYEHBEND", "MOJ", "GOA", "GAB")) %>%
  # go OUT this way
  filter(Night.1 == "GRN") %>%
  filter(Night.2 == "FIF") %>%
  # come BACK this way
  filter(Night.4 != "MANYGLACIER" & Night.5 != "MANYGLACIER") %>%
  filter(Night.6 %in% c("ELF", "ELH")) %T>%
  write.table("route_option4.csv", sep = ',', col.names = TRUE, row.names = FALSE)
