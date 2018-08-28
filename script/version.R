version <- head(system("git tag",intern = TRUE),n = 1)
saveRDS(version,"data/version.rds")