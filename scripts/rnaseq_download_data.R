# Create a "data" directory
dir.create("data_rnaseq")

# Download the data provided by your collaborator
# using a for loop to automate this step
for(i in c("counts_raw.csv", "counts_transformed.csv", "sample_info.csv", "test_result.csv")){
  download.file(
    url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=true"),
    destfile = paste0("data_rnaseq/", i)
  )
}

