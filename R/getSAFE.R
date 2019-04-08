getSAFE <-
function(record_ids, dir='.'){

api <- "https://sandbox.zenodo.org/api/records/%s"

for(rec in record_ids){
record <- fromJSON(url(sprintf(api, record_id)))
bucket <- fromJSON(record$links$bucket)

fname <- file.path(dir, basename(bucket$contents$links$self))
download.file(bucket$contents$links$self, destfile=fname) 
}
}
