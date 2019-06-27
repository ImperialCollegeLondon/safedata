getRecords <-
function(){

records <- jsonlite::fromJSON(url("http://127.0.0.1:8000/datasets/view_datasets?_export_type=json"))
records$record_ids <- as.numeric(sapply(records$zenodo_concept_record, basename))
return(records)
}
