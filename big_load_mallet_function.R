#####
# load_from_mallet_state isn't liking my 500 topics.gz file
# I am going to tweak it just for this case
# so for the 500 topics.gz file, I will use load_from_mallet_state_b500
#also I need to define read_state_bigmem and read_state_nobigmem for 
#load_from_mallet_state_b500 to work
####

read_state_bigmem <- function (simplified_state_file, K) {
  ss <- read_sampling_state(simplified_state_file)
  
  docs <- bigtabulate::bigsplit(ss, c("doc", "topic"),
                                splitcol=NA_real_)
  dtl <- vapply(docs, function (i) sum(ss[i, "count"]), integer(1))
  doc_topics <- matrix(dtl, ncol=K)
  
  words <- bigtabulate::bigsplit(ss, c("topic", "type"),
                                 splitcol=NA_real_, splitret="sparselist")
  twl <- vapply(words, function (i) sum(ss[i, "count"]), integer(1))
  twl_ij <- stringr::str_split_fixed(names(twl), ":", 2)
  topic_words <- Matrix::sparseMatrix(
    i=as.integer(twl_ij[ , 1]),
    j=as.integer(twl_ij[ , 2]),
    x=twl)
  
  list(
    ss=ss,
    doc_topics=doc_topics,
    topic_words=topic_words
  )
}

# read mallet state directly without using bigmemory functions
read_state_nobigmem <- function (simplified_state_file) {
  if (requireNamespace("readr", quietly=TRUE)) {
    gibbs <- readr::read_csv(simplified_state_file, col_types="iiii")
  } else {
    gibbs <- read.csv(simplified_state_file, header=TRUE,
                      row.names=NULL, colClasses=rep("integer", 4))
  }
  
  dtl <- dplyr::group_by_(gibbs, ~ doc, ~ topic)
  dtl <- dplyr::ungroup(dplyr::summarize_(dtl, count=~ sum(count)))
  dtl <- dplyr::mutate_(dtl, doc=~ doc + 1L, topic=~ topic + 1L)
  
  twl <- dplyr::group_by_(gibbs, ~ topic, ~ type)
  twl <- dplyr::ungroup(dplyr::summarize_(twl, count=~ sum(count)))
  twl <- dplyr::mutate_(twl, type=~ type + 1L, topic=~ topic + 1L)
  
  doc_topics <- matrix(0, nrow=max(dtl$doc),
                       ncol=max(dtl$topic))
  doc_topics[cbind(dtl$doc, dtl$topic)] <- dtl$count
  
  topic_words <- Matrix::sparseMatrix(
    i=twl$topic, j=twl$type, x=twl$count
  )
  
  list(
    doc_topics=doc_topics,
    topic_words=topic_words
  )
}

load_from_mallet_state_b500 <- function(mallet_state_file, simplified_state_file = file.path(dirname(mallet_state_file), 
                                                                                             "state.csv"), instances_file = NULL, keep_sampling_state = TRUE, 
                                        metadata_file = NULL, bigmemory = TRUE) 
{
  gzf <- gzcon(file(mallet_state_file, "rb"))
  on.exit(close(gzf))
  pp <- readLines(gzf, n = 3)
  #a <- stringr::str_match(pp[2], "^#alpha : ([0-9. ]+)$")
  b <- stringr::str_match(pp[3], "^#beta : ([0-9. ]+)$")
  #if (is.na(a) || is.na(b)) {
  #  stop("hyperparameter notation missing. Is this really a file produced by\\nmallet train-topics --output-state?")
  #}
  hyper <- list(alpha = as.numeric(str_split(substring(pp[2],10,nchar(pp[2]))," ")[[1]])[1:500],
                beta = as.numeric(b[1, 2]))
  ss_temp <- FALSE
  if (is.null(simplified_state_file)) {
    simplified_state_file <- tempfile()
    ss_temp <- TRUE
  }
  simplify_state(mallet_state_file, simplified_state_file)
  if (!requireNamespace("bigtabulate", quietly = TRUE)) {
    bigmemory <- FALSE
    warning("bigtabulate package not available. Falling back to in-memory loading.\\nSampling state will not be available after loading.")
  }
  if (!bigmemory) {
    keep_sampling_state <- FALSE
    st <- read_state_nobigmem(simplified_state_file)
  }
  else {
    st <- read_state_bigmem(simplified_state_file, K = length(hyper$alpha))
  }
  doc_ids <- NULL
  vocab <- NULL
  il <- NULL
  if (!is.null(instances_file)) {
    il <- read_instances(instances_file)
    doc_ids <- instances_ids(il)
    vocab <- instances_vocabulary(il)
  }
  if (ss_temp) {
    unlink(simplified_state_file)
  }
  if (!keep_sampling_state) {
    st$ss <- NULL
  }
  meta <- NULL
  if (!is.null(metadata_file)) {
    meta = read_dfr_metadata(metadata_file)
  }
  mallet_model(doc_topics = st$doc_topics, topic_words = st$topic_words, 
               hyper = hyper, ss = st$ss, doc_ids = doc_ids, vocab = vocab, 
               instances = il, metadata = meta)
}


