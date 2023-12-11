
# DEFINING FUNCTION FOR WAGNER AP INDEX SPREAD OF SCORES

AP.Wagner.spread = function(parties, results, like) {
  
  #NOTE: IF A RESPONDENT DOESN'T KNOW A PARTY I JUST REMOVE THAT
  # PARTY FROM THE COMPUTATION AND STANDARDIZE
  if(sum(is.na(like)) != length(parties))
  {
    #I standartdize the results by the parties known by respondent
    results = results[!is.na(like)]
    results = results/sum(results)
    like = like[!is.na(like)]
    
    #I compute the average like as the vectorial product of results (expressed between 0 and 1)
    # and like (from 0 to 10)
    like_hat = results %*% like
    
    
    like_hat = rep(like_hat, length(like))
    AP = sqrt(results %*% (like-like_hat)^2)
    return(AP)
  }
  else
    return(NA)
}




# DEFINING FUNCTION FOR WAGNER AP INDEX MEAN DISTANCE

AP.Wagner.meandist = function(parties, results, like) {
  
  if(sum(is.na(like)) != length(parties))
  {
    #I standartdize the results by the parties known by respondent
    results = results[!is.na(like)]
    # I also standardize as if the percentage of the parties add up to 100 
    #(they dont)
    results = results/sum(results)
    
    
    like = like[!is.na(like)]
    like_max = rep(max(like), sum(!is.na(like)))
    
    AP = sqrt(results %*%(like-like_max)^2)
    return(AP)
  }
  else
    return(NA)
}
