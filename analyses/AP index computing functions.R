
AP.Wagner.spread2 = function(parties, results, like) {
  
  #NOTE: IF A RESPONDENT DOESN'T KNOW A PARTY I JUST REMOVE THAT
  # PARTY FROM THE COMPUTATION but I DO NOT STANDARDIZE. So a person
  #that does not know many parties will be less polarized...
  if(sum(is.na(like)) < length(parties)) #if the respondent knows
    #at least two parties
  {
    #I standartdize the results by the electoral percentages but not
    # considering if the respodnent knows the party (standardization
    #before removing the rows where like is null. While in the previous
    #function the order is reversed)
    
    results = results/sum(results)
    results = results[!is.na(like)]
    like = like[!is.na(like)]
    
    #I compute the average like as the vectorial product of results (expressed between 0 and 1)
    # and like (from 0 to 10)
    like_hat = (results/(sum(results))) %*% like #the average like should
    # be standardized by the parties known by the respondent!
    
    
    like_hat = rep(like_hat, length(like))
    AP = sqrt(results %*% (like-like_hat)^2)
    return(AP)
  }
  else #if the respondent does not know any party
  {
    return(0)
  }
}


##############################################################################

################ DEFINING FUNCTION FOR WAGNER AP INDEX MEAN DISTANCE #######

###############################################################################


AP.Wagner.meandist2 = function(parties, results, like) {
  
  if(sum(is.na(like)) != length(parties))
  {
    #I standartdize the results by the electoral percentages but not
    # considering if the respodnent knows the party (standardization
    #before removing the rows where like is null. While in the previous
    #function the order is reversed)
    
    results = results/sum(results)
    results = results[!is.na(like)]
    like = like[!is.na(like)]
    
    like_max = rep(max(like), sum(!is.na(like)))
    
    AP = sqrt(results %*%(like-like_max)^2)
    return(AP)
  }
  else
    return(0)
}


