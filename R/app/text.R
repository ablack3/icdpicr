welcome <- function(){
    tagList(
        h1("Welcome to ICDPIC-R"),
        p(" The intention of the ICD Programs for Injury Categorization (ICDPIC) is to provide 
            inexpensive methods for translating International Classification of Diseases (Ninth Revision) 
            diagnosis codes into standard injury categories and/or scores. Programs contained in this 
            module can provide Abbreviated Injury Scale score (AIS), ISS body region, 
            Injury Severity Score (ISS), an alternate version of the Charlson Score and 
            Elixhauser categories.
          ")
    
    )
}



instructions <- function(){
    tagList(
        h1("Instructions"),
        p("The user's data be in csv format and must contain ICD-9-CM or ICD-10-CM diagnosis codes.  
            The data should be in wide format and contain one row per person and one diagnosis code per
            column. Diagnosis codes in
          the user's data must be of type string.  A decimal point in the diagnosis
          codes is optional.  Diagnosis codes should have a width of 5 (6 if a
          decimal point is present).  The diagnosis code prefix must be the same for
          all diagnosis codes and numbered sequentially starting with 1, for example,
          dx1...dxN.  External Cause of Injury codes (E-Codes) should be included in
          this set and must begin with a capital 'E'.
          ")
    )
    
}
