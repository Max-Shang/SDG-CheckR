 # server application to run checks on SDG files


function(input, output){
  
  
  #Variable Summary -----
  output$blankcolumns <- renderTable({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    
    result.cols <- data.frame(ColumnName = NA,
                              NAs = NA,
                              Ns = NA,
                              Obs = NA,
                              Total= NA)
    
    #for each column, count frequency type
    for(i in 1:ncol(x)){
      
      ColumnName <- colnames(x[i])
      NAs <- sum(is.na(x[i]))
      Ns <- sum(x[i] == "N", na.rm = T)
      Obs <- sum((!is.na(x[i]) & x[i] != "N"))
      Total = NAs + Ns + Obs
      
      temp <- data.frame(ColumnName,
                         NAs,
                         Ns,
                         Obs,
                         Total)
      
      result.cols <- rbind(result.cols,
                           temp)
    }
    
    result.cols[-1,]
    
  })
  
  #Correct column name check ------
  output$columncheck <- renderTable({

    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
  
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    #First test column names
    col.test <- sum(colnames(x) %in% colnames(template)) == length(template)
    
    if(col.test==TRUE){
  
        print("All columns are correct.")
      
    } else {
        incorrect.columns <- x[,!(colnames(x) %in% colnames(template))]
        data.frame(ColumnNameswithErrors = colnames(incorrect.columns))
    }
  })
  
  #3 Required area (i.e. country, region, etc.) check------
  output$reqareas <- renderTable({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)

    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)

    #check if input file has correct area name
    if("REF_AREA" %in% colnames(x)){
      colnames(x)[colnames(x) == "REF_AREA"] <- "GeoAreaCode"
      
    } else if (!("REF_AREA") %in% colnames(x)) {
      
      stop("Cannot find column with geographic code. You must name the column correctly.")
    }
    
    # 3 check if input file has correct area name
    if("Reference_Area_Name" %in% colnames(x)){
      colnames(x)[colnames(x) == "Reference_Area_Name"] <- "GeoAreaName"
      
    } else if (!("GeoAreaName") %in% colnames(x)) {
      stop("Cannot find column with geographic name. You must name the column correctly.")
    }
    
    
    #4 Identify required areas which are not in file(correct area code check)
    req.codes <- geo[geo$required == "R",]
    codes.in.x <- unique(x$GeoAreaCode)
    
    RequiredButMissing<- req.codes[!(req.codes$code %in% codes.in.x), c("Name")]
    
    result <- data.frame(`Missing Areas` = RequiredButMissing)
    
    if(nrow(result) == 0){
      
      data.frame(`Missing Areas` = "All required areas are contained in file.")
      
    } else {
      result
    }
    
  })
  
  

  #4 Check that geo codes are correct ---
  output$geocodes <- renderTable({

    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    #check if input file has correct area name
    if("REF_AREA" %in% colnames(x)){
     # colnames(x)[colnames(x) == "REF_AREA"] <- "REF_AREA"
      
    } else if (!("REF_AREA") %in% colnames(x)) {
      
      stop("Cannot find column with geographic code. You must name the column correctly.")
    }
    
    #check if input file has correct area name
    if("Reference_Area_Name" %in% colnames(x)){
      colnames(x)[colnames(x) == "Reference_Area_Name"] <- "GeoAreaName"
      
    } else if (!("GeoAreaName") %in% colnames(x)) {
      stop("Cannot find column with geographic name. You must name the column correctly.")
    }
    
    
    #create unique lists
    x.codes <- x[, c('GeoAreaName', "REF_AREA")]
    
    x.codes <- unique(x.codes)
    x.codes$REF_AREA <- as.integer(x.codes$REF_AREA)
    
    notFound <- x.codes[!(x.codes$REF_AREA %in% unique(geo$code)),]
    notFound$REF_AREA <- as.character(notFound$REF_AREA)
    
    if(nrow(notFound) == 0){
      
      data.frame(`Codes` = "All required areas are contained in file.")
      
    } else {
      notFound
    }
    
    
  })
  
  #5 Check that units of measure are correct
  output$units <- renderTable({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    x.units <- unique(x$UNIT_MEASURE)
    
    notFound <- x.units[!(x.units %in% unique(units$Code))]
    
    if(length((notFound)) == 0){
      
      data.frame(`Codes` = "All units are correct.")
      
    } else {
      data.frame(codes = notFound)
    }
    
  })
  
  #6 Check nature -----
  output$natureT <- renderTable({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    x.nature <- unique(x$NATURE)
    
    notFound <- x.nature[!(x.nature %in% unique(nature$Code))]
    
    if(length((notFound)) == 0){
      
      data.frame(`Codes` = "All nature are correct.")
      
    } else {
      data.frame(codes = notFound)
    }
    
    
  })
  
  #7 Check nature codes applied correctly to Ns, and NAs, NaN
  output$natureNsNAs <- renderText({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    if(!("OBS_VALUE" %in% colnames(x))) {
      "Cannot find Value column. Check column names!"
    } else {
      
      #First test to see if Values = N, have N in the nature column
      x.N <- x[x$OBS_VALUE %in% c("N", "NaN", "_X"),]
      x.N.test <- (x.N$OBS_VALUE == x.N$NATURE)
      #if x.N$Nature is NA, x.N.test equals false which creates a problem later one,
      #so replace to False if test is NA 
      x.N.test[is.na(x.N.test)] <- FALSE
      
      
      if(length(x.N.test) == 0){
        x.N.test <- 0
      }
      
      #Test to see if NAs in value columns have NAs or blanks in the nature column
      x.NA <- x[x$OBS_VALUE %in% c("NA", "NaN"),]
      
      if(nrow(x.NA) == 0){
        x.NA.test <- 0
      } else {
        
        x.NA$OBS_VALUE <- NA
        x.NA$ValueTest <- is.na(x.NA$OBS_VALUE)
        
        x.NA$NatureTest <- is.na(x.NA$NATURE)
        
        #Negate, so we can use a sum of greater than 1 to indicate an error
        x.NA.test <- !(x.NA$ValueTest == x.NA$NatureTest) 
        
      }
      
      result <- sum(length(x.NA.test[x.NA.test == FALSE]), length(x.N.test[x.N.test == FALSE]))
      
      if(result > 1){
        
        "ERROR: If there is a N, NA, or blank in the value column, there must be N, or NA in the Nature column. Please correct before resubmitting!"
      } else {
        
        "The correct nature codes were applied to N, NAs, and NaN."
      }
    }
    
  })
  
  
  #8 Check source-----  
  
  #Check nature codes applied correctly to Ns, and NAs, NaN 
  output$SourceC <- renderText({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    if(!("SOURCE_DETAIL" %in% colnames(x))) {
      "Cannot find Source column. Check column names!"
    } else {
    
    #non missing values
    `%notin%` <- Negate(`%in%`)
    x.N <- x[x$OBS_VALUE %notin% c("N", "NaN"),]
    #make sure source isn't missing
    x.N.test <- x.N[x.N$SOURCE_DETAIL %in% c("N", "NA"),]
    #if x.N$Nature is NA, x.N.test equals false which creates a problem later one,
    #so replace to False if test is NA 
    x.N.test[is.na(x.N.test)] <- FALSE
    
    
    if(NROW(x.N.test) == 0){
      x.N.test <- 0
      "All non-missing values have a corresponding source."
    } else {
      "Error: Please make sure all non-missing values have a corresponding source. Please correct before resubmitting." 
    }
    }
    
  })
  
  
  
  #9 Check duplicates---- 
  output$duplicatesT <- renderTable({
    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    x <- readxl::read_xlsx(inFile$datapath)
    x <- cleanInputFileCols(x)
    
    colsToCheck <- c("SeriesID","REF_AREA","TIME_PERIOD","SEX","AGE","COMPOSITE_BREAKDOWN")
    
    filtercols <- colnames(x) %in% colsToCheck
    
    y <- x[,filtercols]
    
    result <- y[duplicated(y),]
    result_dupes<- result
    
    if(nrow(result) == 0){
     result <- "No duplicates"
    } else {
    
      result[, ] <- lapply(result[, ], as.character)
    }
  result
    
  })



# 



########################################
#10 Correct Country, Region Check ------
########################################

output$RegionName <- renderTable({
  
  inFile <- input$file1
  if (is.null(inFile)) return(NULL)
  
  x <- readxl::read_xlsx(inFile$datapath)
  
  
  
  #Identify required areas which are not in file
  countries<-geo
  
  #select columns of country and country codes in x
  
  codes.in.x<-x[,c('GeoAreaName', 'REF_AREA')]
  codes.in.x<-unique(x[,c('GeoAreaName', 'REF_AREA')])
  
  #check which are misspelled in x
  misspelled.in.x<- codes.in.x[!(codes.in.x$GeoAreaName %in% geo$GeoAreaName), c("GeoAreaName","REF_AREA") ]
  
  #if some countries are nor properly specified, remove any "M49" or "MDG" to check again
  
  if(nrow(misspelled.in.x) > 0) {
    
    misspelled.in.x$GeoAreaName<-gsub(pattern="(M49)"    , replacement="" ,   misspelled.in.x$GeoAreaName, fixed=TRUE)
    misspelled.in.x$GeoAreaName<-gsub(pattern="(MDG=M49)", replacement="" ,   misspelled.in.x$GeoAreaName, fixed=TRUE)
    misspelled.in.x$GeoAreaName<-gsub(pattern="(m49)"    , replacement="" ,   misspelled.in.x$GeoAreaName, fixed=TRUE)
    misspelled.in.x$GeoAreaName<-gsub(pattern="  "       , replacement=" ",   misspelled.in.x$GeoAreaName, fixed=TRUE)
    
    
    countries$GeoAreaName      <-gsub(pattern="(M49)"    , replacement="", countries$GeoAreaName, fixed=TRUE)
    countries$GeoAreaName      <-gsub(pattern="(MDG=M49)", replacement="", countries$GeoAreaName, fixed=TRUE)
    countries$GeoAreaName      <-gsub(pattern="(m49)"    , replacement="", countries$GeoAreaName, fixed=TRUE)
    
    misspelled.in.x$GeoAreaName<- trimws(misspelled.in.x$GeoAreaName)
    countries$GeoAreaName      <- trimws(countries$GeoAreaName)
    
    #  Check if there are still countries which do not match the geo file   
    misspelled.in.x<- misspelled.in.x[!(misspelled.in.x$GeoAreaName %in% countries$GeoAreaName),  c("GeoAreaName","REF_AREA")]
    
  }
  
  #if there are still some countries that cannot be matched, check whether the discreptancy might be due to differences in capital and lower letters 
  
  if(nrow(misspelled.in.x) > 0) {
    
    #     Lower Case  
    countries$GeoAreaName       <- tolower(countries$GeoAreaName)
    misspelled.in.x$GeoAreaName <- tolower(misspelled.in.x$GeoAreaName)
    
    #    Check which countries remain unmatched to geo file     
    misspelled.in.x<- misspelled.in.x[!(misspelled.in.x$GeoAreaName %in% countries$GeoAreaName),  c("GeoAreaName","REF_AREA")]
    
    #    rename variables so that we can join and keep on the country level data 
    
    names(misspelled.in.x)[names(misspelled.in.x)=="GeoAreaName"] <- "MissingCountry.in.x"
    names(countries)[names(countries)=="code"] <- "REF_AREA"
    
    #   Join and keep only global- note to self, this is because regions come with many differences in names     
    reduce<-left_join(misspelled.in.x, countries)
    reduce<-reduce[reduce$Reference.Area.Type..for.UNSD.use.only. == "1.0-Global",]
    
    reduce<-reduce[rowSums(is.na(reduce)) != ncol(reduce),]
    reduce<-reduce[,c("MissingCountry.in.x", "REF_AREA", "Name")]
    reduce[, ] <- lapply(reduce[, ], as.character)
    reduce
  }
  
  
  
  
  if(nrow(misspelled.in.x)  == 0){
    
    data.frame(`Missing Areas` = "All Areas are spelt correctly in the dataset")
    
  } else {
    reduce[, ] <- lapply(reduce[, ], as.character)
    reduce
  }
})


############################################
#11    CL_sex check ---- 
############################################



output$cl_sex_check <- renderTable({
  
  
  inFile <- input$file1
  if (is.null(inFile)) return(NULL)
  
  x <- readxl::read_xlsx(inFile$datapath)
  
  if ('SEX' %in% colnames(x)){
    #check CL_Sex column
    codes.in.cl_sex<-unique(x[,c('SEX')])
    codes.in.cl_sex<-codes.in.cl_sex[!(codes.in.cl_sex$SEX=="F"  | codes.in.cl_sex$SEX=="M"  | codes.in.cl_sex$SEX=="_T"),]
    codes.in.cl_sex<-codes.in.cl_sex[!is.na(codes.in.cl_sex$SEX),]
    
    
    if(nrow(codes.in.cl_sex) > 0){
      
      #print("The following Cl_sex values do not match the template")
      codes.in.cl_sex
    } else if (nrow(codes.in.cl_sex) == 0) {
      data.frame(`Missing Series ID` = "All SEX values match template")
    }
   
  } else {data.frame(`Message` = "Not applicable, no SEX column in data")}
  
  
  
  
  # #check if series ID or series code is blank 
  # series.id.x<-x[is.na(x$SeriesID),]
  # series.code.x<-x[is.na(x$SeriesCode),]
  # 
  # 
  # if(nrow(series.id.x) > 0){
  #   
  #   data.frame(`Missing Series ID` = "Some rows have a missing Series ID")
  #   
  # } 
  # 
  # if (nrow(series.code.x) >0) {
  #   print("Some rows have a missing Series Code")
  #   
  # }
  
})

############################################
#12    Series ID and codes
############################################



output$series_id_code <- renderTable({
  
  
  inFile <- input$file1
  if (is.null(inFile)) return(NULL)
  
  x <- readxl::read_xlsx(inFile$datapath)


#check if series ID or series code is blank
series.id.x  <-x[is.na(x$SeriesID  ),]
series.code.x<-x[is.na(x$SERIES),]

result<-matrix(NA, 4,1)
if(nrow(series.id.x) > 0){
  
  result[1,]<-" Warning: Some rows have a missing Series ID"
  
} 

if(nrow(series.id.x) == 0){
  
  result[1,]<- "No Missing Series ID"
  
} 

if (nrow(series.code.x) >0) {
  result[2,]<-"Warning: Some rows have a missing Series Code"
  
}

if(nrow(series.id.x) == 0){
  
  result[2,]<-"No Missing Series Code"
  
} 


series.ids<-as.data.frame(unique(x$SeriesID))
series.codes<-unique(x$SERIES)
series.codes<-series.codes[!(is.na(series.codes))]


if(nrow(series.ids) > 1){
  result[3,]<-"Warning: There are multiple Series IDs in this dataset"
} 

if (nrow(series.code.x) >1) {
  result[4,]<-"Warning: There are multiple Series Codes in this dataset"
  
}

result<-result[!is.na(result[,1]), ]
result<-as.data.frame(result)
result
})

  
#############################################
#13               Check Regional Averages
#############################################


output$Regionalavgs <- renderTable({
  
  
  inFile <- input$file1
  if (is.null(inFile)) return(NULL)
  
  x <- readxl::read_xlsx(inFile$datapath)
 
  
  
  colsToCheck <- c("REF_AREA","TIME_PERIOD","GeoAreaName", "OBS_VALUE")
  
  
  y <- x[,colsToCheck]
  
  
  # regions2<- lookup_geoAreas %>%
  #           filter(required == "O")
  
  # regions2<-regions2[,c("code", "Name")]
  
  
  #subset by region
  names(regions)[names(regions) == "M49_Code"] <- "REF_AREA"
  names(regions)[names(regions) == "Country"] <- "GeoAreaName"
  
  #remove dependent territories 
  regions<-regions[regions$include==1,]
  
  #countries by region
  regions_list<- regions %>%
    group_by(region_m49, Region_Name) %>%
    dplyr::summarize(Country_count_total = n_distinct(REF_AREA)) %>%
    ungroup()
  
  
  #Remove if value is missing
  y$OBS_VALUE[y$OBS_VALUE=="NaN"]<-NA
  regions_list2<- y %>%
    dplyr::filter(OBS_VALUE != "NA"  & OBS_VALUE != "N")%>%
    ungroup()

  
  unique.y<-unique(regions_list2[,c('GeoAreaName', 'REF_AREA', 'TIME_PERIOD')])
  names(unique.y)[names(unique.y) == "REF_AREA"] <- "region_m49"
  join.regions.y<- inner_join(unique.y, regions_list)
  # join.regions.y$GeoAreaCode<- join.regions.y$region_m49
  # names(join.regions.y)[names(join.regions.y) == "GeoAreaName"] <- "RegionName"
  # join<- left_join(regions_list2, join.regions.y)
  # 
  join<- left_join(regions_list2, regions)
  
  
  #for every row in join now we're going to check if its one of the regions
  if(NROW(join.regions.y)==0){
    print("No Values for Regional Averages Included")
  } else if (NROW(join.regions.y)>0){
  for(i in 1:NROW(join.regions.y)){
    if (TRUE %in% grepl(join.regions.y$region_m49[i], join$REF_AREA)==TRUE){

      
      
      # #for each region, if this region is included in the m49 region in the dataset
      # for(i in 1:NROW(regions_list)){
      #   #if this region is included in the m49 region in the dataset
      #   if (TRUE %in% grepl(regions_list$region_m49[i],regions_list2$region_m49)==TRUE){
      #then, group by region and year and count number of distinct oberervations
      regions1<- join %>%
        group_by(region_m49,  Region_Name, TIME_PERIOD) %>%
        dplyr::summarise(Country_count = n_distinct(REF_AREA)) %>%
        ungroup()
    }
  }
  
  join_try<- inner_join(regions1, join.regions.y, by=c("region_m49", "TIME_PERIOD"))
  
  d <- transform(join_try, percentage = (Country_count / Country_count_total)*100)
  try<-inner_join(unique.y, d)
  try<-unique(try)
  
  result <- try %>%
    filter(percentage<50)
  
  if(nrow(result) ==0 ) {
    result <- "All regional averages include at least 50% of the countries"
    
  } else{
    
    result<-result[,c("GeoAreaName", "region_m49" , "TIME_PERIOD", "percentage")]
    
    result$region_m49 <- lapply(result$region_m49, as.character)
    
    
  }
result<-as.data.frame(result)
result
}
  
  

   
})
  

}