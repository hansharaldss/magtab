
#Temporary function for making test data for development

#n sets the base number of of obeservations. The total number of rows in the test data set will be 3*n+2

fakedata<-function(n){
  
  #pr_var1: Hierarchical variable with 4 levels, codes of unequal length and two NAs.
  prep_pr_var11<-paste0(sample(LETTERS[1:3], n, replace = T), sample(LETTERS[1:3], n, replace = T), sample(LETTERS[1:3], n, replace = T), sample(LETTERS[1:3], n, replace = T))
  prep_pr_var12<-paste0(sample(LETTERS[1:3], n, replace = T), sample(LETTERS[1:3], n, replace = T))
  prep_pr_var13<-paste0(sample(LETTERS[1:3], n, replace = T), sample(LETTERS[1:3], n, replace = T), sample(LETTERS[1:3], n, replace = T))
  pr_var1<-c(prep_pr_var11, NA, prep_pr_var12, NA, prep_pr_var13)
  
  #pr_var2:. Hierarchical variable where codes are of equal length. Two NAs.
  pr_var2<-c(paste0(sample(LETTERS[1:4], 3*n, replace = T), sample(LETTERS[1:4], 3*n, replace = T)), NA, NA)
  
  #pr_var3: Numerical without NAs range should be of order of magnitude 2
  pr_var3<-runif(length(pr_var1), 0, 500)
  
  #pr_var4: Numerical with two NAs. range is of order of magnitude -1
  pr_var4<-c(NA, NA,runif(length(pr_var1)-2, -0.1, 0.1))
  
  #pr_var5: is a categorical non-hierarchical variable that should not be altered in prep
  pr_var5<-sample(c("M", "F"), length(pr_var4), replace = T)
  
  pr_rammi<-data.frame(pr_var1, pr_var2, pr_var3, pr_var4, pr_var5)
  
  #Make sensitive variable with some ICC - this is very ugly code but it will do the job!
  #There is a reason why I'm not making the effects nested but it's a long story.
  
  svar_rammi<-pr_rammi
  svar_rammi$pr_var1[is.na(svar_rammi$pr_var1)]<-"XXXX"
  svar_rammi$pr_var1[nchar(svar_rammi$pr_var1)==3]<-paste0(svar_rammi$pr_var1[nchar(svar_rammi$pr_var1)==3], "0")
  svar_rammi$pr_var1[nchar(svar_rammi$pr_var1)==2]<-paste0(svar_rammi$pr_var1[nchar(svar_rammi$pr_var1)==2], "00")
  svar_rammi$pr_var2[is.na(svar_rammi$pr_var2)]<-"XX"
  svar_rammi$pr_var4[is.na(svar_rammi$pr_var4)]<-0
  svar_rammi$pr_var1_1<-factor(substring(svar_rammi$pr_var1, 1, 1))
  svar_rammi$pr_var1_2<-factor(substring(svar_rammi$pr_var1, 2, 2))
  svar_rammi$pr_var1_3<-factor(substring(svar_rammi$pr_var1, 3, 3))
  svar_rammi$pr_var1_4<-substring(svar_rammi$pr_var1, 4, 4)
  svar_rammi$pr_var2_1<-substring(svar_rammi$pr_var2, 1, 1)
  svar_rammi$pr_var2_2<-substring(svar_rammi$pr_var2, 2, 2)
  svar_rammi$pr_var1<-NULL
  svar_rammi$pr_var2<-NULL
  
  mm_rammi<-dbarts::makeModelMatrixFromDataFrame(svar_rammi)
  coefs<-rnorm(ncol(mm_rammi))
  coefs[3]<-coefs[3]/400
  y<-mm_rammi%*%coefs
  
  #adding sensitive variable to pr_rammi
  pr_rammi$s_var<-as.vector(y)+rnorm(nrow(pr_rammi), 0, sd(y))
  
  pr_rammi<-pr_rammi[sample(1:nrow(pr_rammi)),]
  
  return(pr_rammi)
}


# pr_rammi_small<-fakedata(10)
# pr_rammi_big<-fakedata(10000)



#'The sign function - returns 1 of the same sign as the input number unless it is 0, then the function returns 0
#'Auxiliary function for std_form()
#'@param k the number

sgn<-function(k){
  if(k>0){
    return(1)
  }else if(k<0){
    return(-1)
  }else{
    return(0)
  }
}

#sgn() examples
# sgn(-100)
# sgn(12)
# sgn(0)

#'Takes in number and returns the digits, sign and order of magnitude (powers of 10, scientific notation)
#'Auxiliary function for bins_frame()
#'Used to find intrval order of magnitude
#'
#'@param c The number 
#'
#'@return A list with elements "c" containing the digits, "s" conitaining the sign of the digits and "pwr" containing the exponent.

std_form<-function(c){
  s<-sgn(c)
  c<-abs(c)
  cnt<-0
  if(c>=10){
    while(c>=10){
      c<-c/10
      cnt<-cnt+1
    }
    
  }else if(c<1){
    while(c<1){
      c<-c*10
      cnt<-cnt+1
    }
    cnt<-cnt*-1
  }else{
    cnt<-0
  }
  return(list("c"=c, "s"=s, "pwr"=cnt))
}

#std_form() examples
# std_form(-10)
# std_form(0.1)
# std_form(1)
# std_form(-50)
# std_form(pi)

#'Takes in a string vector that should represent a hierarchically coded variable. Returns a data frame with a variable for each level of the hierarchy.
#'Auxiliary function for prep_data()
#'
#'@param vec The vector
#'@param vec_name="NN" Name of the hierarchical variable
#'@param addChar=F Whether a character should be added to the END of each string to make them of equal length, if the are not
#'@param charSup="0" Character to add is addChar=T 
#'@param startCount=1 Place in string where from the count starts
#'
#'@retrun Returns a data frame with k variables where the first variable contains only the first element of the string (or the character in the startCount place),
#' the second the first two etc.


hier_frame<-function(vec,
                     vec_name="NN",
                     addChar=F,
                     charSup="0",
                     startCount=1,
                     endCount=NULL){
  
  #Check if vec is character, fix if not
  if(!is.character(vec)){
    vec<-as.character(vec)
  }
  #Check if charSup is character, fix if not
  if(!is.character(charSup)){
    charSup<-as.character(charSup)
  }
  if(length(charSup)>1){
    stop(paste0("Only one arguent can be supplied to charSup for variable ", vec_name))
  }
  
  #Check if all strings are of equal length, fix if addChar=T
  charcnt<-unique(nchar(vec, keepNA = F))
  
  if(length(charcnt)>1){
    if(addChar){
      maxi<-max(charcnt)
      mini<-min(charcnt)
      it<-maxi-mini-1
      for(i in 0:it){
        vec[!is.na(vec)&nchar(vec)==(mini+i)]<-paste0(vec[!is.na(vec)&nchar(vec)==(mini+i)], charSup)
      }
      charcnt<-maxi
    }else{
      stop(paste0("Hierarchical variable ",vec_name," contains strings of unequal length."))
    }
  }else{
    maxi<-unique(charcnt)
  }
  
  #Initialize a data frame where the only variable is the first character of the strings in vec
  name_temp<-paste0(vec_name, "_", startCount)
  eval(parse(text = paste0(
    "ret_frame<-data.frame(",name_temp,"=substring(vec, first = startCount, last = startCount))"
  )))
  if(startCount>1){
    eval(parse(text = paste0(
      "ret_frame$", name_temp, "<-paste0(substring(vec, first=1, last=startCount-1),ret_frame$",name_temp,")"
    ))) 
  }
  
  #Add variables to the data frame with variables containing longer substrings from vec
  if(startCount<maxi){
    loopcnt<-1
    for(i in (startCount+1):maxi){
      name_temp<-paste0(vec_name, "_", startCount+loopcnt)
      eval(parse(text = paste0(
        "ret_frame$", name_temp, "<-substring(vec, first=startCount, last=i)"
      )))
      if(startCount>1){
        eval(parse(text = paste0(
          "ret_frame$", name_temp, "<-paste0(substring(vec, first=1, last=startCount-1),ret_frame$",name_temp,")"
        ))) 
      }
      loopcnt<-loopcnt+1
    }
  }
  
  return(ret_frame)
}

#hier_frame() examples
#hier_frame(pr_rammi_small$pr_var1) #returns error
# hier_frame(pr_rammi_small$pr_var1, addChar = T)
# hier_frame(pr_rammi_small$pr_var1, addChar = T, charSup = "X")
# hier_frame(pr_rammi_small$pr_var2)
# hier_frame(pr_rammi_small$pr_var2, startCount = 2)

#'Takes in a numeric vector and returns a data frame with different levels 
#'Auxiliary function for prep_data()
#'
#'@param vec The numeric vector
#'@param vec_name="NN" Name of numeric vector
#'@param startpwr=-1 Order of magnitude from where to start binning relative to the range order of magnitude, -1 indicates one lower
#'@param intervals=c(1,2,3,5,10,15) Digit of the interval width 
#'
#'@retrun A data frame with number of columns equal to the length of the intervals vector where the variable is cut into different length intervals

bins_frame<-function(vec,
                     vec_name ="NN",
                     startpwr=-1,
                     intervals=c(1,2,3,5,10,15,20,30,50,100)){
  
  #Check if vec is numeric
  if(!is.numeric(vec)){
    stop(paste0("Variable ",vec_name, " must be numeric"))
  }
  
  #Find span of x
  ra<-range(vec, na.rm = T)
  span<-ra[2]-ra[1]
  
  #Find order etc.
  span_std_form<-std_form(span)
  ra_min_std_form<-std_form(ra[1])
  
  
  #find parameter for seq() to for breaks in cut()
  cut_from<-(floor((ra_min_std_form$s*(ra_min_std_form$c))/5)*5)*10**(ra_min_std_form$pwr)-abs((floor((ra_min_std_form$s*(ra_min_std_form$c))/5)*5)*10**(ra_min_std_form$pwr))
  cut_to<-ra[1]+2*span
  cut_intervals<-intervals*10**(span_std_form$pwr+startpwr)
  cut_intervals<-cut_intervals[cut_intervals<span]
  
  #initialize data frame where the first variable is the largest cut
  name_temp<-paste0(vec_name, "_", cut_intervals[length(cut_intervals)])
  
  eval(parse(text = paste0(
    "ret_frame<-data.frame(",name_temp,"=cut(vec, breaks=seq(from=",cut_from,",to=",cut_to,", by=",cut_intervals[length(cut_intervals)],")))"
  )))
  
  #add variables with different length cuts
  
  for(k in (length(cut_intervals)-1):1){
    name_temp<-paste0(vec_name, "_", cut_intervals[k])
    eval(parse(text = paste0(
      "ret_frame$",name_temp,"=cut(vec, breaks=seq(from=",cut_from,",to=",cut_to,", by=",cut_intervals[k],"))"
    )))
  }
  
  
  return(ret_frame)
  
}

#bins_frame() examples

# bins_frame(pr_rammi_small$pr_var3, intervals=c(1,2,3,5,10,15,20,30,50,100))
# bins_frame(pr_rammi_small$pr_var4)


#'Prepares a magtab object
#'
#'@param data data to be prepared for micoaggregation
#'@param sensitive_var The sensitive variable
#'@param hiervars_auto=NULL hierarchically coded categorical variables, each to be automatically turned into a set of variables
#'@param hiervars_man=NULL list of variable names that have been manually turned into a set of variables
#'@param numvars_auto=NULL numerical variables, each to be automatically turned into a set of variables with different length bins
#'@param numvars_man=NULL a list of numerical variables that have been manually turned into a set of variables
#'@param addChar=F passed to hier_frame(). Same parameter will be passed for all variables in hiervars_auto.
#'@param charSup="0" passed to hier_frame(). Same parameter will be passed for all variables in hiervars_auto.
#'@param startCount=1 passed to hier_frame(). Same parameter will be passed for all variables in hiervars_auto.
#'@param startpwr=1 passed to bins_frame(). Same parameter will be passed for all variables in numvars_auto.
#'@param intervals=c(1,2,3,5,10,15,20) passed to nums_bin(). Same parameter will be passed for all variables in numvars_auto.
#'
#'@return object of class magtab.


magtab<-function(data,
                 sensitive_var,
                 hiervars_auto=NULL,
                 hiervars_man=NULL,
                 numvars_auto=NULL,
                 numvars_man=NULL,
                 addChar=F,
                 charSup="0",
                 startCount=1,
                 startpwr=-1,
                 intervals=c(1,2,3,5,10,15,20,30,50,100)){
  
  #Check if sensitive_var is in data set
  if(!(sensitive_var%in%names(data))){
    stop(paste0("There is no variable in the supplied data named ", sensitive_var))
  }
  #Check if sensitive var is in hiervar_man, hiervar_auto, numvar_man or numvar_auto
  check_sens<-sensitive_var%in%hiervars_auto|sensitive_var%in%numvars_auto|sensitive_var%in%names(hiervars_man)|sensitive_var%in%names(numvars_man)
  if(check_sens){
    stop(paste0(sensitive_var, " cannot be a predictor"))
  }
  
  #Initialize lists
  data_list<-list()
  names_list<-list()
  #initialize vector of categorical variable names
  cat_vars<-vector()
  
  #initialize vectors for numerical and categorical variables
  nums<-vector()
  hiers<-vector()
  
  
  #Check if hiervars man is supplied and a list
  #If not than check if hiervars_auto is supplied
  #If hiervars_auto is supplied a list of data frames made with hier_frame is created
  if(is.null(hiervars_man)){
    if(!is.null(hiervars_auto)){
      for(n in hiervars_auto){
        eval(parse(text = paste0(
          "temp<-hier_frame(data$", n, ",vec_name=\"", n, "\", addChar=addChar, charSup=charSup, startCount=startCount)"
        )))
        eval(parse(text = paste0(
          "data_list$", n, "<-temp"
        )))
        eval(parse(text = paste0(
          "names_list$", n, "<-names(temp)"
        )))
        cat_vars<-c(cat_vars, names(temp)[1])
      }
      hiers<-hiervars_auto
    }
  }else{
    if(!is.list(hiervars_man)){
      stop("hiervars_man must be a list")
    }else{
      for(i in length(hiervars_man)){
        names_list<-c(names_list, hiervars_man[i]) #This needs further testing and development!
      }
      hiers<-names(hiervars_man)
    }
  }
  
  #Check if numvars_man is supplied and a list
  #If not check if numvars_auto is supplied
  #If numvars auto is supplied a list of data frames is created with bin_frame
  
  if(is.null(numvars_man)){
    if(!is.null(numvars_auto)){
      for(n in numvars_auto){
        eval(parse(text=paste0(
          "temp<-bins_frame(data$", n, ",vec_name=\"", n, "\",startpwr=startpwr, intervals=intervals)"
        )))
        eval(parse(text = paste0(
          "data_list$", n, "<-temp"
        )))
        eval(parse(text = paste0(
          "names_list$", n, "<-names(temp)"
        )))
        cat_vars<-c(cat_vars, names(temp)[1])
      }
      nums<-numvars_auto
    }
  }else{
    if(!is.list(numvars_man)){
      stop("numvars_man must be a list")
    }else{
      for(i in length(numvars_man)){
        names_list<-c(names_list, numvars_man[i]) #This needs further testing and development!
      }
      nums<-names(numvars_man)
    }
  }
  
  #Find variables in the input data that are not included in hiervars_man, hiervars_auto, numvars_man og numvars_auto
  #Those are added as a data frame to data_list
  
  excl_names<-vector()
  if(!is.null(hiervars_man)){excl_names<-c(excl_names, hiervars_man)}
  if(!is.null(hiervars_auto)){excl_names<-c(excl_names, hiervars_auto)}
  if(!is.null(numvars_man)){excl_names<-c(excl_names, numvars_man)}
  if(!is.null(numvars_auto)){excl_names<-c(excl_names, numvars_auto)}
  
  dat_rest<-data.frame(data[,!(names(data)%in%excl_names)])
  
  cat_vars<-c(cat_vars, names(dat_rest)[names(dat_rest)!=sensitive_var])
  
  if(ncol(dat_rest)>0){
    data_list$rest<-dat_rest
    ndr<-names(dat_rest)
    for(n in ndr){
      eval(parse(text = paste0(
        "names_list$", n , "<-\"null\""
      )))
    }
  }
  
  prepped_data<-data_list[[1]]
  
  for(i in 2:length(data_list)){
    prepped_data<-cbind(prepped_data, data_list[[i]])
  }
  
  eval(parse(text = paste0(
    "type<-typeof(data$", sensitive_var, ")"
  )))
  
  if(length(cat_vars)>0){
    eval(parse(text = paste0(
      "cat_freq<-prepped_data |> 
      dplyr::group_by(", paste0(cat_vars, collapse=",") ,") |> 
      dplyr::tally()|>
      dplyr::arrange(n)
      "
    )))
    if(1%in%cat_freq$n){
      warning("Some observations are unique by crudest level of hierarchical/binned variables - check for rare NAs and data errors")
    }
  }else{
    cat_freq<-NULL
  }
  
  ls<-list("org_data"=data, 
           "prepped_data"=prepped_data, 
           "names_hierarchy"=names_list,
           "sensitive_var"=sensitive_var,
           "sensitive_type"=type,
           "category_freq"=cat_freq,
           "binned_numericals"=nums,
           "split_hierarchicals"=hiers)
  class(ls)<-"magtab"
  
  return(ls)
  
}

#magtab() exapmle

#magtab_test<-magtab(data = pr_rammi_small, sensitive_var = "s_var", hiervars_auto = c("pr_var1", "pr_var2"), numvars_auto = c("pr_var3", "pr_var4"), addChar = T)



#'makes table of numbers of levels for each element in a names hierarchy
#'@param hier a names hierarchy from a magtab object

get_nlevels<-function(hier){
  tab<-data.frame("var_names"=names(hier), "size"=unlist(lapply(hier, FUN=length)))
  rownames(tab)<-NULL
  return(tab)
}

#get_nlevels() example

#get_nlevels(magtab_test$names_hierarchy)


#'takes in a magtab object and makes a list of data frames with all possible combinations of predictor variables
#'@param magtab a magtab object
#'@param verbose whether to print the number of possible tables
#'@param m maximum size of combinations. The function returns all combinations of 1, 2, 3...m variables. 


variable_subsets<-function(magtab, verbose=T, m=NULL){
  
  #List original variable names
  varnames<-names(magtab$org_data)
  #Remove sensitive variable from the list
  varnames<-varnames[varnames!=magtab$sensitive_var]
  
  #if m is NULL the value of m is set at the number of variables in the original data set
  if(is.null(m)){
    m<-length(varnames)
  }
  
  
  if(verbose){message("Preparing list of variable combinations")}
  
  
  #List possibe subsets of original variable names
  namelist_org<-list()
  for(i in 1:m){
    namelist_org<-c(namelist_org, combn(varnames, i, simplify = F))
  }
  
  if(verbose){message(paste0("There are ", length(namelist_org), " possible combinations"))}
  
  #make vector of original variable names with multiple corresponding variable names
  mults<-c(magtab$binned_numericals, magtab$split_hierarchicals)
  
  #Get names hierarchi
  nhier<-magtab$names_hierarchy
  
  #Find number of rows needed to be able to fit all variable levels in a matrix
  nhier_tab<-get_nlevels(nhier)
  nhier_tab<-nhier_tab[nhier_tab$var_names!=magtab$sensitive_var,] |> 
    dplyr::arrange(dplyr::desc(size))
  nrow_num<-ceiling(matrixStats::product(nhier_tab$size)) 
  
  #Initialize list to receive tables of variable combos
  namelist_prepped<-list()
  
  #Make list
  #If a hierarchical or binned variable appears in a combination all variants are included
  for(i in 1:length(namelist_org)){
    varnames2<-namelist_org[[i]]
    vn_ord<-nhier_tab$var_names
    vn_ord<-vn_ord[vn_ord%in%varnames2]
    varnames2<-vn_ord
    
    expg_list<-list()
    for(n in varnames2){
      if(n%in%mults){
        eval(parse(text = paste0(
          "expg_list$", n, "<-nhier[[\"",n,"\"]]"
        )))
      }
    }
    for(n in varnames2){
      if(!(n%in%mults)){
        eval(parse(text = paste0(
          "expg_list$", n, "<-\"", n, "\""
        )))
      }
    }
    tempdf<-expand.grid(expg_list, stringsAsFactors = F)
    tempdf<-unique(tempdf)
    namelist_prepped[[i]]<-tempdf
  }
  
  #count number of combinations
  cnt<-0
  
  for(i in 1:length(namelist_prepped)){
    cnt<-cnt+nrow(namelist_prepped[[i]])
  }
  
  if(verbose){message(paste0("List prepared. There are ",cnt, " possible tables" ))}
  
  ret<-list("namelist_prepped"=namelist_prepped, "namelist_org"=namelist_org)
  
  
  return(ret)
}

# variable_subsets example

#subsets<-variable_subsets(magtab_test)

#'Function that checks a table against privacy criteria. Auxiliary function for maketabs.


listfilter<-function(tab,
                     k,
                     l,
                     relative,
                     keep_forbidden,
                     min_n,
                     min_rmse_ratio,
                     min_rmse){
  
  
  if(min_n>=k){
    if(relative){
      if(min_rmse_ratio>=l){
        ret<-tab |> tibble::as_tibble()
        ret_perm<-TRUE
      }else{
        if(keep_forbidden){
          ret<-tab |>  tibble::as_tibble()
        }else{
          ret<-"not_permitted"
        }
        ret_perm<-FALSE
      }
    }else{ #if relative=F
      if(min_rmse>=l){
        ret<-tab |>  tibble::as_tibble()
        ret_perm<-TRUE
      }else{
        if(keep_forbidden){
          ret<-tab |>  tibble::as_tibble()
        }else{
          ret<-"not_permitted"
        }
        ret_perm<-FALSE
      }
    }
  }else{ # if k-kriteron fails
    if(keep_forbidden){
      ret<-tab
    }else{
      ret<-"not_permitted"
    }
    ret_perm<-FALSE
  }
  
  ret_list<-list("ret"=ret, "ret_perm"=ret_perm)
  return(ret_list)
}

#'Funtion that finds the best table for each combination of original variables and each focus variable. Auxiliary function for maketabs.
#'@param mt a temporary containing the original magtab object passed from maketabs

findbest<-function(mt){
  
  #Getting all permitted tables from the tab_report data frame
  mt_rep<-mt$tab_report |> dplyr::filter(permitted)
  #Gets names hierarchy from the magtab object
  hier<-mt$magtab$names_hierarchy
  #makes a vector with names of all binned numerical and hierarchical variables
  nhier<-c(mt$magtab$binned_numericals, mt$magtab$split_hierarchicals)
  #First column of tab_report split into a list
  varv_split<-strsplit(mt_rep$varv, split = " ")
  
  #Makes columns for a "percission score" for each variable
  #Every score starts at zero
  for(n in nhier){
    eval(parse(text = paste0(
      "mt_rep$", n, "<-rep(0, nrow(mt_rep))"
    )))
  }
  
  for(n1 in nhier){ #iterates over binned numerical and hierarchical variables
    #creates a temporary vector with names of all variants of the present binned numerical or hierarchical variable
    eval(parse(text = paste0(
      "hier_temp<-hier$", n1
    )))
    cntr<-1
    for(n2 in hier_temp){ #iterates over temporary vector giving a higher score to finer variants
      p<-which(sapply(varv_split, function(y) n2 %in% y))
      eval(parse(text = paste0(
        "mt_rep$", n1, "[p]<-cntr"
      )))
      cntr<-cntr+1
    }
  }
  
  #Finds names of newly created columns 
  namepicks<-names(mt_rep[(ncol(mt_rep)-length(nhier)+1):ncol(mt_rep)])
  
  #Finds the maximum of each column
  for(n in namepicks){
    eval(parse(text = paste0(
      n, "_max<-max(mt_rep$",n,")"
    )))
  }
  
  #Turns each score into percent of maximum
  for(n in namepicks){
    eval(parse(text = paste0(
      "mt_rep$", n, "<-mt_rep$", n, "/",n,"_max"
    )))
  }
  
  #Creates a rowsum of precission scores and arranges mt_rep by it. Makes avreage precission on other variables a tie-breaker when 
  #more than one table have an equally fine grained variant of a given variable as which,max() returns the number of first maximum
  mt_rep<-mt_rep |> 
    dplyr::mutate(rsum=rowSums(mt_rep[,(ncol(mt_rep)-length(nhier)+1):ncol(mt_rep)])) |> 
    dplyr::arrange(dplyr::desc(rsum))
  
  
  #Vestor of unique combinations of original variables
  combos<-unique(mt_rep$orginal_variables)
  cntr_out<-1
  bests<-vector()
  bestnames<-vector()
  focusvar<-vector()     
  
  for(n1 in combos){ #iterates over all possible combinations
    #Makes temporary data frame with a particular combination of original variables
    temp<-mt_rep[mt_rep$orginal_variables==n1,] 
    #splits and unlists the combination, returning a character vector
    ntemp<-strsplit(n1, split = " ") |> unlist()
    for(n2 in ntemp){ #iterates over all the variables with multiple variants and finds the table with the most precise variant
      if(n2%in%nhier){
        eval(parse(text = paste0(
          "sel<-which.max(temp$", n2, ")"
        )))
        bests[cntr_out]<-temp$nr[sel]
      }else{
        bests[cntr_out]<-NA
      }
      bestnames[cntr_out]<-n1
      focusvar[cntr_out]<-n2
      cntr_out<-cntr_out+1
    }
  }
  
  #just tidying. Arranging so that simpler tables appear earlier in th output, as the would in tab_report 
  bestrep<-data.frame(bestnames, focusvar,"best_nr"=bests) |> 
    dplyr::filter(!is.na(best_nr)) |> 
    dplyr::arrange(best_nr)
  
  return(bestrep)
  
}

#'Function that makes all possible tables form prepped data in maketab
#'@param magtab a magtab object
#'@param m=NULL maximum depth of table. maketabs will make and try all 1-way, 2-way... m-way tables.
#'@param k=3 minumum number of observations per cell
#'@param l=0.25 variability criterion. Minimum ratio of new RMSE to old RMSE per cell if relative=T. Minimum RMSE if relative=F
#'@param publics Names of variables if tables are already in the public domain. If relative=T RMSE will be compared to that table. If publics=NULL old RMSE will be computed using the overall mean. If relative=F, this argument has no effect.   
#'@param keep_forbidden=F if keep_forbidden=F tables that fail criteria are not included in output. 
#'
#'@return A list with 3 items. tab_list: A list of all tables, tab_report: A data frame with details of all tables produced. best_tables: A data frame with details of tables deemed best by findbest() 

maketabs<-function(
    magtab,
    m=NULL,
    k=3,
    l=0.25,
    publics_org=NULL,
    publics_pepped=NULL,
    relative=T,
    keep_forbidden=F){
  
  variable_subs0<-variable_subsets(magtab, m=m, verbose = F)
  variable_subs<-variable_subs0$namelist_prepped
  namelist_org<-variable_subs0$namelist_org
  eval(parse(text = paste0(
    "magtab$prepped_data$sensitive_variable<-magtab$prepped_data$", magtab$sensitive_var
  )))
  
  prepped_data<-dtplyr::lazy_dt(magtab$prepped_data)
  tab_list<-list()
  cnt<-1
  cnt_v<-vector()
  varv<-vector()
  nvar<-vector()
  orgs<-vector()
  min_n<-vector()
  min_rmse<-vector()
  min_rmse_ratio<-vector()
  sum_n<-vector()
  sum_droppedNA<-vector()
  permitted<-vector()
  total_rows<-nrow(magtab$prepped_data)
  publics_used<-vector()
  
  t1<-Sys.time()
  for(i1 in 1:length(variable_subs)){
    nrow_num<-nrow(variable_subs[[i1]])
    for(i2 in 1:nrow_num){
      vars<-variable_subs[[i1]][i2,]
      
      if(!is.null(publics_org)){
        publics_use<-vector()
        for(n in publics_org){
          publics_use<-c(publics_use, publics_pepped[grepl(paste0("^",n), vars)])
        }
        
        # print(paste0("vars : ", paste0(vars, collapse = " ")))
        # print(length(vars))
        # print(paste0("publics use", paste0(publics_use, collapse = " ")))
        if(length(publics_use)>0){
          
          tab_ref<-prepped_data |> 
            dplyr::select(c(as.character(publics_use),"sensitive_variable")) |> 
            tidyr::drop_na() |> 
            dplyr::group_by(!!!rlang::syms(publics_use)) |> 
            dplyr::summarise(ref=mean(sensitive_variable))
          
          tab<-prepped_data |> 
            dplyr::select(c(as.character(vars), as.character(publics_use), "sensitive_variable")) |> 
            tidyr::drop_na() |> 
            dplyr::left_join(
              tab_ref
            ) |> 
            dplyr::mutate(se_old=(sensitive_variable-ref)**2)
        }else{
          tab<-prepped_data |> 
            dplyr::select(c(as.character(vars), "sensitive_variable")) |> 
            tidyr::drop_na() |> 
            dplyr::mutate(ref=mean(sensitive_variable)) |> 
            dplyr::mutate(se_old=(sensitive_variable-ref)**2)
          
        }
      }else{
        tab<-prepped_data |> 
          dplyr::select(c(as.character(vars), "sensitive_variable")) |> 
          tidyr::drop_na() |> 
          dplyr::mutate(ref=mean(sensitive_variable)) |> 
          dplyr::mutate(se_old=(sensitive_variable-ref)**2)
      }
      
      
      tab<-tab |> 
        dplyr::full_join(
          prepped_data |> 
            dplyr::select(c(as.character(vars), "sensitive_variable")) |> 
            tidyr::drop_na() |> 
            dplyr::group_by(!!!rlang::syms(as.character(vars))) |> 
            dplyr::summarise(group_mn=mean(sensitive_variable))
        ) |> 
        dplyr::mutate(se_new=(sensitive_variable-group_mn)**2) |> 
        dplyr::group_by(!!!rlang::syms(as.character(vars))) |> 
        dplyr::summarise(mn=mean(sensitive_variable),
                         #mn_new=mean(group_mn),
                         ref=mean(ref),
                         mse_old=mean(se_old),
                         mse_new=mean(se_new),
                         cnt=dplyr::n()) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(
          rmse_new=sqrt(mse_new),
          rmse_old=sqrt(mse_old)
        ) |> 
        dplyr::mutate(
          rmse_ratio=rmse_new/rmse_old
        )
      
      tab_summary<-tab |> 
        dplyr::summarise(
          min_n=min(cnt),
          min_rmse=min(rmse_new),
          min_rmse_ratio=min(rmse_ratio),
          sum_n=sum(cnt)
        ) |> 
        tibble::as_tibble()
      
      cnt_v[cnt]<-cnt
      varv[cnt]<-paste0(as.character(vars), collapse = " ")
      orgs[cnt]<-paste0(namelist_org[[i1]], collapse = " ")
      min_n[cnt]<-tab_summary$min_n
      min_rmse[cnt]<-tab_summary$min_rmse
      min_rmse_ratio[cnt]<-tab_summary$min_rmse_ratio
      sum_n[cnt]<-tab_summary$sum_n
      sum_droppedNA[cnt]<-total_rows-tab_summary$sum_n
      nvar[cnt]<-length(vars)
      
      if(!is.null(publics_org)){
        publics_used[cnt]<-paste0(publics_use, collapse = " ") 
      }else{
        publics_used[cnt]<-"none"
      }
      
      
      tab_temp<-listfilter(tab = tab, k=k, l=l, relative = relative, keep_forbidden = keep_forbidden, min_n = tab_summary$min_n,
                           min_rmse = tab_summary$min_rmse, min_rmse_ratio = tab_summary$min_rmse_ratio)
      
      tab_list[[cnt]]<-tab_temp$ret
      permitted[cnt]<-tab_temp$ret_perm
      
      cnt<-cnt+1
    }
  }
  
  print(Sys.time()-t1)
  
  tab_report<-data.frame("nr"=cnt_v,
                         varv,
                         "orginal_variables"=orgs,
                         nvar,
                         min_n,
                         min_rmse,
                         min_rmse_ratio,
                         sum_n,
                         sum_droppedNA,
                         permitted)
  
  if(!is.null(publics_org)){
    tab_report$publics_used<-publics_used
  }
  
  ret_temp<-list("tab_list"=tab_list, "tab_report"=tab_report, "magtab"=magtab)
  bests<-findbest(ret_temp)
  
  ret<-list("tab_list"=tab_list, "tab_report"=tab_report, "best_tables"=bests)
  
  return(ret)
}

#maketabs example

#magtab_test2<-magtab(data = pr_rammi_big, sensitive_var = "s_var", hiervars_auto = c("pr_var1", "pr_var2"), numvars_auto = c("pr_var3", "pr_var4"), addChar = T)

#mt1_nopubs<-maketabs(magtab_test2)
#mt1_pubs<-maketabs(magtab_test2, publics = c("pr_var1_1", "pr_var5")) 

#'Fuction to get best tables from a list returned by maketabs().
#'
#'@param maketabs an object returned by maketabs()

get_best<-function(maketabs){
  
  bests<-maketabs$best_tables$best_nr
  nms<-maketabs$best_tables$bestnames
  fcs<-maketabs$best_tables$focusvar
  tabs<-maketabs$tab_list
  
  ret<-list()
  
  for(i in 1:length(bests)){
    sl<-list(
      "vars"=nms[i],
      "focus_var"=fcs[i],
      "tab"=tabs[[bests[i]]]
    )
    ret[[i]]<-sl
  }
  
  return(ret)
  
}
