install.packages("wru", dependencies = T)
install.packages("Rfacebook", dependencies = T)
install.packages("gender", dependencies = T)
install.packages("tidyr", dependencies = T)
install.packages("plyr", dependencies = T)
install.packages("stringr", dependencies = T)
install.packages("devtools", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("RCurl", dependencies = T)
install.packages("Hmisc", dependencies = T)
install.packages("slam", dependencies = T)
install.packages("coda", dependencies = T)
install.packages("tm", dependencies = T)
install.packages("BCRA", dependencies = T)
install.packages("gridExtra", dependencies = T)
install.packages("data.table", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("doParallel", dependencies = T)
install.packages("matrixStats", dependencies = T)

library(Rfacebook)
library(devtools)
library(stringr)
library(plyr)
library(data.table)
library(wru)
library(tidyr)
library(tm)
library(slam)
library(Hmisc)
library(gender)
library(dplyr)
library(coda)
library(RCurl)
library(BCRA)
library(gridExtra)
library(ggplot2)
library(doParallel)
library(matrixStats)

#This is how I got the data, it requires personal information, I therefore decided to save the files extracted from facebook.

   my_authorization <- fbOAuth(app_id = "enter your ID here" , app_secret = "secret code enter here")
    save(my_authorization , file = "my_authorization")
   load("my_authorization")
  page.post1 <- getPage(page="ihadcancer", token=my_authorization, n=5000, feed=TRUE)
  page_posts.3 <- getPage(page="TheBreastCancerSite", token=my_authorization, n=3000, feed=TRUE)
  

  page.post1 <- read.csv("~/Desktop/Data Science Project/Raw Files/ihadcancer.csv", header=TRUE, sep=",")
  page_posts.3 <- read.csv("~/Desktop/Data Science Project/Raw Files/TheBreastCancerSite.csv", header=TRUE, sep=",")

# I. Merge these two above files, coming from two different Page: the purpose here is simply to have more diverse source of data
  page.post1 = full_join(page.post1,page_posts.3)
# Remove posts that are named IhadCancer and TheBreastCancerSite,we want to keep person names
  page.post1 <- page.post1[page.post1$from_name!="I Had Cancer", ] #race   #ethnicity
  page.post1 <- page.post1[page.post1$from_name!="The Breast Cancer Site", ]

# II. Separate FirstName and LastName
  
  ll = strsplit(page.post1$from_name, ' ')
  page.post1$firstname = sapply(ll, function(x) x[1])
  page.post1$lastname = sapply(ll, function(x) x[length(x)])

#Remove posts belonging to organization 
  names <- list(page.post1$from_name)
  pkd.names.quotes <- names

    # III. DetectNames from Organizations , to keep only human names
    url <- "http://www2.census.gov/topics/genealogy/2000surnames/names.zip"
    tf <- tempfile()
    download.file(url,tf, mode="wb")                     # download archive of surname data
    files    <- unzip(tf, exdir=tempdir())               # unzips and returns a vector of file names
    surnames <- read.csv(files[grepl("\\.csv$",files)])  # 152,000 surnames occurring >100 times
    url <- "http://deron.meranda.us/data/census-derived-all-first.txt"
    firstnames <- read.table(url(url), header=FALSE)
    freq <- 10000
    dict  <- unique(c(tolower(surnames$name[1:freq]), tolower(firstnames$V1[1:freq])))
    corp <- Corpus(VectorSource(page.post1$firstname))
    tdm  <- TermDocumentMatrix(corp, control=list(tolower=TRUE, dictionary=dict))
    m    <- as.matrix(tdm)
    m    <- m[rowSums(m)>0,]
    
    m = as.data.frame(m)
    vect1 <- as.vector(rownames(m))
    df = data.frame(vect1, m)
    colnames(df)[which(names(df) == "vect1")] <- "firstname"

#change it to character first
    df$firstname = as.character(df$firstname)
    df$firstname = capitalize(df$firstname) 
    df = df[,c("firstname")]
    df = as.data.frame(df)
    colnames(df)[which(names(df) == "df")] <- "firstname"
# Now merge with original file, of recognized first names
  merge.dataset = inner_join(page.post1,df)


# IV. Find gender of facebook user, using the package Gender
    dataset.names = gender(merge.dataset$firstname,years = c(1932,2012))
    colnames(dataset.names)[which(names(dataset.names) == "name")] <- "firstname"
    dataset.names = dataset.names[,c("firstname","gender")]
    merge.dataset <- merge.dataset[!duplicated(merge.dataset), ]
    dataset.name.gender = inner_join(merge.dataset,dataset.names)         
    dataset.name.gender <- dataset.name.gender[!duplicated(dataset.name.gender), ]


# V. Find Race of facebook users using package wru"
    dataset.name.gender$surname = dataset.name.gender$lastname
    race.dataset = race.pred(dataset.name.gender, races = c("white", "black", "latino", "asian", "other"), name.clean = TRUE, surname.only = TRUE)

#Return values that is maximum 
    
    dat <- transform(race.dataset, max = pmax(race.dataset$pred.whi,race.dataset$pred.bla,race.dataset$pred.his,race.dataset$pred.asi,race.dataset$pred.oth))
    dat$race =  names(dat)[15:19][max.col(dat[, 15:19])]
    #Rename values in Race Column 
    dat$race = ifelse(dat$race == "pred.whi","white",ifelse(dat$race=="pred.bla","black", ifelse(dat$race=="pred.asi","asian",ifelse(dat$race=="pred.his","hispanic",ifelse(dat$race=="pred.oth","other","NA")))))
# Subset More clean Data

    Df = dat
    Df = Df[ , -which(names(Df) %in% c("pred.whi","pred.bla","pred.his","pred.asi","pred.oth","max"))]

#Pick one name for each block of repetition 
    Df <- ddply(Df, "from_name", function(z) tail(z,1)) #1531 Distinct People 

# VI. Assign ID to each person for unique names 
    
    for (i in 1:nrow(Df)){
      a<-unique(Df$from_name)
      a<-as.vector(a)
      Df$id[Df$from_name==a[i]]<-i
    }

#Subset final dataset by these columns 
  Final.dataset = Df[,c("id","firstname","lastname","gender","race","from_id","message","likes_count","comments_count","shares_count","type","link","created_time")]

# VII. Remove males, we are interested in absolute risk of breast cancer in women
  Final.dataset = subset(Final.dataset,Final.dataset$gender=="female")

# I omitted these next few steps for searching for those with history of cancer,
    # since the information can be unknown in the calculation of absolute risk 
    cancerSubset <- Final.dataset[grep("treatment|chemo|diagnosed|mammogram|lumpectomie|
                                       #Chemo|radiation|breast cancer|FREE", Final.dataset$message), ]
    cancerSubset$presence_bc = rep(1,nrow(cancerSubset))
    
    #merge datasets again
    merge.1 = left_join(Final.dataset,cancerSubset)
    # Replace NA by 0 
    merge.1 <- merge.1  %>%
      mutate(colname = ifelse(is.na(merge.1 $presence_bc),0,merge.1$presence_bc))
    colnames(merge.1)[which(names(merge.1) == "colname")] <- "Absence-Presence_Cancer"
    drops <- c("presence_bc")
    merge.1 <- merge.1[ , !(names(merge.1) %in% drops)]




# VII. Load Social Security Administration files in the below files included with the project folder 

# change first directory
dir = setwd("~/Desktop/Data Science Project/age files ")

#check if file_listing as only the SSA files

    file_listing = list.files(path = dir)
    for (f in file_listing) 
    {
      year = str_extract(f, "[0-9]{4}")
      if (year == "1880") 
      { 
        name_data = read.csv(f, header=FALSE)
        names(name_data) = c("Name", "Sex", "Pop")
        name_data$Year = rep(year, dim(name_data)[1])
      }
      
      else 
      {
        name_data_new = read.csv(f, header=FALSE)
        names(name_data_new) = c("Name", "Sex", "Pop")
        name_data_new$Year = rep(year, dim(name_data_new)[1])
        name_data = rbind(name_data, name_data_new)
        #dcast(data = name_data, df ~ Rptname, value.var = "freq")
      }
    }

# Clean this Dataset, it has more than 1,000,000 rows, subset only wanted age ranges and gender
    
    name_data = subset(name_data,name_data$Sex == "F")
    colnames(name_data)[which(names(name_data) == "Name")] <- "firstname"
    named_data = left_join(Final.dataset,name_data)
    current_year = as.numeric(substr(Sys.time(),1,4))
    named_data$age = current_year - as.numeric(named_data$Year)
    #Subset only age between 35 and 90 
    named_data <- subset(named_data,named_data$age <=90)
    named_data <- subset(named_data,named_data$age >= 35)

    #Cummulative Sum for Population
    names_pool = filter(named_data, Year >= 1926, Sex == "F")  %>%
      group_by(firstname) %>%
      #m#utate(value = sum(named)) %>%
      mutate(csum = cumsum(Pop))
    
    # Remove age Column cause age will vary
    drops <- c("age")
    names_pool <- names_pool[ , !(names(names_pool) %in% drops)]

#VIII. Create a function that returns median,1st quartile and 3rd quartile of age

    estimate_age = function (input_name,Sex = F)
      
    {
      
      if (is.na(Sex)) {
        
        names_pool = subset(names_pool, firstname == input_name & Year >= 1926)
      }
      
      else 
        
      {
        names_pool = subset(names_pool, firstname == input_name & Year >= 1926 & Sex == "F")
        
      }  
      
      current_year = as.numeric(substr(Sys.time(),1,4))
      age = current_year - as.numeric(names_pool$Year)
      mean = mean(rep( names_pool$age,names_pool$Pop))
      median = median(rep( names_pool$age,names_pool$Pop))
      twentyfifth.quantile = quantile(rep( names_pool$age,  names_pool$Pop),probs = c(0.25))
      seventyfifth.quantile = quantile(rep( names_pool$age,  names_pool$Pop),probs = c(0.75))
      
      
      return(list(mean = mean, median = median,
                  first_quartile = twentyfifth.quantile,Third_quartile = seventyfifth.quantile))
    }

    
    
    tr = setwd("~/Desktop/Data Science Project/nameagecalculator/names/F") 
    file_listing = list.files(path = tr)
    names_pool = Final.dataset
    new <- as.character(names_pool$firstname)
    new <- paste0(new,"-")
    main.data <- matrix(0,length(new),3)
    for(i in 1:length(new))
    {
      index <- which(str_detect(list.files(pattern = "-stats.txt$", recursive = TRUE),new[i]) == TRUE)
      if(length(index) != 0)
      {
        vas <- read.table(list.files(pattern = "-stats.txt$", recursive = TRUE)[index], header = F)
        flag <- as.character(vas[2,])
        main.data[i,2] <- as.numeric(str_sub(flag,1,4))
        main.data[i,1] <- as.numeric(str_sub(flag,6,9))
        main.data[i,3] <- as.numeric(str_sub(flag,11,14))
      }
      
      else
        main.data[i,] = 0
      
      #print(i)   
    }
    main.data <- as.data.frame(main.data)
    colnames(main.data) <- c("3rd_quartile","Median","1st_quartile")
    data.complet <- cbind(names_pool, main.data)
    current_year = as.numeric(substr(Sys.time(),1,4))
    
    which(colnames(data.complet)=="1st_quartile" ) #16
    which( colnames(data.complet)=="3rd_quartile" ) #14
    which( colnames(data.complet)=="Median" ) #15
    
    First_quartile_age = current_year - data.complet[,16] 
    Median_age = current_year - data.complet[,15]
    Third_quartile_age = current_year - data.complet[,14]
    
    data.complet <- cbind(data.complet, First_quartile_age, Median_age,Third_quartile_age)
    data.complet <- data.complet[which(data.complet$First_quartile_age >= 35 & data.complet$Third_quartile_age <= 90),]





#VIIII. Looking for Variability in Race , based on the code I had above, I will extract the variable races

# Since I had done the work already up top, I will just merge my current dataset with the race dataset
# we are identifying age based on surnames(based refer above for more details on line 93)


#colnames(merge.1)[which(names(merge.1) == "colname")] <- "Absence-Presence_Cancer"
#race.dataset = race.pred(dataset.name.gender, races = c("white", "black", "latino", "asian", "other"), name.clean = TRUE, surname.only = TRUE)
      race_predict = race.dataset[,c("firstname","surname","lastname","gender","pred.whi","pred.bla","pred.his","pred.asi","pred.oth")]
      race_predict = race_predict[!duplicated(race_predict), ]
      data.variability.race = left_join(data.complet,race_predict)
      surnames = race.dataset$surname
      
      view.race =  race_predict[match(surnames,race_predict$surname),]
      
      view.race = view.race[!duplicated(view.race), ]
      
      view.race.sans.surname = view.race[,-(1:4)]
      
      data.variability.race = left_join(data.complet,race_predict)
      surnames = data.variability.race$surname
      
      view.race =  race_predict[match(surnames,data.variability.race$surname),]

#view.race = view.race[!duplicated(view.race), ]

    view.race.sans.surname = view.race[,-(1:4)]
    
    #data.complet <- cbind(data.complet,race)
    
    # identifying duplicate columns
    
    #data.complet = data.complet[!duplicated(lapply(data.complet, summary))]
    
    data.complet$race.dicho <- ifelse(data.complet$race == "white",1,ifelse(data.complet$race=="black",2,
                                                                        ifelse(data.complet$race=="hispanic",3,ifelse(data.complet$race == "asian",4,5 ))))


# Use Race.predict already created that has the races

    # Select Only Needed columns 
    
    race_pb = data.variability.race[ ,which(names(data.variability.race) %in% c("surname","pred.whi","pred.bla","pred.his","pred.asi","pred.oth" ))]
    
    race_those_greater_0.01 <- function(x)
    {
      return(which(as.matrix(x) > 0.001))
    }
    race_newer <- apply(as.matrix(race_pb[ , -which(names(race_pb) %in% c("surname"))]), 1, race_those_greater_0.01)
    
    
# Eliminate the "Other"gender and Keep the other 4's
    decoding <- function(x)
    {
      if(length(which(x == 5)) > 0)
        x = x[-which(x == 5)]
      return(x)
    }
    
    change_code <- lapply(race_newer, decoding)
    
    firstname.last = data.variability.race[,c("id","firstname","lastname")]  
    data.complet  = left_join(data.complet,firstname.last)   

# Variability in age from 1st quartile to third quartile 


      age_sequence <- list()
      for ( i in 1:nrow(data.complet))
        
      {
        ages = seq( data.complet$First_quartile_age[i],data.complet$Third_quartile_age[i],1)
        matrix.data = matrix(0,nrow = length(ages), ncol = 7)
        T_small = ages + 5
        T_small = ifelse(T_small <= 90, T_small,90)
        ID = seq(1,length(ages),1)
        N_Biop = rep(99,length(ages))# repeat 99 for all rows, this are missing
        HypPlas = rep(99,length(ages))# repeat 99 for all rows, this are missing
        Agemen = rep(99, length(ages))# repeat 99 for all rows, this are missing
        Age1st = rep(99,length(ages)) # repeat 99 for all rows, this are missing
        N_Rels = rep(99,length(ages))# repeat 99 for all rows, this are missing
        T_90 = rep(90,length(ages))# repeat 99 for all rows, this are missing
        race = rep(data.complet$race.dicho[i],length(ages))
        five_data = as.data.frame(cbind(ID,ages,T_small,N_Biop,HypPlas,Agemen,Age1st,N_Rels,race))
        colnames(five_data) = c("ID","T1","T2","N_Biop","HypPlas","AgeMen","Age1st","N_Rels","Race")
        five.lifetime = as.data.frame(cbind(ID,ages,T_90,N_Biop,HypPlas,Agemen,Age1st,N_Rels,race))
        colnames(five.lifetime) = c("ID","T1","T2","N_Biop","HypPlas","AgeMen","Age1st","N_Rels","Race")
        abs_five = absolute.risk(five_data,iloop = 1)
        abs_five.i = absolute.risk(five_data,iloop = 2)
        abs_ninety = absolute.risk(five.lifetime,iloop = 1)
        abs_ninety.i = absolute.risk(five.lifetime,iloop = 2)
        matrix.data[,1] =  abs_five #arrange the data in each column ,column 1
        matrix.data[,2] =  abs_five.i #arrange the data in each column ,column 2
        matrix.data[,3] =  abs_ninety ##arrange the data in each column ,column 3
        matrix.data[,4] =  abs_ninety.i #arrange the data in each column ,column 4
        matrix.data[,5] =  ages #arrange the data in each column ,column 5
        matrix.data[,6] = rep(data.complet$firstname[i])  # column 6,repeat for each row the names by the number, using length for each i,
        # so merge first firstname to data.complet
        matrix.data[,7] = race
        age_sequence[[i]] <- matrix.data
      }
      
      age1 <- (do.call(rbind, age_sequence))
# Add column names
    colnames(age1) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","age","firstname","race")
    view.age = as.data.frame(unlist(age1))
    view.age$`Absolute risk(5 years)` = as.numeric(as.character(view.age$`Absolute risk(5 years)`))
    view.age $`Average absolute risk(5 years)`= as.numeric(as.character(view.age$`Average absolute risk(5 years)`))
    view.age$`Lifetime absolute risk` = as.numeric(as.character(view.age$`Lifetime absolute risk`))
    view.age$`Average lifetime risk` = as.numeric(as.character(view.age$`Average lifetime risk`))
    
    view.age$`Absolute risk(5 years)`= round(view.age$`Absolute risk(5 years)`,2)
    view.age$`Average absolute risk(5 years)`= round(view.age$`Average absolute risk(5 years)`,2)
    view.age$`Lifetime absolute risk` = round(view.age$`Lifetime absolute risk`,2)
    view.age$`Average lifetime risk` = round(view.age$`Average lifetime risk`,2)


# Now plot distribution of absolute risk per age and ethnicity 
# Looking at the distribution without including variation of age and race, median being the best age and race having highest probability

      median_sequence <- list()
      for ( i in 1:nrow(data.complet))
        
      {
        last_naming <- data.complet$lastname[[i]]
        ages = data.complet$Median_age[[i]]
        matrix.data = matrix(0,nrow = length(last_naming), ncol = 8)
        ID = seq(1,length(last_naming),1)
        T_small = ages + 5
        race = data.complet$race.dicho[[i]]
        T_small = ifelse(T_small <= 90, T_small,90)
        N_Biop = 99
        HypPlas = 99
        Agemen = 99
        Age1st = 99
        N_Rels = 99
        T_90 = 90
        five_data = as.data.frame(cbind(ID,ages,T_small,N_Biop,HypPlas,Agemen,Age1st,N_Rels,race))
        colnames(five_data) = c("ID","T1","T2","N_Biop","HypPlas","AgeMen","Age1st","N_Rels","Race")
        five.lifetime = as.data.frame(cbind(ID,ages,T_90,N_Biop,HypPlas,Agemen,Age1st,N_Rels,race))
        colnames(five.lifetime) = c("ID","T1","T2","N_Biop","HypPlas","AgeMen","Age1st","N_Rels","Race")
        abs_five = absolute.risk(five_data,iloop = 1) #create absolute risk
        abs_five.i = absolute.risk(five_data,iloop = 2) #create average absolute risk
        abs_ninety = absolute.risk(five.lifetime,iloop = 1)
        abs_ninety.i = absolute.risk(five.lifetime,iloop = 2)
        matrix.data[,1] =  abs_five #column 1
        matrix.data[,2] =  abs_five.i #column 2
        matrix.data[,3] =  abs_ninety #column 3
        matrix.data[,4] =  abs_ninety.i #column 4
        matrix.data[,5] =  last_naming #column 5
        matrix.data[,6] = data.complet$firstname[i] 
        matrix.data[,7] = race
        matrix.data[,8] = ages
        median_sequence[[i]] <- matrix.data
      }


      medians <- (do.call(rbind, median_sequence))
      colnames(medians) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk",
                             "Average lifetime risk","lastname","firstname","race","ages")
      
      medians.age = as.data.frame(unlist(medians))
      medians.age$`Absolute risk(5 years)` = as.numeric(as.character(medians.age$`Absolute risk(5 years)`))
      medians.age $`Average absolute risk(5 years)`= as.numeric(as.character(medians.age$`Average absolute risk(5 years)`))
      medians.age$`Lifetime absolute risk` = as.numeric(as.character(medians.age$`Lifetime absolute risk`))
      medians.age$`Average lifetime risk` = as.numeric(as.character(medians.age$`Average lifetime risk`))
      
      medians.age$`Absolute risk(5 years)`= round(medians.age$`Absolute risk(5 years)`,2)
      medians.age$`Average absolute risk(5 years)`= round(medians.age$`Average absolute risk(5 years)`,2)
      medians.age$`Lifetime absolute risk` = round(medians.age$`Lifetime absolute risk`,2)
      medians.age$`Average lifetime risk` = round(medians.age$`Average lifetime risk`,2)

#ggplot(medians.age, aes(medians.age$race)) + geom_bar()

    ggplot(medians.age,aes(x=ages, y=`Absolute risk(5 years)`))+ 
      geom_point(aes(colour=race, group=race)) + scale_x_discrete(breaks=c("35","45","55","65","75")) + 
      #scale_fill_manual(labels=c("white", "black", "hispanic","asian"))
    scale_colour_discrete(name  ="race",labels=c("white", "black", "hispanic","asian")) + labs(x = "age")  
    
    
    ggplot(medians.age,aes(x=ages, y=`Lifetime absolute risk`))+ 
      geom_point(aes(colour=race, group=race)) + scale_x_discrete(breaks=c("35","45","55","65","75")) + 
      #scale_fill_manual(labels=c("white", "black", "hispanic","asian"))
      scale_colour_discrete(name  ="race",
                            
                            labels=c("white", "black", "hispanic","asian"))   +  labs(x = "age")  
    

#Subset four names 

    name.1 = as.data.frame(unlist(age_sequence[[1051]]))
    name.2 = as.data.frame(unlist(age_sequence[[1049]]))
    name.3 = as.data.frame(unlist(age_sequence[[198]]))
    name.4 = as.data.frame(unlist(age_sequence[[600]]))
    colnames(name.1) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","age","firstname")
    colnames(name.2) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","age","firstname")
    colnames(name.3) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","age","firstname")
    colnames(name.4) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","age","firstname")

    
    name.3$`Absolute risk(5 years)` = as.numeric(as.character(name.3$`Absolute risk(5 years)`))
    name.3$`Average absolute risk(5 years)`  =as.numeric(as.character(name.3$`Average absolute risk(5 years)`))
    name.3$`Lifetime absolute risk`  =as.numeric(as.character(name.3$`Lifetime absolute risk`))
    name.3$`Average lifetime risk`  =as.numeric(as.character(name.3$`Average lifetime risk`))
    name.3$age  =as.numeric(as.character(name.3$age))


      
      name.1$`Absolute risk(5 years)` = as.numeric(as.character(name.1$`Absolute risk(5 years)`))
      name.1$`Average absolute risk(5 years)`  =as.numeric(as.character(name.1$`Average absolute risk(5 years)`))
      name.1$`Lifetime absolute risk`  =as.numeric(as.character(name.1$`Lifetime absolute risk`))
      name.1$`Average lifetime risk`  =as.numeric(as.character(name.1$`Average lifetime risk`))
      name.1$age  =as.numeric(as.character(name.1$age))
      
      
      
      name.2$`Absolute risk(5 years)` = as.numeric(as.character(name.2$`Absolute risk(5 years)`))
      name.2$`Average absolute risk(5 years)`  =as.numeric(as.character(name.2$`Average absolute risk(5 years)`))
      name.2$`Lifetime absolute risk`  =as.numeric(as.character(name.2$`Lifetime absolute risk`))
      name.2$`Average lifetime risk`  =as.numeric(as.character(name.2$`Average lifetime risk`))
      name.2$age  =as.numeric(as.character(name.2$age))

      
      
      breaks <- pretty(range(name.1$`Absolute risk(5 years)`), n = nclass.FD(name.1$`Absolute risk(5 years)`), min.n = 1)
      bwidth <- breaks[2]-breaks[1]
      t =  ggplot(name.1, aes(`Absolute risk(5 years)`)) + geom_histogram(binwidth = bwidth,fill="white",colour="black") +
        geom_density(alpha=0.5, fill="#FF6666") +
        labs(title="Yvonne",x="Absolute risk(5 years)", y = "Density")#Overlay with transparent density plot
      # to reset graphics device

        
        
        done <- pretty(range(name.1$`Lifetime absolute risk`), n = nclass.FD(name.1$`Lifetime absolute risk`), min.n = 1)
        bd <- done[2]-done[1]
        s =ggplot(name.1, aes(`Lifetime absolute risk`)) + geom_histogram(binwidth = bd,fill="white",colour="black") +
          geom_density(alpha=0.5, fill="#FF6666") +
          labs(title="Yvonne",x="Absolute risk(Lifetime)", y = "Density")
        
        
        
        tr<- pretty(range(name.3$`Absolute risk(5 years)`), n = nclass.FD(name.3$`Absolute risk(5 years)`), min.n = 1)
        bh <- tr[2]-tr[1]
        v = ggplot(name.1, aes(`Absolute risk(5 years)`)) + geom_histogram(binwidth = bh,fill="white",colour="black") +
          geom_density(alpha=0.5, fill="#FF6666") +
          labs(title="Cindy",x="Absolute risk(5 years)", y = "Density")
        
        
        vs <- pretty(range(name.3$`Lifetime absolute risk`), n = nclass.FD(name.3$`Lifetime absolute risk`), min.n = 1)
        gh <- vs[2]-vs[1]
        w = ggplot(name.3, aes(`Lifetime absolute risk`)) + geom_histogram(binwidth = gh,fill="white",colour="black") +
          geom_density(alpha=0.5, fill="#FF6666") +
          labs(title="Cindy",x="Absolute risk(Lifetime)", y = "Density")#Overlay with transparent density plot
        # to reset graphics device #dev.off()
        
        grid.arrange(t, s, v, w , ncol=2, nrow =2) # empirical on top of theoritical distribution, either remove density
        # or remove histogram 
        
        
# Now see variability in Race
        race_sequence <- list()
        for ( i in 1:nrow(data.complet))
          
        {
          race_changing <- change_code[[i]]
          ages = rep(data.complet$Median_age[i],length(race_changing))
          matrix.data = matrix(0,nrow = length(race_changing), ncol = 6)
          ID = seq(1,length(race_changing),1)
          T_small = ages + 5
          T_small = ifelse(T_small <= 90, T_small,90)
          N_Biop = rep(99,length(race_changing))# repeat 99 for all rows, this are missing
          HypPlas = rep(99,length(race_changing))# repeat 99 for all rows, this are missing
          Agemen = rep(99, length(race_changing))# repeat 99 for all rows, this are missing
          Age1st = rep(99,length(race_changing))# repeat 99 for all rows, this are missing
          N_Rels = rep(99,length(race_changing))# repeat 99 for all rows, this are missing
          T_90 = rep(90,length(race_changing))# repeat 99 for all rows, this are missing
          #race = rep(data.complet$race.dicho[i],length(race_changing))
          five_data = as.data.frame(cbind(ID,ages,T_small,N_Biop,HypPlas,Agemen,Age1st,N_Rels,race_changing))
          colnames(five_data) = c("ID","T1","T2","N_Biop","HypPlas","AgeMen","Age1st","N_Rels","Race")
          five.lifetime = as.data.frame(cbind(ID,ages,T_90,N_Biop,HypPlas,Agemen,Age1st,N_Rels,race_changing))
          colnames(five.lifetime) = c("ID","T1","T2","N_Biop","HypPlas","AgeMen","Age1st","N_Rels","Race")
          abs_five = absolute.risk(five_data,iloop = 1) #create absolute risk
          abs_five.i = absolute.risk(five_data,iloop = 2) #create average absolute risk
          abs_ninety = absolute.risk(five.lifetime,iloop = 1)
          abs_ninety.i = absolute.risk(five.lifetime,iloop = 2)
          matrix.data[,1] =  abs_five #column 1
          matrix.data[,2] =  abs_five.i #column 2
          matrix.data[,3] =  abs_ninety #column 3
          matrix.data[,4] =  abs_ninety.i #column 4
          matrix.data[,5] =  race_changing #column 5
          matrix.data[,6] = rep(data.complet$firstname[i])  # repeat for each row the names by the number, using length for each i,
          # didnt merge firstname to data.complet
          race_sequence[[i]] <- matrix.data
        }
        
        
        Race1 <- (do.call(rbind, race_sequence))
        
        view.race = Race1[!duplicated(Race1), ]
        
        colnames(view.race) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","race","firstname")
        
        
        #Extract distinct races
        
        name.5 = as.data.frame(unlist(race_sequence[[1051]]))
        name.6 = as.data.frame(unlist(race_sequence[[1049]]))
        name.7 = as.data.frame(unlist(race_sequence[[198]]))
        name.8 = as.data.frame(unlist(race_sequence[[600]]))
        colnames(name.5) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","race","firstname")
        colnames(name.6) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","race","firstname")
        colnames(name.7) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","race","firstname")
        colnames(name.8) <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk","race","firstname")
        
        
        
        
        name.5$`Absolute risk(5 years)`  = as.numeric(as.character(name.5$`Absolute risk(5 years)`))
        name.6$`Absolute risk(5 years)`  = as.numeric(as.character(name.6$`Absolute risk(5 years)`))
        name.7$`Absolute risk(5 years)`  = as.numeric(as.character(name.7$`Absolute risk(5 years)`))
        name.8$`Absolute risk(5 years)`  = as.numeric(as.character(name.8$`Absolute risk(5 years)`))



      name.5$`Lifetime absolute risk` = as.numeric(as.character(name.5$`Lifetime absolute risk`))
      name.6$`Lifetime absolute risk`  = as.numeric(as.character(name.6$`Lifetime absolute risk`))
      name.7$`Lifetime absolute risk`  = as.numeric(as.character(name.7$`Lifetime absolute risk`))
      name.8$`Lifetime absolute risk`  = as.numeric(as.character(name.8$`Lifetime absolute risk`))
      
      
      p = ggplot(data = name.5 , aes( x = race, y = `Absolute risk(5 years)`),colour = factor(race)) +geom_point(size=3, shape=21, fill="lightblue", alpha = 0.75) +
        labs(title="Yvonne",x="race range", y = "Absolute risk(5 years)")
      g = ggplot(data = name.5 , aes( x = race, y = `Lifetime absolute risk`),colour = factor(race)) +geom_point(size=3, shape=21, fill="lightgreen", alpha = 0.75) +
        labs(title="Yvonne",x="race range", y = "Lifetime absolute risk")
      w = ggplot(data = name.7 , aes( x = race, y = `Absolute risk(5 years)`),colour = factor(race)) +geom_point(size=3, shape=21, fill="lightblue", alpha = 0.75)+
        labs(title="Cindy",x="race range", y = "Absolute risk(5 years)")
      l = ggplot(data = name.7 , aes( x = race, y = `Lifetime absolute risk`),colour = factor(race)) +geom_point(size=3, shape=21, fill="lightgreen", alpha = 0.75) +
        labs(title="Cindy",x="race range", y = "Lifetime absolute risk")
      
      grid.arrange(p, g, w, l, ncol=2, nrow =2)

#Function with combination of age and race with 5years projection         
      race_age_all <- list()
      for(i in 1: nrow(data.complet))
      {
        races_change <- change_code[[i]]
        age_with_variations <- seq(data.complet$First_quartile_age[i], data.complet$Third_quartile_age[i],1)
        age.race.varation <- expand.grid(age_with_variations,races_change)#create combinations
        colnames(age.race.varation) <- c("age.variation", "race.variation")
        T1 <- age.race.varation$age.variation
        Race <- age.race.varation$race.variation
        N_Biop <- rep(99, nrow(age.race.varation)) # repeat 99 for all rows, this are missing
        HypPlas <- rep(99, nrow(age.race.varation)) # repeat 99 for all rows, this are missing
        AgeMen <- rep(99, nrow(age.race.varation))# repeat 99 for all rows, this are missing
        Age1st <- rep(99, length(age.race.varation))# repeat 99 for all rows, this are missing
        N_Rels <- rep(99, nrow(age.race.varation))# repeat 99 for all rows, this are missing
        m <- matrix(0, nrow = nrow(age.race.varation), ncol = 6)
        ID <- seq(1, nrow(age.race.varation), 1)
        T2 <- T1 + 5 # 5 years
        T2 <- ifelse(T2 <= 90, T2, 90) #plafond
        final_data_five <- as.data.frame(cbind(ID, T1, T2, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, Race))#create a dataframe
        race_age_all[[i]] <- final_data_five
      }      


# Function with combination race with a plafond of lifetime age        
        race_age_all_lifetime <- list()
        for(i in 1: nrow(data.complet))
        {
          races_change = change_code[[i]]
          age_with_variations = seq(data.complet$First_quartile_age[i], data.complet$Third_quartile_age[i],1)
          age.race.variation = expand.grid(age_with_variations,races_change)#create combinations
          colnames(age.race.variation) = c("age.variation", "race.variation")
          T1 <- age.race.variation$age.variation
          Race <- age.race.variation$race.variation
          ID <- seq(1, nrow(age.race.variation), 1)
          T2 = rep(90, nrow(age.race.variation))
          N_Biop = rep(99, nrow(age.race.variation))
          HypPlas = rep(99, nrow(age.race.variation))
          AgeMen = rep(99, nrow(age.race.variation))
          Age1st = rep(99, length(age.race.variation))
          N_Rels = rep(99, nrow(age.race.variation))
          final_data_five = as.data.frame(cbind(ID, T1, T2, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, Race))
          race_age_all_lifetime[[i]] = final_data_five
        }      




#Calculate confidence intervals for uncertainty 
# Making the transition from serial to parallel change  %do% to %dopar%.
# Using Library parallel() is a native R library, no CRAN required 
#The parallel package is basically about doing the above in parallel.
# set up a cluster, 
#a collection of “workers” that will be doing the job such as the numbers of available cores – 1. 


    # Calculate the number of cores
    no_cores <- detectCores() - 1
    # Initiate cluster
    registerDoParallel(no_cores)
    
    n <- length(race_sequence)
    racerisk_lifetime <- foreach(i = 1:n,.packages = ("BCRA")) %dopar%
      absolute.risk( race_age_all_lifetime[[i]], iloop = 1)
    racerisk_5.years <- foreach(i = 1:n,.packages = ("BCRA")) %dopar%
      absolute.risk(race_age_all[[i]], iloop = 1)
    # stop the cluster and remove parallel instances
    stopImplicitCluster()

# median and quartiles for each list element
    lifetime = lapply(racerisk_lifetime, quantile, probs = 1:3/4,na.rm = TRUE) #confidence interval for 5 years projection
    five.years = lapply(racerisk_5.years, quantile, probs = 1:3/4, na.rm = TRUE) #confidence interval for lifetime
    fiveyears.standard.deviation = lapply(racerisk_5.years, sd, na.rm = TRUE) # standard deviation for 5 years projection
    lifetime.standard.deviation = lapply(racerisk_lifetime, sd, na.rm = TRUE) #confidence interval for lifetime 
    
    
    #https://www.r-bloggers.com/concatenating-a-list-of-data-frames/
    confidence.interval = as.data.frame(cbind(data.complet$firstname,data.complet$lastname,data.complet$race.dicho,
                                              do.call(rbind,lifetime),do.call(rbind,five.years)))
    
    
    #Rename data frame 
    colnames(confidence.interval) <- c("Firstname", "Lastname","lifetime_absolute_risk_1stquartile", 
                                       "lifetime_absolute_risk_median", "lifetime_absolute_risk_3rdquartile", 
                                       "5years_risk_1stquartile", "5years_risk_median", 
                                       "5years_risk_3rdquartile")
    # change dataypes to obtained decimals 

      confidence.interval$Lastname = as.character(confidence.interval$Lastname)
      confidence.interval$Firstname = as.character(confidence.interval$Firstname)
      
      confidence.interval$lifetime_absolute_risk_1stquartile  =as.numeric(as.character(confidence.interval$lifetime_absolute_risk_1stquartile)) 
      confidence.interval$lifetime_absolute_risk_median  =as.numeric(as.character(confidence.interval$lifetime_absolute_risk_median)) 
      confidence.interval$lifetime_absolute_risk_3rdquartile  =as.numeric(as.character(confidence.interval$lifetime_absolute_risk_3rdquartile)) 
      confidence.interval$`5years_risk_1stquartile`  =as.numeric(as.character(confidence.interval$`5years_risk_1stquartile`)) 
      confidence.interval$`5years_risk_median`  =as.numeric(as.character(confidence.interval$`5years_risk_median`)) 
      confidence.interval$`5years_risk_3rdquartile` =as.numeric(as.character(confidence.interval$`5years_risk_3rdquartile`))
#confidence.interval$sigma = rep(0,nrow(confidence.interval))



# Exploratory Plots 
    #hist(confidence.interval$lifetime_absolute_risk_3rdquartile, freq=FALSE, xlab="lifetime_3rdQ", main="Distribution of Upper C.I/lifetime projection", col="lightgreen")
    #curve(dnorm(x, mean=mean(confidence.interval$lifetime_absolute_risk_3rdquartile), sd=sd(confidence.interval$lifetime_absolute_risk_3rdquartile)), add=TRUE, col="darkblue", lwd=2)
    #hist(confidence.interval$lifetime_absolute_risk_1stquartile, freq=FALSE, xlab="lifetime_1stQ", main="Distribution of lower C.I/lifetime projection", col="lightgreen")
    #curve(dnorm(x, mean=mean(confidence.interval$lifetime_absolute_risk_1stquartile), sd=sd(confidence.interval$lifetime_absolute_risk_1stquartile)), add=TRUE, col="darkblue", lwd=2)
    #hist(confidence.interval$`5years_risk_1stquartile`, freq=FALSE, xlab="lifetime_1stQ", main="Distribution of lower C.I/5year projection", col="lightgreen")
    #curve(dnorm(x, mean=mean(confidence.interval$`5years_risk_1stquartile`), sd=sd(confidence.interval$`5years_risk_1stquartile`)), add=TRUE, col="darkblue", lwd=2)
    #hist(confidence.interval$`5years_risk_3rdquartile`, freq=FALSE, xlab="lifetime_3rdQ", main="Distribution of upper C.I/5year projection", col="lightgreen")
    #curve(dnorm(x, mean=mean(confidence.interval$`5years_risk_3rdquartile`), sd=sd(confidence.interval$`5years_risk_3rdquartile`)), add=TRUE, col="darkblue", lwd=2)

      #check relation of lifetime risk and 5 years projection 
      
      op = as.data.frame(do.call(rbind,race_sequence))
      colnames(op) <- c("Absolute risk(5 years)","Average absolute risk(5 years)",
                        "Lifetime absolute risk","Average lifetime risk","race","firstname")
      
      
      colnames(op)[which(names(op) == "V1")] <- "Absolute risk(5 years)"
      colnames(op)[which(names(op) == "V2")] <- "Average absolute risk(5 years)"
      colnames(op)[which(names(op) == "V3")] <- "Lifetime absolute risk"
      colnames(op)[which(names(op) == "V4")] <- "Average lifetime risk"
      colnames(op)[which(names(op) == "V5")] <- "race"
      colnames(op)[which(names(op) == "V6")] <- "firstname"
      
      
      op$`Absolute risk(5 years)`  =as.numeric(as.character(op$`Absolute risk(5 years)`)) 
      op$`Average absolute risk(5 years)`  =as.numeric(as.character(op$`Average absolute risk(5 years)`)) 
      op$`Lifetime absolute risk`  =as.numeric(as.character(op$`Lifetime absolute risk`)) 
      op$`Average lifetime risk`  =as.numeric(as.character(op$`Average lifetime risk`)) 



#Histogram of the combinations for race risk lifetime and projection of 5 years
      Racelifetime <- (do.call(rbind, racerisk_lifetime))
      Racerisk <- (do.call(rbind, racerisk_5.years))
      
      race.lif = unlist(racerisk_lifetime)
      race.5years = unlist(racerisk_5.years )
      
      hist(race.lif, ylim = c(0,0.3),freq=FALSE, xlab="lifetime", main="Distribution of lifetime risk/combination of race & age", col="lightblue")
      curve(dnorm(x, mean=mean(race.lif), sd=sd(race.lif)), add=TRUE, col="red", lwd=2)
      
      hist(race.5years , ylim = c(0,1.8),freq=FALSE, xlab="5 years projetion", main="Distribution of 5 years absolute risk/combination of race & age", col="lightblue")
      curve(dnorm(x, mean=mean(race.5years), sd=sd(race.5years)), add=TRUE, col="red", lwd=2)



