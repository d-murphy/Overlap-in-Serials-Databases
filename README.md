## Overview

Serials databases purchased by academic libraries often overlap meaning access to a particular journal can be paid for several times.  When evaluating database purchases or renewals, this overlap makes it difficult to see a database's actual value in the library's collection.  Using the script below, a library can identify which journals in each package are provided elsewhere in their other subscriptions.  Additionally, the script compares coverage dates in other packages to confirm the journals exact coverage is reproduced elsewhere.  The script produces a report showing the percentage overlap of all databases along with individual reports for each database showing the journals contained therein and where other coverage exists.  

## Script

### Data Cleaning

Load Packages

```{r}
library(lubridate)
library(dplyr)
library(tidyr)
```

Load the data set.  Paste ID and Title to assure Title duplicates are not misinterpreted.  Also corrects the odd column name for ID resulting from the import.  

```{r}
JournalData <- read.csv2("C:/Users/murph/Desktop/PackagesList.csv", header=TRUE, 
                         sep=",", stringsAsFactors = FALSE)


JournalData$KBID_Title = paste(JournalData$ï..KBID, JournalData$Title, sep=" - ")

```

Data set includes Managed Date and Custom Date.  If Custom Date exists, it overrides managed date.  

```{r}
JournalData$TrueCoverageBegin <- ifelse(nchar(JournalData$CustomCoverageBegin)==0, 
                                        JournalData$ManagedCoverageBegin,
                                        JournalData$CustomCoverageBegin)


JournalData$TrueCoverageEnd <- ifelse(nchar(JournalData$CustomCoverageEnd)==0, 
                                      JournalData$ManagedCoverageEnd,
                                      JournalData$CustomCoverageEnd)
```

Get current date and convert values of "Present" to today's date.  Update values in column to date format.  

```{r}
DateAsChar <- as.character(Sys.Date())
JournalData$TrueCoverageEnd <- ifelse(JournalData$TrueCoverageEnd=="Present",
                                          DateAsChar,JournalData$TrueCoverageEnd)
JournalData$TrueCoverageEnd <- ymd(JournalData$TrueCoverageEnd)
```

Some journals are added to the databases with an embargo period which delays access to current articles for a period of time.  Below the embargo time is interpreted and subtracted from the Coverage End date, if one exists.  Format adjustments are also made.  

```{r}
JournalData <- separate(data=JournalData, col = Embargo, into = c("TimeValue", "TimeUnit"), 
                        sep = " ", fill="right")
JournalData$EmbargoUnitNum <- ifelse(JournalData$TimeUnit=="years",365,
                                 ifelse(JournalData$TimeUnit=="months",30, 1))
JournalData$EmbargoUnitNum <- as.numeric(JournalData$EmbargoUnitNum)
JournalData$TimeValue <- as.numeric(JournalData$TimeValue) 
JournalData$EmbargoDays <- JournalData$TimeValue*JournalData$EmbargoUnitNum
dateclassvec <- class(JournalData$TrueCoverageEnd)
JournalData$TrueCoverageEnd <- ifelse(is.na(JournalData$EmbargoDays),
                                      JournalData$TrueCoverageEnd,
                                      JournalData$TrueCoverageEnd-days(JournalData$EmbargoDays))
class(JournalData$TrueCoverageEnd) <- dateclassvec
```

Coverage Begin Date format is adjusted.  The duration of a database's coverage of each journal is calculated.  

```{r}
JournalData$TrueCoverageBegin <- ymd(JournalData$TrueCoverageBegin)
JournalData$CoverageLength <- difftime(JournalData$TrueCoverageEnd, JournalData$TrueCoverageBegin, units = "days")
```

### Data Analysis

First, a list of database names is created as PackagesList.  The list is updated to remove Packages which should not be a part of the analysis (e.g. Access to package is only a trial basis.).  Then, the original data set is trimmed to only include journals from the packages being investigated.  Last, the data frame containing previous results is cleared.      

```{r}
PackagesList <- JournalData %>% select(PackageName) %>% distinct() %>% arrange(PackageName)
PackagesList <- as.data.frame(PackagesList[-c(3,20,24,28,55),])
colnames(PackagesList) <- "PackageName"
JournalData <- JournalData %>% filter(PackageName %in% PackagesList$PackageName)
results <- NULL
```

A for loop steps through the list of databases.  For each database, the list of journals within the database is extracted.  This list of journals is then temporarily removed from the original data set of all other packages.  Then the original data set is filtered to remove the journals not found in the database being investigated.  For each journal in this remainder of the original data set, the database with the earliest coverage of the particular journal, the database with the latest coverage, and the database with the longest coverage are selected.  These findings are then merged in a data frame with the listing of journals in the database being investigated.  Last, a new column is created in this data frame which is marked 1 if one of the three databases identified have coverage that overlaps the coverage provided by the database being investigated.     

Additional notes are included.  

```{r}
#Step through Packages
for(i in 1:dim(PackagesList)[1])
{

#Set PackName = to Package to Test
PackName <- PackagesList$PackageName[i]

### Gets journal list in package along with dates to Investigate
PackNameJournals <- JournalData %>% filter(PackageName == PackName) %>% select (KBID_Title, PackageName, 
                                                                                TrueCoverageBegin,
                                                                                TrueCoverageEnd,
                                                                                EmbargoDays,
                                                                                HideOnPublicationFinder,
                                                                                CoverageLength)

### Gets the earliest (longest earliest if ties) coverage for each journal in other packages
PossibleOverlapEarliest <- JournalData %>% 
  filter(PackageName != PackName) %>% 
  filter(KBID_Title %in% PackNameJournals$KBID_Title) %>%
  group_by(KBID_Title) %>%
  arrange(desc(CoverageLength)) %>%
  mutate(EarliestPackageCov = row_number(TrueCoverageBegin)) %>%
  filter(EarliestPackageCov == 1) %>% 
  select(PackageName, KBID_Title, HideOnPublicationFinder, TrueCoverageBegin, TrueCoverageEnd)

### Joins Investigated journals with earliest coverage in other packages.  
PackNameJournalsWithRep <- left_join(PackNameJournals,PossibleOverlapEarliest, by="KBID_Title")

### Update col names
colnames(PackNameJournalsWithRep)[c(2,3,4,5,6,7,8,9,10,11)] <- c("InvestigatedPackName", 
                                                                 "IPackCoverageStart",
                                                                 "IPackCoverageEndLessEmbargo",
                                                                 "IPackEmbargo",
                                                                 "IPackHideOnPubFinder",
                                                                 "IPackCoverageLength",
                                                                 "RepEarliestPackName",
                                                                 "RepEarliestHideOnPubFinder",
                                                                 "RepEarliestCoverageStart",
                                                                 "RepEarliestCoverageEndLessEmargo")

PackNameJournalsWithRep$CoveredByEarliest <- NULL
### Test if Earlist Replacement covers.  If so, print Replacement Name
PackNameJournalsWithRep$CoveredByEarliest <- ifelse(PackNameJournalsWithRep$IPackCoverageStart>=
                                                    PackNameJournalsWithRep$RepEarliestCoverageStart &
                                                    PackNameJournalsWithRep$IPackCoverageEndLessEmbargo<=
                                                    PackNameJournalsWithRep$RepEarliestCoverageEndLessEmargo, 
                                                    PackNameJournalsWithRep$RepEarliestPackName,NA)                                                               

##### Run the same test for possible replacements with the latest coverage
PossibleOverlapLatest <- JournalData %>% 
  filter(PackageName != PackName) %>% 
  filter(KBID_Title %in% PackNameJournals$KBID_Title) %>%
  group_by(KBID_Title) %>%
  arrange(desc(CoverageLength)) %>%
  mutate(LatestPackageCov = row_number(desc(TrueCoverageEnd))) %>%
  filter(LatestPackageCov == 1) %>% 
  select(PackageName, KBID_Title, HideOnPublicationFinder, TrueCoverageBegin, TrueCoverageEnd)

PackNameJournalsWithRep <- left_join(PackNameJournalsWithRep,PossibleOverlapLatest, by="KBID_Title")

colnames(PackNameJournalsWithRep)[c(13,14,15,16)] <- c("RepLatestPackName",
                                                       "RepLatestHideOnPubFinder",
                                                       "RepLatestCoverageStart",
                                                       "RepLatestCoverageEndLessEmargo")

PackNameJournalsWithRep$CoveredByLatest <- NULL
PackNameJournalsWithRep$CoveredByLatest <- ifelse(PackNameJournalsWithRep$IPackCoverageStart>=
                                                  PackNameJournalsWithRep$RepLatestCoverageStart &
                                                  PackNameJournalsWithRep$IPackCoverageEndLessEmbargo<=
                                                  PackNameJournalsWithRep$RepLatestCoverageEndLessEmargo, 
                                                  PackNameJournalsWithRep$RepLatestPackName,NA)                                                               

##### Run the same test for replacement packages with the longest possible coverage
PossibleOverlapLongest <- JournalData %>% 
  filter(PackageName != PackName) %>% 
  filter(KBID_Title %in% PackNameJournals$KBID_Title) %>%
  group_by(KBID_Title) %>%
  mutate(LongestPackageCov = row_number(desc(CoverageLength))) %>%
  filter(LongestPackageCov == 1) %>% 
  select(PackageName, KBID_Title, HideOnPublicationFinder, TrueCoverageBegin, TrueCoverageEnd)

PackNameJournalsWithRep <- left_join(PackNameJournalsWithRep,PossibleOverlapLongest, by="KBID_Title")

colnames(PackNameJournalsWithRep)[c(18,19,20,21)] <- c("RepLongestPackName",
                                                       "RepLongestHideOnPubFinder",
                                                       "RepLongestCoverageStart",
                                                       "RepLongestCoverageEndLessEmargo")

PackNameJournalsWithRep$CoveredByLongest <- NULL
PackNameJournalsWithRep$CoveredByLongest <- ifelse(PackNameJournalsWithRep$IPackCoverageStart>=
                                                   PackNameJournalsWithRep$RepLongestCoverageStart &
                                                   PackNameJournalsWithRep$IPackCoverageEndLessEmbargo<=
                                                   PackNameJournalsWithRep$RepLongestCoverageEndLessEmargo, 
                                                   PackNameJournalsWithRep$RepLongestPackName,NA)                                                               


### Check three Replication possibilities to see if covered
PackNameJournalsWithRep$Covered <- NULL
PackNameJournalsWithRep$Covered <- ifelse(!is.na(PackNameJournalsWithRep$CoveredByEarliest),1,
                                    ifelse(!is.na(PackNameJournalsWithRep$CoveredByLatest),1,
                                      ifelse(!is.na(PackNameJournalsWithRep$CoveredByLongest),1,NA)))                                                               

results <- bind_rows(results, PackNameJournalsWithRep)

}
```

Here, the percentage of overlap within each package is calculated and written to a csv file.  

```{r}
Counts <- results %>% group_by(InvestigatedPackName) %>% count(InvestigatedPackName)
CoveredCounts <- results %>% group_by(InvestigatedPackName, Covered) %>% 
                             summarise(count=n()) %>%       
                             filter(Covered==1)
Counts <- left_join(Counts,CoveredCounts,by="InvestigatedPackName")
Counts <- Counts %>% mutate(CoveredPct = (count/n)*100)
Counts$Covered <- NULL

write.csv(Counts, paste("C:/Users/murph/Desktop/JournalReports/Counts.csv"))

```

Last, the journals within each database are printed as a csv file.  Included are also the Earliest, Latest, and Longest potential replacements and the indication of if the journal is covered in another database.  

```{r}
export <- NULL
for(i in 1:dim(PackagesList)[1])
{
  export <- results %>% filter(InvestigatedPackName == PackagesList$PackageName[i]) %>% arrange(desc(Covered))
  write.csv(export, paste("C:/Users/murph/Desktop/JournalReports/",i,".csv"))
}
### Index

PackagesListIndex <- PackagesList
PackagesListIndex$Index <- c(1:dim(PackagesList)[1])

write.csv(PackagesListIndex, paste("C:/Users/murph/Desktop/JournalReports/PackagesListIndex.csv"))
```

