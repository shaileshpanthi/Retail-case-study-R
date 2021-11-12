setwd("E:/Business alaysis 360/Assignments/R/Projects and assignment/Retail Case Study Assignment")

##Importing files

Customer_Info<- read.csv("E:/Business alaysis 360/Assignments/R/Projects and assignment/Case study 1/R - Retail Case study/R case study 1 (Retail)/Customer.csv")
Product_Info<- read.csv("E:/Business alaysis 360/Assignments/R/Projects and assignment/Case study 1/R - Retail Case study/R case study 1 (Retail)/prod_cat_info.csv")
Transactions_Info<- read.csv("E:/Business alaysis 360/Assignments/R/Projects and assignment/Case study 1/R - Retail Case study/R case study 1 (Retail)/Transactions.csv")

# Checking classes

class(Customer_Info)
class(Product_Info)
class(Transactions_Info)

# Checking data types

print(data.frame(sapply(Customer_Info,class)))
print(data.frame(sapply(Product_Info,class)))
print(data.frame(sapply(Transactions_Info,class)))

# Converting date data type from character to date

require(lubridate)

Transactions_Info$tran_date<- lubridate::dmy(Transactions_Info$tran_date)
class(Transactions_Info$tran_date)


Customer_Info$DOB<- lubridate::dmy(Customer_Info$DOB)
class(Customer_Info$DOB)


#########################################################################################
######################## Q1. Creating Customer final ####################################


#a. using base merge

Customer_Final <- merge(x=Transactions_Info,y=Customer_Info ,by.x = "cust_id",by.y = "customer_Id")
Customer_Final <- merge(x=Customer_Final,y= Product_Info ,by ="prod_cat_code",all = T)
head(Customer_Final)

#b. Using dplyr

require(dplyr)
Customer_Final1 <- full_join(Transactions_Info,Product_Info,by = "prod_cat_code")
Customer <- dplyr::rename(Customer_Info,"cust_id"="customer_Id")
Customer_Final1 <- full_join(Customer_Final1,Customer,by = "cust_id")

head(Customer_Final1)



###################################################################################################
######################## Q2. Summary report for merge data set ####################################



#a. columns with data types

Col_data_types<- (data.frame(sapply(Customer_Final,class)))

#b. top 10 observations

Top_ten_observations<-(data.frame(head(Customer_Final, n=10)))

#c. five number summary of continuous variables
summary(Customer_Final)


fivenum(Customer_Final$Qty)
fivenum(Customer_Final$Rate)
fivenum(Customer_Final$Tax)
fivenum(Customer_Final$total_amt)


#d. frequency table for categorical variables


Gender<- table(Customer_Final$Gender)

Product_cat<- table(Customer_Final$prod_cat)

product_subcat<- table(Customer_Final$prod_subcat)

store_type<- table(Customer_Final$Store_type)

City_code<- table(Customer_Final$city_code)

Prod_catagory_code<- table(Customer_Final$prod_cat_code)

product_subcat_code<- table(Customer_Final$prod_subcat_code)




################################################################################################################################
################ Q3. Histograms for continuous variables and frequency bar for categorical variables ###########################



# Histograms

histogram_qty<- hist(Customer_Final$Qty,main = "Histogram of Qty",xlab = "Quantity")

histogram_Rate<- hist(Customer_Final$Rate,main = "Histogram of Rate",xlab = "Rate")

histogra_Tax<- hist(Customer_Final$Tax,main = "Histogram of Tax",xlab = "Tax")

histogram_total_amt<- hist(Customer_Final$total_amt,main = "Histogram of Total Amount",xlab = "Total Amount")


# Frequency Bar

require(ggplot2)

Gender_freqbar<- barplot(Gender, main="Gender", col="Blue")

Product_cat_freqbar<- barplot(Product_cat, main = "Product Category", col = "Green")

product_subcat_freqbar<- barplot(product_subcat, main = "Product subcatagory", col = "Red")

store_type_freqbar<- barplot(store_type, main = "Store Type", col = "Orange")

City_code_freqbar <- barplot(City_code, main = "City", col = "pink")

Prod_catagory_code_freqbar<- barplot(Prod_catagory_code, main= "Product catagory code", col="Green")

product_subcat_code_freqbar<- barplot(product_subcat_code, main= "Product sub category codes", col="Red")


#################################################################################################################
################################ Q4. Calculate the following ####################################################


#a. Time period of the available transaction data


Time_Period <- print(max(Customer_Final$tran_date,na.rm = TRUE) - min(Customer_Final$tran_date,na.rm = TRUE))


#b. Count the transactions where total amount of transactions was negative

Negative_Totalamt_transactions <- length(which(Customer_Final$total_amt<0))


################################################################################################################
#################### Q5. Analyze which product categories are more popular among males and females #############

product_by_gender<- table(Customer_Final$Gender, Customer_Final$prod_cat)


barplot(product_by_gender, col= c("Blue", "Pink"), main= "Product popularity by Gender", beside= TRUE, legend=c("Male", "Female"))

#################################################################################################################
################ Q6. Which city code has maximum customers and percentage #######################################

require(dplyr)

City_wise_customer<- Customer_Final %>% group_by(city_code) %>% summarise(No_of_cust= length(city_code))
City_wise_customer$Percentage <- (City_wise_customer$No_of_cust/sum(City_wise_customer$No_of_cust)*100)
City_wise_customer$Percentage <- round(City_wise_customer$Percentage, digits= 3)

Max_city_Customer<- subset(City_wise_customer, Percentage= max(Percentage))
Max_City <- print(Max_city_Customer[which.max(City_wise_customer$Percentage),])

#Therefore City 4, having 10571 number of customers with 10.6% has the maximum customers.



#################################################################################################################
######################### Q7. Store type that sells maximum products by value and quantity ######################


Store_Type_Value <- Customer_Final %>% group_by(Store_type) %>% summarise(TOtalAmount= sum(total_amt,na.rm = TRUE))

Store_Type_Numbers <- Customer_Final %>% group_by(Store_type,cust_id) %>% summarise()

Store_Type_Quantity <- Store_Type_Numbers %>% group_by(Store_type) %>% summarise(TotalQuantity = length(Store_type))

Store_type_Value_Quant<- merge(Store_Type_Value, Store_Type_Quantity, all= TRUE)


#Store sells with maximum value

Store_max_value<- Store_type_Value_Quant[which.max(Store_type_Value_Quant$TOtalAmount),]

#Store sells with maximum Quantity

Store_max_Quantity<- Store_type_Value_Quant[which.max(Store_type_Value_Quant$TotalQuantity),]


################################################################################################################
################# Q8. Total amount earned from electronics and clothing from flagship stores ###################

Store_Type_cat_revenue<- Customer_Final %>% 
  group_by(Store_type, prod_cat) %>% 
  summarise(TotalRevenue= sum(total_amt, na.rm=TRUE))

Flagship_store<- Store_Type_cat_revenue[Store_Type_cat_revenue$Store_type== "Flagship store",]

Flagship_store_electronics<- Flagship_store[Flagship_store$prod_cat=="Electronics",]

print(Flagship_store_electronics)

Flagship_store_clothings<- Flagship_store[Flagship_store$prod_cat=="Clothing",]
print(Flagship_store_clothings)

####################################################################################################
################# Q9. Total amount earned from male customers under electronics category ###########

store_type_gender_cat_rev <- Customer_Final %>%
  group_by(Gender, prod_cat) %>%
  summarise(TotalRevenue= sum(total_amt, na.rm = TRUE))

Male_revenue<- store_type_gender_cat_rev[store_type_gender_cat_rev$Gender=="M",]
Male_revenue_electronics<- Male_revenue[Male_revenue$prod_cat=="Electronics",]
print(Male_revenue_electronics)

############################################################################################################################
### Q10. How many customers has more than 10 unique transactions after removing transactions with negative amount ##########

Positive_transactions_above_ten<- Customer_Final%>% dplyr::filter(total_amt>0) %>% select(cust_id)%>% count(cust_id) %>% dplyr::filter(n>10)
count(Positive_transactions_above_ten)

#############################################################################################################
########### Q11. For all customers between 25- 35 find out the following ###########################

### Preparing data for customers age between 25-35

Customer_Final$Age <- (Customer_Final$tran_date - Customer_Final$DOB)/365.25
Customer_Final$Age <- round(Customer_Final$Age,digits = 2)

Customer_25_to_35<- Customer_Final %>% dplyr::filter(Age>=25 & Age<= 35)
head(Customer_25_to_35)

#11.a. Total amount spent for electronics and books

Spent_BY_Catagories<- Customer_25_to_35 %>% group_by(prod_cat) %>% summarise(TotalSpent=sum(total_amt, na.rm=TRUE))

Spent_Books<- Spent_BY_Catagories[Spent_BY_Catagories$prod_cat=="Books",] 
print(Spent_Books)
Spent_Electronics<- Spent_BY_Catagories[Spent_BY_Catagories$prod_cat=="Electronics",]
print(Spent_Electronics)

#11.b. Total amount spent between 1st Jan 2014 to 1st Mar 2014

Spent_between_days<- Customer_25_to_35 %>% group_by(tran_date) %>% summarise(TotalSpent=sum(total_amt, na.rm = TRUE))

Date_req <- Spent_between_days[Spent_between_days$tran_date>="2014-01-01" & Spent_between_days$tran_date<="2014-03-01",]

TotalAmountSpent <-sum(Spent_between_days$TotalSpent)

print(TotalAmountSpent)
