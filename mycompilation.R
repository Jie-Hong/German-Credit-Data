#The code is separated into several sections by dashed lines. 
#The first section is the complete list of commands used on R console
#The other sections are functions that I created and are used in the commands 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#read original dataset
data<-read.table("C:/Users/User/Desktop/C4.5/germancredit.txt")

#add the header into the original dataset
column_names<-c("Status_of_existing_checking_account","Duration_in_month",
"Credit_history","Purpose","Credit_amount","Savings_account_bonds",
"Present_employment_since","Installment_rate_in_percentage_of_disposable_income",
"Personal_status_and_sex","Other_debtors_guarantors","Present_residence_since",
"Property","Age_in_years","Other_installment_plans","Housing",
"Number_of_existing_credits_at_this_bank","Job",
"Number_of_people_being_liable_to_provide_maintenance_for","Telephone",
"Foreign_worker","Class")
colnames(data)<-column_names

#reduce dimensions by choosing 3 out of 20 attributes based on Chi-square test
data_less_dimensions<-data[,c(1,2,3,21)]

#proportional stratified sampling: 70% training data, 30% test data
source("C:\\Users\\User\\Desktop\\C4.5\\mystratifiedsampling.R")
mystratifiedsampling(data_less_dimensions,700)

#read the training and test data into R
train_data<-read.csv("C:/Users/User/Desktop/C4.5/train_data.csv")
test_data<-read.csv("C:/Users/User/Desktop/C4.5/test_data.csv")

#Use C4.5 to build a DT based on the training data
source("C:\\Users\\User\\Desktop\\C4.5\\myC45.R")
myC45(train_data)

#pruning
leafnode1<-matrix(c(0,2,4,3),ncol=2,byrow=T)
leafnode2<-matrix(c(0,4,3,7),ncol=2,byrow=T)
leafnode3<-matrix(c(26,33,17,42),ncol=2,byrow=T)
leafnode4<-matrix(c(3,0,0,5),ncol=2,byrow=T)
leafnode5<-matrix(c(17,20,0,13),ncol=2,byrow=T)
leafnode6<-matrix(c(3,3,2,6),ncol=2,byrow=T)
leafnode7<-matrix(c(1,4,7,1),ncol=2,byrow=T)
leafnode8<-matrix(c(37,13,17,24),ncol=2,byrow=T)
leafnode9<-matrix(c(4,17,0,7),ncol=2,byrow=T)
leafnode10<-matrix(c(24,3,7,6),ncol=2,byrow=T)
leafnode11<-matrix(c(1,1,22,1,7,0,0,5,1,3),ncol=5,byrow=T)
leafnode12<-matrix(c(3,2,62,5,42,0,0,5,1,1),ncol=5,byrow=T)
leafnode13<-matrix(c(0,2,56,15,50,1,1,13,2,6),ncol=5,byrow=T)
source("C:\\Users\\User\\Desktop\\C4.5\\mypruning.R")
mypruning(leafnode1,1.64)
mypruning(leafnode2,1.64)
mypruning(leafnode3,1.64)
mypruning(leafnode4,1.64)
mypruning(leafnode5,1.64)
mypruning(leafnode6,1.64)
mypruning(leafnode7,1.64)
mypruning(leafnode8,1.64)
mypruning(leafnode9,1.64)
mypruning(leafnode10,1.64)
mypruning(leafnode11,1.64)
mypruning(leafnode12,1.64)
mypruning(leafnode13,1.64)
leafnode14<-matrix(c(7,32,0,9),ncol=2,byrow=T)
leafnode15<-matrix(c(123,114,23,7),ncol=2,byrow=T)
mypruning(leafnode14,1.64)
mypruning(leafnode15,1.64)

#testing
source("C:\\Users\\User\\Desktop\\C4.5\\myclassification.R")
myclassification(test_data)

#accuracy rate
x<-myclassification(test_data)
y<-test_data[,4]
sum(x==y)/nrow(test_data)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mystratifiedsampling<-function(dataset,n_sample){
#for sampling training and test data
#dataset=original dataset
#n_sample=number of samples in the training data

class_1<-c()
class_2<-c()
n_dataset<-nrow(dataset)

#separate class 1 and 2
for(i in 1:n_dataset){
	if(dataset$Class[i]==1) class_1<-rbind(class_1,dataset[i,])
	else class_2<-rbind(class_2,dataset[i,])
}

#class 1: sampling and separate train and test data
s<-sample(nrow(class_1),round(0.7*n_sample))
class_1_train<-class_1[s,]
class_1_test<-class_1[-s,]

#class 2: sampling and separate train and test data
t<-sample(nrow(class_2),round(0.3*n_sample))
class_2_train<-class_2[t,]
class_2_test<-class_2[-t,]

#combine class 1 and 2 for train and test data
train_data<-rbind(class_1_train,class_2_train)
test_data<-rbind(class_1_test,class_2_test)

#output the two data sets in csv
write.csv(train_data,file="train_data.csv")
write.csv(test_data,file="test_data.csv")
}
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myC45<-function(dataset){
#C4.5 decision tree modelling
#dataset=training data

#split the dataset based on the feature chosen in following algorithm
#and also split number(for numerical attribute)
mynodesplit<-function(dataset,index,split_number){
	n_data<-length(dataset[,n_column])	
	if(is.factor(dataset[,index])) new_dataset<-split(dataset[,-index],dataset[,index],drop=TRUE)
	else{
		indicator<-c() #for indicating if the numerical value <= split number
				   #1 indicates yes; 2 indicates no
		for(i in 1:n_data){
			if(dataset[i,index]<=split_number) indicator<-c(indicator,1)
			else indicator<-c(indicator,2)
		}
		new_dataset<-split(dataset[,-index],indicator,drop=TRUE) #split according to the indicators
	}
	new_dataset
}

n_column<-ncol(dataset)
if(is.null(n_column)){ #1st stopping criterion: check if there is no attribute for further splitting
	n_class_1<-sum(dataset==1)
	n_class_2<-sum(dataset==2)
	#if there is no more attribute, the following sentence will be printed 
	cat(sprintf("This is a leaf node as no further attribute: %s of class 1 and %s of class 2.\n", n_class_1, n_class_2))
}
else{ 
	n_data<-length(dataset[,n_column])
	n_class_1<-sum(dataset[,n_column]==1) 
	n_class_2<-sum(dataset[,n_column]==2)
	log_entrophy<-c(log2(n_class_1/n_data),log2(n_class_2/n_data))
	#if either n_class_1 or n_class_2 is 0, the next step will change log2 to 0 
	log_entrophy[is.infinite(log_entrophy)]<-0
	#compute the entrophy of the parent nodes
	entrophy_node<--(n_class_1/n_data)*log_entrophy[1]-(n_class_2/n_data)*log_entrophy[2] 

	n_features<-ncol(dataset[,-n_column,drop=FALSE]) #drop=FALSE prevents the last attribute being changed to an array
	
	gain_split_vector<-c() #initiated to store the info gain for each attribute
	gain_split_number_vector<-c() #initiated to store the split number for numerical attributes
	for(i in 1:n_features){ #loop for each feature
		if(is.factor(dataset[,i])){ #check if the feature is of factor type
			levels<-levels(dataset[,i])
			n_levels<-nlevels(dataset[,i])
			entrophy_i<-0 #initiated to store the entrophy for each child node
			for(j in 1:n_levels){ #loop for each level of the looping attribute
				n_j<-sum(dataset[,i]==levels[j]) #count no. of datapoints with the looping level
				if(n_j>0){ 
					n_j_1<-sum(dataset[,i]==levels[j] & dataset[,n_column]==1)
					n_j_2<-sum(dataset[,i]==levels[j] & dataset[,n_column]==2)
					log_entrophy_j<-c(log2(n_j_1/n_j),log2(n_j_2/n_j))
					#if either n_j_1 or n_j_2 is 0, the next step will change log2 to 0
					log_entrophy_j[is.infinite(log_entrophy_j)]<-0
					entrophy_j<--(n_j_1/n_j)*log_entrophy_j[1]-(n_j_2/n_j)*log_entrophy_j[2]
					#compute the accumulated entrophy of the child nodes
					entrophy_i<-entrophy_i+(n_j/n_data)*entrophy_j
				}
			}
			gain_split<-entrophy_node-entrophy_i #compute the information gain
			gain_split_vector<-c(gain_split_vector,gain_split) #store info gain together with other attributes 
			gain_split_number_vector<-c(gain_split_number_vector,0) #to retrace the split number more easily later,
												  #split number is put as 0 for categorical attributes
		}
	
		else{
			sort_numeric<-sort(unique(dataset[,i]))
			n_sort_numeric<-length(sort_numeric)
			if(n_sort_numeric==1){ #if there is only one numerical value, info gain=0
				gain_split<-0
				gain_split_vector<-c(gain_split_vector,gain_split)
				gain_split_number_vector<-c(gain_split_number_vector,sort_numeric)
			}
			else{
				gain_split_j<-c()
				for(j in 1:(n_sort_numeric-1)){
					n_less_j<-sum(dataset[,i]<=sort_numeric[j])
					n_more_j<-n_data-n_less_j
					n_less_1<-sum(dataset[,i]<=sort_numeric[j] & dataset[,n_column]==1) 
					n_less_2<-n_less_j-n_less_1
					n_more_1<-n_class_1-n_less_1
					n_more_2<-n_class_2-n_less_2
					log_entrophy_i<-c(log2(n_less_1/n_less_j),log2(n_less_2/n_less_j),log2(n_more_1/n_more_j),log2(n_more_2/n_more_j))
					#if any value is 0, the next step will change log2 to 0
					log_entrophy_i[is.infinite(log_entrophy_i)]<-0
					entrophy_less<--(n_less_1/n_less_j)*log_entrophy_i[1]-(n_less_2/n_less_j)*log_entrophy_i[2]
					entrophy_more<--(n_more_1/n_more_j)*log_entrophy_i[3]-(n_more_2/n_more_j)*log_entrophy_i[4]
					#compute the accumulated entrophy of the child nodes
					entrophy_i<-((n_less_j/n_data)*entrophy_less+(n_more_j/n_data)*entrophy_more)
					#compute the info gain for the looping numerical value
					gain_split_j<-c(gain_split_j,(entrophy_node-entrophy_i))
				}		
				#compute the max info gain among the numerical values
				gain_split<-max(gain_split_j)
				gain_split_vector<-c(gain_split_vector,gain_split)
				gain_split_number_vector<-c(gain_split_number_vector,(sort_numeric[which(gain_split_j==gain_split)])[1]) #when there is more than one, the first one is chosen
			}
		}
	}
	gain_split_max<-max(gain_split_vector)
	gain_split_max_feature<-which(gain_split_vector==gain_split_max)[1] #if there is more than one, then the first attribute is chosen
	gain_split_number<-gain_split_number_vector[gain_split_max_feature]
	#store the name of the chosen attribute
	feature_name<-colnames(dataset)[gain_split_max_feature]
	
	if(entrophy_node==0){ #2nd stopping criterion: check if the entrophy of parent node is 0
		if(n_class_1==0) cat(sprintf("This is a leaf node with %s of class 2.\n", n_class_2))
		if(n_class_2==0) cat(sprintf("This is a leaf node with %s of class 1.\n", n_class_1))
	}
	#3rd stopping criterion: check if the max info gain among the attributes is 0
	else if(gain_split_max==0) cat(sprintf("This is a leaf node as max gain split is 0: %s of class 1 and %s of class 2.\n", n_class_1, n_class_2))
	
	#if the stopping criteria are not met, algorithm continues
	else{ 
		#output the descripions of chosen attribute
		if(is.factor(dataset[,gain_split_max_feature])){
			cat(sprintf("The splitting feature is %s.\n",feature_name))
			cat("The levels are: ",levels(droplevels(dataset[,gain_split_max_feature])),"\n",sep="\t")		
		}
		else
			cat(sprintf("The splitting feature is %s and split number is %s.\n",feature_name,gain_split_number))
		#split the data set based on the chosen attribute
		new_dataset<-mynodesplit(dataset,gain_split_max_feature,gain_split_number)
		length_new_dataset<-length(new_dataset)
		#recursive function: loop the whole algorithm for each subdataset again
		for(i in 1:length_new_dataset){
			x<-myC45(new_dataset[[i]])
		}
		
	}
}
}
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mypruning<-function(matrix,z){
#for pruning
#matrix stores the no. of class 1 and 2 of each child node
#z=1.96(95% CI) or 1.64(90% CI)

n_data<-sum(matrix)
parent_1_percentage<-sum(matrix[1,])/n_data
parent_2_percentage<-sum(matrix[2,])/n_data
#compute the upper error boundary for the parent node
if(parent_1_percentage>parent_2_percentage) parent_error<-parent_2_percentage+z*sqrt(((parent_2_percentage*(1-parent_2_percentage))/n_data))
else parent_error<-parent_1_percentage+z*sqrt(((parent_1_percentage*(1-parent_1_percentage))/n_data))

n_col<-ncol(matrix)
child_error_vector<-c() #initiated to store upper error boundaries of the child nodes
for(i in 1:n_col){
	n_data_i<-sum(matrix[,i])
	i_1_percentage<-matrix[1,i]/n_data_i
	i_2_percentage<-matrix[2,i]/n_data_i
	#compute the upper error boundary for each child node
	if(matrix[1,i]>matrix[2,i]) upper_error<-i_2_percentage+z*sqrt(((i_2_percentage*(1-i_2_percentage))/n_data_i))
	else upper_error<-i_1_percentage+z*sqrt(((i_1_percentage*(1-i_1_percentage))/n_data_i))
	#store the upper boundary for each child node
	child_error_vector<-c(child_error_vector,upper_error)
}
child_error<-mean(child_error_vector)

if(child_error>parent_error) print("Pruning is required.")
else print("No pruning is required.")
}
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myclassification<-function(dataset){
#test data classification
#dataset=test data

n_data<-nrow(dataset)
classification<-c()

for(i in 1:n_data){
	if(dataset[i,1]=="A11" & (dataset[i,3]=="A30" | dataset[i,3]=="A31"))
		classification<-c(classification,2)
	if(dataset[i,1]=="A11" & dataset[i,2]<=15 & dataset[i,3]=="A32")
		classification<-c(classification,1)
	if(dataset[i,1]=="A11" & dataset[i,2]>15 & dataset[i,3]=="A32")
		classification<-c(classification,2)
	if(dataset[i,1]=="A11" & dataset[i,2]<=18 & dataset[i,3]=="A33")
		classification<-c(classification,1)
	if(dataset[i,1]=="A11" & dataset[i,2]>18 & dataset[i,3]=="A33")
		classification<-c(classification,2)
	if(dataset[i,1]=="A11" & dataset[i,3]=="A34")
		classification<-c(classification,1)
	if(dataset[i,1]=="A12" & dataset[i,3]=="A30")
		classification<-c(classification,2)
	if(dataset[i,1]=="A12" & dataset[i,2]<=24 & dataset[i,3]=="A31")
		classification<-c(classification,2)
	if(dataset[i,1]=="A12" & dataset[i,2]>24 & dataset[i,3]=="A31")
		classification<-c(classification,1)
	if(dataset[i,1]=="A12" & dataset[i,2]<=18 & dataset[i,3]=="A32")
		classification<-c(classification,1)
	if(dataset[i,1]=="A12" & dataset[i,2]>18 & dataset[i,3]=="A32")
		classification<-c(classification,2)
	if(dataset[i,1]=="A12" & dataset[i,3]=="A33")
		classification<-c(classification,1)
	if(dataset[i,1]=="A12" & dataset[i,3]=="A34")
		classification<-c(classification,1)
	if(dataset[i,1]=="A13" & dataset[i,2]<=6)
		classification<-c(classification,1)
	if(dataset[i,1]=="A13" & dataset[i,2]>6)
		classification<-c(classification,1)
	if(dataset[i,1]=="A14")
		classification<-c(classification,1)
}
return(classification)
}













