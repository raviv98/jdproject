##### FORMATTING ####

click = read.csv("C:/Users/Vignesh/Documents/School/Senior Year/2nd Semester/Project/clicks.csv")
user = read.csv("C:/Users/Vignesh/Documents/School/Senior Year/2nd Semester/Project/users.csv")
order = read.csv("C:/Users/Vignesh/Documents/School/Senior Year/2nd Semester/Project/orders1.csv")
delivery = read.csv("C:/Users/Vignesh/Documents/School/Senior Year/2nd Semester/Project/delivery.csv")
sku = read.csv("C:/Users/Vignesh/Documents/School/Senior Year/2nd Semester/Project/skus.csv")

c = click
o = order
u = user
d = delivery
s = sku


nrow(order)
#nrow(user[user$user_level == '10',])
#nrow(user)

library(mfx)
library(plyr)
library(lubridate)
library(anytime)

u = u[!(u$user_level == '10'),]
u = u[!(u$purchase_power == '-1'),]
u = u[!(u$age == 'U'),]
u = u[!(u$gender == 'U'),]
summary(u)
nrow(u)
o = o[!(o$original_unit_price=='0'),]
d$type[d$type == '0'] = '2'

c$browsedProduct = c$sku_ID
o$boughtProduct = o$sku_ID
bb = merge(o[, c('boughtProduct','user_ID','order_ID')], c[, c('browsedProduct','user_ID','request_time')], by='user_ID')
bb$boughtProduct = as.character(bb$boughtProduct)
bb$browsedProduct = as.character(bb$browsedProduct)

bb$match <- ifelse(bb$boughtProduct==bb$browsedProduct,1,0)
k1 <- ddply(bb, c('user_ID','browsedProduct','order_ID','request_time'), summarize, browsedAndBought = max(match))
nrow(k1)
head(k1)

f2 = merge(k1, sku, by.x='browsedProduct', by.y='sku_ID')

f2 = f2[!(f2$attribute1 == '-'),]
f2 = f2[!(f2$attribute2 == '-'),]

write.csv(final3,"C:/Users/Vignesh/Documents/School/Senior Year/2nd Semester/Project/export2.csv",row.names = FALSE)


######## 1st Question: From the initial browse, how long does it take a product to reach a customer? ##########

final = merge(f2, d, by.x='order_ID', by.y = 'order_ID')
final$request_time = sub("(:[^:]+):.*", "\\1",final$request_time)
final$arr_time = sub("(:[^:]+):.*", "\\1",final$arr_time)
nrow(final)
final = merge(final, u, by.x='user_ID', by.y = 'user_ID')
final = final[!(final$city_level < 1),]
summary(final$purchase_power)




final2 = final
final2$test = as.POSIXlt(final2$request_time, tz = "EST", format = "%m/%d/%Y %H:%M")
final2$test2 = as.POSIXlt(final2$arr_time, tz = "EST", format = "%m/%d/%Y %H:%M")
final2$transport = (difftime(final2$test2, final2$test, units = "hours"))
final2$test3 = round(as.numeric(final2$transport),0)
final2 = final2[!(final2$test3 < 0),]
summary(final2$test3)
summary(final2)
final2 = final2[,c(1,2,5,6,8,9,17,19,20,23,24,25,29)]
final2$type.x[final2$type.x == '2'] = '0'
final2 = final2[!(final2$education < 0),]
final2 = final2[!(final2$user_level < 0),]
final2$city_level = as.numeric(ifelse(final2$city_level == '1', '5', ifelse(final2$city_level == '2', '4', ifelse(final2$city_level == '4',  '2', ifelse(final2$city_level == '5',  '1', '3')))))
summary(final2)


#plot(final2$city_level, final2$test3)
#head(final2)
final2$gender = ifelse(final2$gender == 'F', 0, final2$gender == 'M')

final2$logDif = round(log(final2$test3),0)
summary(final2$test3)
final2 = final2[(final2$logDif > 0),]
summary(final2$logDif)

poisson_reg = lm(final2$logDif ~ final2$city_level+final2$user_level ,data = final2[final2$browsedAndBought == 1,])
summary(poisson_reg)

#plot(final2$city_level,final2$test3, main = "Distribution of Travel Time", xlab = "City Level", ylab = "Delivery Time (Hours)")


############### 2nd Question: What factors impact the discount percentage offered to individuals? ############



o2 = ddply(order,c('user_ID'), numcolwise(sum))

o2$total_discount = o2$original_unit_price - o2$final_unit_price
o2$percentage = o2$total_discount / o2$original_unit_price
o2$typeDisc = ifelse(o2$direct_discount_per_unit>o2$quantity_discount_per_unit | o2$direct_discount_per_unit>o2$bundle_discount_per_unit | o2$direct_discount_per_unit>o2$coupon_discount_per_unit, 1, ifelse(o2$bundle_discount_per_unit>o2$direct_discount_per_unit | o2$bundle_discount_per_unit>o2$quantity_discount_per_unit| o2$bundle_discount_per_unit > o2$coupon_discount_per_unit, 2, ifelse(o2$quantity_discount_per_unit > o2$direct_discount_per_unit | o2$quantity_discount_per_unit > o2$bundle_discount_per_unit | o2$quantity_discount_per_unit > o2$coupon_discount_per_unit, 3, ifelse(o2$coupon_discount_per_unit>o2$direct_discount_per_unit | o2$coupon_discount_per_unit > o2$quantity_discount_per_unit | o2$coupon_discount_per_unit > o2$bundle_discount_per_unit, 4, 0))))
summary(o2$typeDisc)
head(o2)
o2 = o2[(o2$gift_item <1),]
o2 = o2[,c(1,13,14,15)]
final3 = merge(u, o2, by.x = 'user_ID', by.y = 'user_ID')
final3$city_level = as.numeric(ifelse(final3$city_level == '1', '5', ifelse(final3$city_level == '2', '4', ifelse(final3$city_level == '4',  '2', ifelse(final3$city_level == '5',  '1', '3')))))
final3$marital_status = ifelse(final3$marital_status == 'S', 0, 1)
final3$gender = ifelse(final3$gender == 'F', 0, 1)
final3= final3[(final3$user_level > 0),]
final3 = final3[(final3$education > 0),]
summary(final3)

new_reg = lm(final3$percentage ~ final3$city_level+final3$user_level+final3$purchase_power)
summary(new_reg)


#ge = hist(final3$total_discount)
#dens = density(final2$test3)
#plot(dens)
#plot(ge)

