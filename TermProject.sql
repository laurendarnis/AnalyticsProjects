DROP TABLE Customer;
DROP TABLE Customer_info;
DROP TABLE Customer_feedback;
DROP TABLE Customer_order;
DROP TABLE Points;
DROP TABLE Splits;
DROP TABLE Purchase;
DROP TABLE Restaurant_check;
DROP TABLE Restaurant;
DROP TABLE Restaurant_type;
DROP TABLE Restaurant_address;
DROP TABLE Table_type;
DROP TABLE Menu;
DROP TABLE Menu_item;

CREATE TABLE CUSTOMER (
customer_id DECIMAL(12) NOT NULL PRIMARY KEY,
purchase_id DECIMAL(12),
table_id DECIMAL(12),
customer_info_id DECIMAL(12));

CREATE TABLE CUSTOMER_INFO (
customer_info_id DECIMAL(12) NOT NULL PRIMARY KEY,
customer_first_name VARCHAR(255),
customer_last_name VARCHAR(255),
cellphone_number CHAR(15),
email_address VARCHAR(255),
date_registered DATE);

CREATE TABLE CUSTOMER_FEEDBACK (
feedback_id DECIMAL(12) NOT NULL PRIMARY KEY,
customer_id DECIMAL(12),
description VARCHAR(255),
rating DECIMAL(1));

CREATE TABLE POINTS (
points_id DECIMAL(12) NOT NULL PRIMARY KEY,
customer_id DECIMAL(12),
points_earned DECIMAL(12),
points_redeemed DECIMAL(12),
points_balance DECIMAL(12));

CREATE TABLE SPLITS (
splits_id DECIMAL(12) NOT NULL PRIMARY KEY,
purchase_id DECIMAL(12),
customer_id DECIMAL(12),
num_of_cust_split DECIMAL (3) NOT NULL,
tax_per_split DECIMAL(12,2),
tip_per_split DECIMAL(12,2),
split_portion DECIMAL(12,2));

CREATE TABLE PURCHASE (
purchase_id DECIMAL(12) NOT NULL PRIMARY KEY,
table_id DECIMAL(12),
date_purchased DATE);

CREATE TABLE RESTAURANT_CHECK(
check_id DECIMAL(12) NOT NULL PRIMARY KEY,
restaurant_id DECIMAL(12),
purchase_id DECIMAL(12),
order_id DECIMAL(12),
total_tax DECIMAL(12,2),
total_tip DECIMAL(12,2),
total_amount_due DECIMAL(12,2));

CREATE TABLE RESTAURANT (
restaurant_id DECIMAL(12) NOT NULL PRIMARY KEY,
restaurant_type_id DECIMAL(12),
restaurant_address_id DECIMAL(12),
table_id DECIMAL(12),
name VARCHAR(255),
rating_stars DECIMAL(1));

CREATE TABLE RESTAURANT_TYPE (
restaurant_type_id DECIMAL(12) NOT NULL PRIMARY KEY,
restaurant_type VARCHAR(255),
restaurant_description VARCHAR(255));

CREATE TABLE RESTAURANT_ADDRESS (
restaurant_address_id DECIMAL(12) NOT NULL PRIMARY KEY,
street_number DECIMAL(5),
street_address VARCHAR(255),
city VARCHAR(255),
state CHAR(2),
zipcode CHAR(5));

CREATE TABLE TABLE_TYPE(
table_id DECIMAL(12) NOT NULL PRIMARY KEY,
num_of_seats DECIMAL(3),
description VARCHAR (255));

CREATE TABLE CUSTOMER_ORDER(
order_id DECIMAL(12) NOT NULL PRIMARY KEY,
customer_id DECIMAL(12),
menu_item_id DECIMAL(12),
quantity_ordered DECIMAL(3));

CREATE TABLE MENU (
menu_id DECIMAL(12) NOT NULL PRIMARY KEY,
restaurant_id DECIMAL(12),
name VARCHAR(255));

CREATE TABLE MENU_ITEM (
menu_item_id DECIMAL(12) NOT NULL PRIMARY KEY,
menu_id DECIMAL(12),
description VARCHAR(255),
price DECIMAL(12,2));

ALTER TABLE CUSTOMER
ADD CONSTRAINT cust_purch_fk
FOREIGN KEY (purchase_id)
REFERENCES PURCHASE (purchase_id);

ALTER TABLE CUSTOMER
ADD CONSTRAINT cust_table_fk
FOREIGN KEY (table_id)
REFERENCES TABLE_TYPE (table_id);

ALTER TABLE CUSTOMER
ADD CONSTRAINT cust__custinfo_fk
FOREIGN KEY (customer_info_id)
REFERENCES CUSTOMER_INFO (customer_info_id);

ALTER TABLE CUSTOMER_FEEDBACK
ADD CONSTRAINT feedback_cust_fk
FOREIGN KEY (customer_id)
REFERENCES CUSTOMER (customer_id);

ALTER TABLE POINTS
ADD CONSTRAINT points_cust_fk
FOREIGN KEY (customer_id)
REFERENCES CUSTOMER (customer_id);

ALTER TABLE SPLITS
ADD CONSTRAINT splits_purch_fk
FOREIGN KEY (purchase_id)
REFERENCES PURCHASE (purchase_id);

ALTER TABLE SPLITS
ADD CONSTRAINT splits_cust_fk
FOREIGN KEY (customer_id)
REFERENCES CUSTOMER (customer_id);

ALTER TABLE PURCHASE
ADD CONSTRAINT purch_table_fk
FOREIGN KEY (table_id)
REFERENCES TABLE_TYPE (table_id);

ALTER TABLE RESTAURANT_CHECK
ADD CONSTRAINT rest_check_fk
FOREIGN KEY (restaurant_id)
REFERENCES RESTAURANT (restaurant_id);

ALTER TABLE RESTAURANT_CHECK
ADD CONSTRAINT check_purch_fk
FOREIGN KEY (purchase_id)
REFERENCES PURCHASE (purchase_id);

ALTER TABLE RESTAURANT_CHECK
ADD CONSTRAINT rest_order_fk
FOREIGN KEY (order_id)
REFERENCES CUSTOMER_ORDER (order_id);

ALTER TABLE RESTAURANT
ADD CONSTRAINT rest_table_fk
FOREIGN KEY (table_id)
REFERENCES TABLE_TYPE (table_id);

ALTER TABLE RESTAURANT
ADD CONSTRAINT resttype_rest_fk
FOREIGN KEY (restaurant_type_id)
REFERENCES RESTAURANT_TYPE (restaurant_type_id);

ALTER TABLE RESTAURANT
ADD CONSTRAINT rest_address_fk
FOREIGN KEY (restaurant_address_id)
REFERENCES RESTAURANT_ADDRESS (restaurant_address_id);

ALTER TABLE CUSTOMER_ORDER
ADD CONSTRAINT order_customer_fk
FOREIGN KEY(customer_id)
REFERENCES CUSTOMER (customer_id);

ALTER TABLE CUSTOMER_ORDER
ADD CONSTRAINT order_menuitem_fk
FOREIGN KEY(menu_item_id)
REFERENCES MENU_ITEM (menu_item_id);

ALTER TABLE MENU
ADD CONSTRAINT menu_restaccount_fk
FOREIGN KEY(restaurant_id)
REFERENCES Restaurant (restaurant_id);

ALTER TABLE MENU_ITEM
ADD CONSTRAINT menuitem_menu_fk
FOREIGN KEY(menu_id)
REFERENCES MENU (menu_id);

ALTER TABLE Customer_info 
DROP COLUMN date_registered;

ALTER TABLE Restaurant_address
DROP COLUMN state;

CREATE TABLE Address_state (
state_id DECIMAL(12) NOT NULL PRIMARY KEY,
restaurant_address_id DECIMAL(12),
state_code CHAR(2));

ALTER TABLE Address_state
ADD CONSTRAINT fk_stateadd_to_restadd
FOREIGN KEY (restaurant_address_id)
REFERENCES Restaurant_address (restaurant_address_id);

CREATE OR REPLACE PROCEDURE Add_Customer_Info ( --add information to customer account
	Customer_info_id_arg IN DECIMAL, 
	customer_first_name_arg IN VARCHAR, --the customer's first name
	customer_last_name_arg IN VARCHAR, --the customer's last name
	cellphone_number_arg IN CHAR, --the customer's cellphone number	
	email_address_arg IN VARCHAR) --the customer's email address
IS --required by syntax
BEGIN
	INSERT INTO Customer_Info 
    (customer_info_id, customer_first_name, customer_last_name, cellphone_number, email_address)
	VALUES
	(customer_info_id_arg, customer_first_name_arg, customer_last_name_arg, cellphone_number_arg, email_address_arg);
END;

BEGIN
	Add_Customer_Info(1, 'Lauren', 'Darnis', '8603358493', 'ldarnis@bu.edu');
END;
/

BEGIN
	Add_Customer_Info(2, 'Greg', 'Rose', '7813748592', 'grose@bu.edu');
END;
/

BEGIN
	Add_Customer_Info(3, 'Anthony', 'DiMaggio', '3369482748', 'adimmaggio@bu.edu');
END;
/

BEGIN
	Add_Customer_Info(4, 'Richard', 'Gallant', '9198864213', 'rgallant@bu.edu');
END;
/

BEGIN
	Add_Customer_Info(5, 'Jake', 'Movson', '7819906544', 'jmovson@bu.edu');
END;
/

BEGIN
	Add_Customer_Info(6, 'Sylvia', 'Fink', '7723339801', 'sfink@bu.edu');
END;
/

SELECT * FROM Customer_Info;

CREATE OR REPLACE PROCEDURE Add_Table_Type ( --create a table entity
	table_id_arg IN DECIMAL, --the table id primary key associated with the entity
	num_of_seats_arg IN DECIMAL, --the associated number of seats per table
	description_arg IN VARCHAR) --the associated table description
IS --required by syntax
BEGIN
	INSERT INTO Table_type
   	 (table_id, num_of_seats, description)
	VALUES
    	(table_id_arg, num_of_seats_arg, description_arg);
END;

BEGIN
	Add_Table_type(1, 4, 'Table of four');
END;
/

BEGIN
	Add_Table_type(2, 2, 'Table of two');
END;
/

BEGIN
	Add_Table_type(3, 6, 'Table of six');
END;
/

SELECT * FROM Table_type;

CREATE OR REPLACE PROCEDURE Add_Purchase ( --create an purchase entity
	purchase_id_arg IN DECIMAL, --the purchase ID associated with purchase
	table_id_arg IN DECIMAL, --the associated table with the purchase
	date_purchased_arg IN DATE) --the date the purchase was made
IS --required by syntax
BEGIN
	INSERT INTO Purchase
    	(purchase_id, table_id, date_purchased)
	VALUES
    	(purchase_id_arg, table_id_arg, date_purchased_arg);
END;

BEGIN
	Add_Purchase(1, 3, to_date('08-30-2018', 'MM-DD-YYYY'));
END;
/

SELECT * FROM Purchase;

CREATE OR REPLACE PROCEDURE Add_Customer ( --create an customer account
	Customer_id_arg IN DECIMAL, --the new customer ID, must be unused
	purchase_id_arg IN DECIMAL, --the associated purchase ID
	table_id_arg IN DECIMAL, --the associated table ID
	customer_info_id_arg IN DECIMAL) --the associated customer information ID
IS --required by syntax
BEGIN
	INSERT INTO Customer 
    (customer_id, purchase_id, table_id, customer_info_id)
	VALUES
    (customer_id_arg, purchase_id_arg, table_id_arg, customer_info_id_arg);
END;

BEGIN
	Add_Customer(1, 1, 3, 1);
END;
/

BEGIN
	Add_Customer(2, 1, 3, 2);
END;
/

BEGIN
	Add_Customer(3, 1, 3, 3);
END;
/

BEGIN
	Add_Customer(4, NULL, NULL, 4);
END;
/

BEGIN
	Add_Customer(5, NULL, NULL, 5);
END;
/

BEGIN
	Add_Customer(6, NULL, NULL, 6);
END;
/

SELECT * FROM Customer;

CREATE OR REPLACE PROCEDURE Add_Customer_Feedback ( --create an entity for a customer to leave feedback
	feedback_id_arg IN DECIMAL, --the feedback ID associated with the customer feedback
	customer_id_arg IN DECIMAL, --the associated customer ID
	rating_arg IN CHAR, --the associated rating
	description_arg IN VARCHAR) --the description of the feedback
IS --required by syntax
BEGIN
	INSERT INTO Customer_feedback
   	(feedback_id, customer_id, rating, description)
	VALUES
   	(feedback_id_arg, customer_id_arg, rating_arg, description_arg);
END;

BEGIN
	Add_Customer_Feedback(1, 2, 5, 'I would recommend this application to my friends and family');
END;
/

BEGIN
	Add_Customer_Feedback(2, 2, 5, 'The application makes my restaurant paying experience easier and stress-free');
END;
/

BEGIN
	Add_Customer_Feedback(3, 2, 3, 'The amount of restaurants that utilize this application is satisfactory');
END;
/

SELECT * FROM Customer_feedback;

CREATE OR REPLACE PROCEDURE Add_Points ( --create an entity for a customer to track and gain points for application use
	points_id_arg IN DECIMAL, --the points ID associated with the customer feedback
	customer_id_arg IN DECIMAL, --the associated customer ID
	points_earned_arg IN DECIMAL, --the points earned per customer 
	points_redeemed_arg IN DECIMAL, --the amount of points used
	points_balance_arg IN DECIMAL) --the amount of points the customer has currently
IS --required by syntax
BEGIN
	INSERT INTO Points
   	(points_id, customer_id, points_earned, points_redeemed, points_balance)
	VALUES
   	(points_id_arg, customer_id_arg, points_earned_arg, points_redeemed_arg, points_balance_arg);
END;

BEGIN
	Add_Points(1, 2, 3, 0, 3);
END;
/

BEGIN
	Add_Points(2, 3, 5, 0, 5);
END;
/

BEGIN
	Add_Points(3, 1, 3, 0, 2);
END;
/

SELECT * FROM POINTS;

CREATE OR REPLACE PROCEDURE Add_Restaurant_address ( --create an entity for the address of the restaurant
	restaurant_address_id_arg IN DECIMAL, --ID associated with the restaurant address
	street_number_arg IN DECIMAL, --street number associated with the address
	street_address_arg IN VARCHAR, --street address associated with the address
	city_arg IN VARCHAR, --city associated with the address
	zipcode_arg IN CHAR) --zipcode associated with the address
IS --required by syntax
BEGIN
	INSERT INTO Restaurant_address
   	(restaurant_address_id, street_number, street_address, city, zipcode)
	VALUES
   	(restaurant_address_id_arg, street_number_arg, street_address_arg, city_arg, zipcode_arg);
END;

BEGIN
	Add_Restaurant_address(3, 100, 'Pizza Way', 'Cambridge', '02139');
END;
/

BEGIN
	Add_Restaurant_address(2, 200, 'Lasagna Terrace', 'Cambridge', '02139');
END;
/

SELECT * FROM Restaurant_address;

CREATE OR REPLACE PROCEDURE Add_address_state ( --create an entity for the state for the address of the restaurant
	state_id_arg IN DECIMAL, --ID associated with the restaurant address
	restaurant_address_id_arg IN DECIMAL, --addressID FK associated with the address
	state_code_arg IN CHAR) --state associated with the address
IS --required by syntax
BEGIN
	INSERT INTO Address_state
   	(state_id, restaurant_address_id, state_code)
	VALUES
   	(state_id_arg, restaurant_address_id_arg, state_code_arg);
END;

BEGIN
	Add_address_state(21, 2, 'MA');
END;
/

BEGIN
	Add_address_state(20, NULL, 'MD');
END;
/

BEGIN
	Add_address_state(19, NULL, 'ME');
END;
/

BEGIN
	Add_address_state(18, NULL, 'LA');
END;
/

BEGIN
	Add_address_state(17, NULL, 'KY');
END;
/

BEGIN
	Add_address_state(16, NULL, 'KS');
END;
/

BEGIN
	Add_address_state(15, NULL, 'IA');
END;
/

BEGIN
	Add_address_state(14, NULL, 'IN');
END;
/

BEGIN
	Add_address_state(13, NULL, 'IL');
END;
/

BEGIN
	Add_address_state(12, NULL, 'ID');
END;
/

BEGIN
	Add_address_state(11, NULL, 'HI');
END;
/

BEGIN
	Add_address_state(10, NULL, 'GA');
END;
/

BEGIN
	Add_address_state(9, NULL, 'FL');
END;
/

BEGIN
	Add_address_state(8, NULL, 'DE');
END;
/

BEGIN
	Add_address_state(7, NULL, 'CT');
END;
/

BEGIN
	Add_address_state(6, NULL, 'CO');
END;
/

BEGIN
	Add_address_state(5, NULL, 'CA');
END;
/

BEGIN
	Add_address_state(4, NULL, 'AK');
END;
/

BEGIN
	Add_address_state(3, NULL, 'AZ');
END;
/

BEGIN
	Add_address_state(2, NULL, 'AS');
END;
/

BEGIN
	Add_address_state(1, NULL, 'AL');
END;
/

SELECT * FROM address_state;

CREATE OR REPLACE PROCEDURE Add_restaurant_type ( --create an entity for type of restaurant
	restaurant_type_id_arg IN DECIMAL, --ID associated with the restaurant address type
	restaurant_type_arg IN VARCHAR, --type of restaurant or cuisine
	restaurant_description_arg IN VARCHAR) --description by the restaurant for the cuisine or type of restaurant
IS --required by syntax
BEGIN
	INSERT INTO Restaurant_type
   	(restaurant_type_id, restaurant_type, restaurant_description)
	VALUES
   	(restaurant_type_id_arg, restaurant_type_arg, restaurant_description_arg);
END;

BEGIN
	Add_restaurant_type(500, 'Italian', 'Delicious modern Italian restaurant');
END;
/

SELECT * FROM Restaurant_type;

CREATE OR REPLACE PROCEDURE Add_restaurant ( --create an entity for the restaurant account
	restaurant_id_arg IN DECIMAL, --ID associated with the restaurant 
	table_id_arg IN DECIMAL, --table ID associated with restaurant
	restaurant_type_id_arg IN DECIMAL, -- restaurant type id associated with restaurant
	restaurant_address_id_arg IN DECIMAL,
	name IN VARCHAR,
	rating_stars IN CHAR)
IS --required by syntax
BEGIN
	INSERT INTO Restaurant
   	(restaurant_id, table_id, restaurant_type_id, restaurant_address_id, name, rating_stars)
	VALUES
   	(restaurant_id_arg, table_id_arg, restaurant_type_id_arg, restaurant_address_id_arg, name, rating_stars);
END;

BEGIN
	Add_restaurant(1, 3, 500, 3, 'Restaurant Italian', 4);
END;
/

SELECT * FROM Restaurant;

CREATE OR REPLACE PROCEDURE Add_menu ( --create an entity for the menu
	menu_id_arg IN DECIMAL, --ID associated with the menu 
	restaurant_id_arg IN DECIMAL, --menuto associate with restaurant
	name_arg IN VARCHAR) -- name of menu
IS --required by syntax
BEGIN
	INSERT INTO Menu
   	(menu_id, restaurant_id, name)
	VALUES
   	(menu_id_arg, restaurant_id_arg, name_arg);
END;

BEGIN
	Add_menu(1, 1,'Italian Menu');
END;
/

SELECT * FROM MENU;

CREATE OR REPLACE PROCEDURE Add_menu_item ( --create an entity for the menu items
	menu_item_id_arg IN DECIMAL, --ID associated with the menu item 
	menu_id_arg IN DECIMAL, --menu to associate with menu item
	description_arg IN VARCHAR, -- description of menu item
	price_arg IN DECIMAL) --price of menu item
IS --required by syntax
BEGIN
	INSERT INTO Menu_item
   	(menu_item_id, menu_id, description, price)
	VALUES
   	(menu_item_id_arg, menu_id_arg, description_arg, price_arg);
END;

BEGIN
	Add_menu_item(300, 1,'Four Cheese Pizza', 9.99);
END;
/

SELECT * FROM MENU_ITEM;

BEGIN
	Add_menu_item(301, 1,'Spaghetti and Meatballs', 12.99);
END;
/

BEGIN
	Add_menu_item(302, 1,'Ravioli', 15.99);
END;
/

BEGIN
	Add_menu_item(303, 1,'Chicken Parmesean', 17.99);
END;
/

SELECT * FROM MENU_ITEM;

SELECT * FROM CUSTOMER;

SELECT * FROM CUSTOMER_INFO;

SELECT * FROM Address_state;

CREATE INDEX Points_balance_Idx
ON Points(points_balance);

CREATE INDEX Rating_stars_Idx
ON Restaurant(rating_stars);

CREATE INDEX State_code_Idx
ON Address_state(state_code);

SELECT description, price
FROM Menu_item
WHERE price < 15;

SELECT Menu.name, 
    Restaurant.name, 
    restaurant_type.restaurant_description, 
    restaurant_address.city,
    address_state.state_code
FROM Restaurant
JOIN Restaurant_type ON Restaurant.restaurant_type_id = Restaurant_type.restaurant_type_id
JOIN Restaurant_address ON Restaurant.restaurant_address_id = Restaurant_address.restaurant_address_id
JOIN Menu ON Restaurant.restaurant_id = Menu.restaurant_id
JOIN Address_state ON Restaurant_address.restaurant_address_id = Address_state.restaurant_address_id;

SELECT Menu.name, 
    Restaurant.name, 
    restaurant_type.restaurant_description, 
    menu_item.description,
    menu_item.price,
    restaurant_address.city
FROM Restaurant
JOIN Restaurant_type ON Restaurant.restaurant_type_id = Restaurant_type.restaurant_type_id
JOIN Restaurant_address ON Restaurant.restaurant_address_id = Restaurant_address.restaurant_address_id
JOIN Menu ON Restaurant.restaurant_id = Menu.restaurant_id
JOIN Menu_item ON Menu.menu_id = menu_item.menu_id
WHERE menu_item.price > 15;
    
SELECT Customer_info.Customer_info_id,
    Customer_info.Customer_first_name,
    Customer_info.Customer_last_name
FROM Customer_info
JOIN Customer ON Customer_info.customer_info_id = customer.customer_info_id
JOIN Points ON Customer.customer_id = Points.points_id
WHERE Points_balance > 2;