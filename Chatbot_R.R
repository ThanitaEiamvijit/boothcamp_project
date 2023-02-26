# Homework R Programming
# Chatbot (rule-based)
# 1. 
# Covid check application

app_auth <- function() {
  print("Welcome to our Covid check application.")
  ask_help <- readline("Do you need any help (Yes/No) ? ")
  
  if(ask_help == "Yes"){
    print("Please login and have a diagnose.")
  } else if(ask_help == "No") {
    print("Thank you.")
    return(NULL)
  } else {
    print("Invalid input. Please answer with Yes or No.")
  }

  username <- readline("Username: ")
  password <- readline("Password: ")
  print(paste("Hello", username))
  print("Please fill in the following information.")
  
  age <- readline("How old are you? ")
  sex <- readline("Please identify your sex (m/f/d): ")
  covid_risk <- readline("Did you be in contact or stay close to the Covid patient in last 3 days (Yes/No)? ")
  ask_question <- readline("Do you have a fever and temperature over 38 degree celcius (Yes/No)? ")
  
  if(ask_question == "Yes"){
    print("You have a high risk to get the Covid, please see the doctor.")
  } else {
    print("You have no risk to get the covid. Stay safe!")
  }
}


# 2
# Order pizza
order_pizza <- function() {
  print("Welcome to crazy pizza world application. Let's explore our fantacy pizza!")
  name <- readline("What is your name? ")
  print(paste("Hi", name))
  
  # Question 1
  print("What would you like to order today?")
  print("Option: 1. Original cheeze pizza, 2. Seasonal seafood pizza")
  pizza_type <- readline("Please select the order option: ")
  
  # Question 2
  pizza_dough <- readline("Please select pizza dough (thick/thin): ")
  
  # Question 3
  add_question <- readline("Would you like anything else (Yes/No)?" )
  
  if(add_question == "Yes"){
    add_order <- readline("What would you like 1. Fries 2. Drink? ")
  } else {
    print("Next Process...")
  }
  
  # Question 5
  print("Please select your payment method: 1.Paypal 2.Credit Card 3.Cash")
  payment <- readline("Your payment method: ")
  
  # Question 
  confirm_order <- readline("Please confirm your order (Yes/No): ")
  
  if(confirm_order == "Yes"){
    print("Thank you for ordering with us. Enjoy your pizza!")
  } else {
    print("Hope you order with us again.")
  }
}

