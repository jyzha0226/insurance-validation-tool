library(shiny)
library(shinythemes)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(highcharter)


############# life table ##################################
Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)
length(Lxs)

############ prerequisite data ######################################

############ group X #########################################
# Wholelife annuity due
epv_wl_annuity_due = function(age,ir){
  lx = Lxs[-(1:(age-17))]; if (age==17) lx = Lxs
  v = 1/(1+ir)
  epv = sum( (v^(0:(length(lx)-1)))*lx )/Lxs[age-17+1]
  return(epv)}

epv_wl_annuity_xy_due = function(agex,agey,ir){
  lx = Lxs[-(1:(agex-17))]; if (agex==17) lx = Lxs
  ly = Lxs[-(1:(agey-17))]; if (agey==17) ly = Lxs
  v = 1/(1+ir)
  epv = sum( (v^(0:(min(length(lx),length(ly))-1)))*lx*ly )/Lxs[agex-17+1]/Lxs[agey-17+1]
  return(epv)}


# Term annuity due
epv_term_annuity_due = function(age,ir,term){
  lx = Lxs[-(1:(age-17))]; if (age==17) lx = Lxs
  v = 1/(1+ir)
  epv = sum((v^(0:(term-1)))*lx[-((term+1):length(lx))])/Lxs[age-17+1]
  return(epv)}

#epv_term_annuity_due_x(20,0.04,10)




### assurance 
# Wholelife assurance
epv_wl_assurance = function(age,ir){
  d = ir/(1+ir)
  epv = 1 - d*epv_wl_annuity_due(age,ir)
  return(epv)}

#epv_wl_assurance_x(20,0.4)


# Wholelife assurance bar
epv_wl_assurance_bar = function(age,ir){
  epv = (1+ir)^(0.5)*epv_wl_assurance(age,ir)
  return(epv)}

#epv_wl_assurance_bar_x(20,0.4)


# Term assurance
epv_term_assurance = function(age,ir,term){
  dsf = (Lxs[age-17+1+term]/Lxs[age-17+1]) * ((1/(1 + ir))^term)
  epv = epv_wl_assurance(age,ir)-dsf*epv_wl_assurance((age+term),ir)
  return(epv)}

#epv_term_assurance_x(20,0.04,10)

# Term assurance bar
epv_term_assurance_bar = function(age,ir,term){
  dsf = (Lxs[age-17+1+term]/Lx[age-17+1]) * ((1/(1 + ir))^term)
  epv = epv_wl_assurance_bar(age,ir)-dsf*epv_wl_assurance_bar(age+term,ir)
  return(epv)}

#epv_term_assurance_bar_x(20,0.04,10)

# Pure endowment
epv_pure_endowment_assurance = function(age,ir,term){
  dsf = (Lx[age-17+1+term]/Lx[age-17+1]) * ((1/(1 + ir))^term)
  epv = dsf
  return(epv)}

#epv_pure_endowment_assurance_x(20,0.04,10)


# Endowment assurance
epv_endowment_assurance = function(age,ir,term){
  epv = epv_term_assurance(age,ir,term) + epv_pure_endowment_assurance(age,ir,term)
  return(epv)}

#epv_endowment_assurance_x(20,0.04,10)

# Endowment assurance bar
epv_endowment_assurance_bar_x = function(age,ir,term){
  epv = epv_term_assurance_bar(age,ir,term) + epv_pure_endowment_assurance(age,ir,term)
  return(epv)}

#epv_endowment_assurance_bar_x(20,0.04,10)





########################### Premium X ##############################

#################single premium X  ##############
#There are no premium expenses when the premium is single.
#G = EPV benefits (including bonuses) + EPV expenses
#I = G*p_i     (p_i ->  initial expense percentage  I ->initial expense )
#s Sum Assured 
#how ever after the transfer only the percentagfe rate for initial expense percentage will be considered 

# Single Premium


#term assurance single premium 
term_assurance_s = function(age,ir,p_i,p_c,s,term){
  B = s*epv_term_assurance(age,ir,term)
  c = B*p_c
  G = (B + c) /(1 - p_i)
  return(G)
}

#term_assurance_s_x(20,0.04,0.1,0.1,100,10)





#term assurance bar (immediately) single premium 
term_assurance_bar_s = function(age,ir,p_i,p_c,s,term){
  B = s*epv_term_assurance_bar(age,ir,term)
  c = B*p_c
  G = (B + c) /(1 - p_i)
  return(G)
}

#term_assurance_bar_s_x(20,0.04,0.1,0.1,100,10)



#pure endowment single premium  (immediately and at the end will be the same one)
pure_endowment = function(age,ir,p_i,p_c,s,term){
  B = s*epv_pure_endowment_assurance(age,ir,term)
  c = B*p_c
  G = (B + c) /(1 - p_i)
  return(G)
}

#pure_endowment_x(20,0.04,0.1,0.1,100,10)


#pure endowment single premium 
endowment_assurance_s = function(age,ir,p_i,p_c,s,term){
  B = s*epv_endowment_assurance(age,ir,term)
  c = B*p_c
  G = (B + c) /(1 - p_i)
  return(G)
}

#endowment_assurance_s_x(20,0.04,0.1,0.1,100,10)



#pure endowment bar(immediately) single premium 
endowment_assurance_bar_s = function(age,ir,p_i,p_c,s,term){
  B = s*epv_endowment_assurance_bar(age,ir,term)
  c = B*p_c
  G = (B + c) /(1 - p_i)
  return(G)
}

feature_xy = function(age,ir,p_i,p_c,s,term){
B = s*epv_wl_annuity_xy_due(age,ir)
c = B*p_c
G = (B + c) /(1 - p_i)
return(G)
}


#endowment_assurance_bar_s_x(20,0.04,0.1,0.1,100,10)





######### Level Premium X ########### 
#level premium will consider premium expense 
#G = EPV benefits (including bonuses) + EPV expenses  
#I = G*p_i     (p_i ->  initial expense percentage  I ->initial expense )
#s Sum Assured 
#claim expense c = p_c(claim expense percentage)*B 
#premium expense= p_p (premium expense percentage)* gross premium   


#level premium for term assurance 
term_assurance_lv = function(age,ir,p_i,p_p,p_c,s,term){
  B = s*epv_term_assurance(age,ir,term)
  c = B*p_c
  G = (B + c) /(epv_term_annuity_due(age,ir,term) - p_i - p_p*(epv_term_annuity_due(age,ir,term)-1))
  return(G)
}
#term_assurance_lv_x(20,0.04,0.1,0.1,0.1,100,10)

#level premium for term assurance bar(immediately)
term_assurance_bar_lv = function(age,ir,p_i,p_p,p_c,s,term){
  B = s*epv_term_assurance_bar(age,ir,term)
  c = B*p_c
  G = (B + c) /(epv_term_annuity_due(age,ir,term) - p_i - p_p*(epv_term_annuity_due(age,ir,term)-1))
  return(G)
}

#term_assurance_bar_lv_x(20,0.04,0.1,0.1,0.1,100,10)


#level premium for pure endowment(at the end of the year will be same as immediately)
pure_endowment_lv = function(age,ir,p_i,p_p,p_c,s,term){
  B = s*epv_pure_endowment_assurance(age,ir,term)
  c = B*p_c
  G = (B + c) /(epv_term_annuity_due(age,ir,term) - p_i - p_p*(epv_term_annuity_due(age,ir,term)-1))
  return(G)
}

#pure_endowment_lv_x(20,0.4,0.1,0.1,0.1,100,10)



#level premium for endowment assurance(at the end of the year)
endowment_assurance_lv = function(age,ir,p_i,p_p,p_c,s,term){
  B = s*epv_endowment_assurance(age,ir,term)
  c = B*p_c
  G = (B + c) /(epv_term_annuity_due(age,ir,term) - p_i - p_p*(epv_term_annuity_due(age,ir,term)-1))
  return(G)
}

#endowment_assurance_lv_x(20,0.04,0.1,0.1,0.1,100,10)



#level premium for endowment assurance bar(immediately)

endowment_assurance_bar_lv = function(age,ir,p_i,p_p,p_c,s,term){
  B = s*epv_endowment_assurance_bar(age,ir,term)
  c= B*p_c
  G = (B + c) /(epv_term_annuity_due(age,ir,term) - p_i - p_p*(epv_term_annuity_due(age,ir,term)-1))
  return(G)
}


#endowment_assurance_bar_lv_x(20,0.04,0.1,0.1,0.1,100,10)






ui = fluidPage(
  titlePanel("Insurance Benefit Valuation Tool"),
  sidebarPanel(
    conditionalPanel(
      condition = "input.Product != 'Caring for (y) after Death'",
      sliderInput(inputId="Age",label="Age", value = 30,min=20,max=100),
    ),
    conditionalPanel(
      condition = "input.Product == 'Caring for (x) after Death'",
      sliderInput(inputId="Agex",label="Age (x)", value = 30,min=20,max=90),
      sliderInput(inputId="Agey",label="Age (y)", value = 30,min=20,max=90),
    ),
    numericInput(inputId="IR",label="Interest Rate (in %)",value=4,min=0,max =50),
    numericInput(inputId="term",label="Benefit Term",value=5,min=0,max =100),
    radioButtons(inputId="Policyholder",label="Groups of policyholders", choices = list("Group (x)","Group (y)"), selected = "Group (x)"),
    radioButtons("Product",label="Insurance Benefit Type", choices = list("Pure endowment","Term assurance","Endowment assurance","Caring for (x) after Death"), selected = "Term assurance"),
    radioButtons("BenefitPayment",label="Insurance Benefit payment", choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
    radioButtons("PremiumPayment",label="Premium Payment", choices = list("Single Premium","Level Premium"), selected = "Single Premium"),
    numericInput(inputId="AssuredSum",label="Assured Sum",value=1,min=0,max=100000000),
    numericInput(inputId="InitialE",label="Initial Expense (in %)",value=4,min=0,max =50),
    numericInput(inputId="PremiumE",label="Premium Expense (in %)",value=4,min=0,max =50),
    numericInput(inputId="ClaimE",label="Claim Expense (in %)",value=4,min=0,max =50),
  ),

  mainPanel( 
    textOutput("OutputText"), 
    plotOutput("OutputFig")
    )
)

server = function(input,output) {
  output$OutputText = renderText( {

    if (input$Policyholder=="Group (x)") {age = input$Age +1 }
    if (input$Policyholder=="Group (y)") {age = input$Age -3 }
    agex = input$Agex +1 
    agey = input$Agey -3
    Interest = input$IR/100
    term = input$term
    p_i = input$InitialE/100
    p_p = input$PremiumE/100
    p_c = input$ClaimE/100
    s = input$AssuredSum
  
    if (input$PremiumPayment=="Single Premium" & input$BenefitPayment=="End of year of death" & input$Product=="Term assurance") {Premium = term_assurance_s(age,Interest,p_i,p_c,s,term)}

    if (input$Product=="Caring for (y) after Death") {Premium = epv_wl_annuity_xy_due(agex,agey,Interest)}  
    
    print(Premium)
  } )
  
  output$OutputFig <- renderPlot( {
    
    if (input$Policyholder=="Group (x)") {age = input$Age +1 }
    if (input$Policyholder=="Group (y)") {age = input$Age -3 }
    agex = input$Agex +1 
    agey = input$Agey -3
    Interest = 0:(15)/100
    term = input$term
    p_i = input$InitialE/100
    p_p = input$PremiumE/100
    p_c = input$ClaimE/100
    s = input$AssuredSum

    Premium=c()
    for (i in 1:length(Interest)){
    if (input$PremiumPayment=="Single Premium" & input$BenefitPayment=="End of year of death" & input$Product=="Term assurance") {Premium[i] = term_assurance_s(age,Interest[i],p_i,p_c,s,term)}
      if (input$Product=="Caring for (y) after Death") {Premium[i] = epv_wl_annuity_xy_due(agex,agey,Interest[i])}      
    }    
    plot(Interest,Premium,type="l")
  } )
  
}

shinyApp(ui = ui , server = server)




