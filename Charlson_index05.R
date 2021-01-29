
#/**********************************************************************/
#/* Programme: Comorbidities_Index */
#/* Auteur: Ahmed Ghachem */
#/* Date: 12 juin 2020 */
#/**********************************************************************/'

###################### Library ################
library(odbc)
library(dplyr)
library(data.table)
library(lubridate)
con <- dbConnect(odbc::odbc(), "PEI_PRD",uid = "ms069a", pwd = "Kong2017")
####################### ####################### ####################### #######################
###################### Chronic_pulmonary_disease #######################

Dx1<-c('490%','491%','492%','493%','494%','495%','496%','497%','498%','499%','500%','501%','502%','503%','504%','505%','5064%','5081%','5088%',
       'I278%','I279%','J40%','J41%','J42%','J43%','J44%','J45%','J46%','J47%','J60','J61%','J62%','J63%','J64%','J65%','J66%','J67%','J684%','J701%','J703%')
DX1<- as.character()
for(i in 1:length(Dx1)){
  DX1<-paste0(DX1,",",Dx1[i])
}
DX1<-sub('^.',"'", DX1)
DX1<-gsub(",", "','",DX1)
DX1<-paste0(DX1,"'")

var_names1<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Chronic_pulmonary_disease")
Condition1<- as.character()
Condition1<-paste0(Condition1,var_names1)


###################### Cardiac_arrhythmias #######################

Dx2<-c('4260%', '42610%','42612%', '42613%','4267%', '4269%', '4270%','4271%','4272%','4273%','4274%','4276%',
       '4277%','4278%','4279%','7850%', '99601%', '99604%','V450%', 'V533%',
       'I441%','I442%','I443%', 'I456%','I459%', 'I47%','I48%','I49%','R000%', 'R001%','R008%', 'T821%','Z450%', 'Z950%')
DX2<- as.character()
for(i in 1:length(Dx2)){
  DX2<-paste0(DX2,",",Dx2[i])
}
DX2<-sub('^.',"'", DX2)
DX2<-gsub(",", "','",DX2)
DX2<-paste0(DX2,"'")

var_names2<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Cardiac_arrhythmias")
Condition2<- as.character()
Condition2<-paste0(Condition2,var_names2)

###################### Deficiency_anemia #######################

Dx3<-c('28091%','28092%','28093%','28094%','28095%','28096%','28097%','28098%','28099%','281%','2859%',
       'D501%', 'D508%', 'D509%','D51%','D52%','D53%','D63%','D649%')
DX3<- as.character()
for(i in 1:length(Dx3)){
  DX3<-paste0(DX3,",",Dx3[i])
}
DX3<-sub('^.',"'", DX3)
DX3<-gsub(",", "','",DX3)
DX3<-paste0(DX3,"'")

var_names3<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Deficiency_anemia")
Condition3<- as.character()
Condition3<-paste0(Condition3,var_names3)


###################### Any_tumor_without_metastasis #######################

Dx4<-c('140%','141%','142%','143%','144%','145%','146%','147%','148%','149%',
       '150%','151%','152%','153%','154%','155%','156%','157%','158%','159%',
       '160%','161%','162%','163%','164%','165%','166%','167%','168%','169%',
       '170%','171%','172%','174%','175%','179%','180%','181%','182%','183%',
       '184%','185%','186%','187%','188%','189%','190%','191%','192%','193%',
       '194%','195%','200%','201%','202%','2030%','2386%','2733%',
       'C00%','C01%','C02%','C03%','C04%','C05%','C06%','C07%','C08%','C09%',
       'C10%','C11%','C12%','C13%','C14%','C15%','C16%','C17%','C18%','C19%',
       'C20%','C21%','C22%','C23%','C24%','C25%','C26%','C30%','C31%','C32%',
       'C33%','C34%','C37%','C38%','C39%','C40%','C41%','C43%','C45%','C46%',
       'C47%','C48%','C49%','C50%','C51%','C52%','C53%','C54%','C55%','C56%',
       'C57%','C58%','C60%','C61%','C62%','C63%','C64%','C65%','C66%','C67%',
       'C68%','C69%','C70%','C71%','C72%','C73%','C74%','C75%','C76%','C81%',
       'C82%','C83%','C84%','C85%','C88%','C900%','C902%','C96%')
DX4<- as.character()
for(i in 1:length(Dx4)){
  DX4<-paste0(DX4,",",Dx4[i])
}
DX4<-sub('^.',"'", DX4)
DX4<-gsub(",", "','",DX4)
DX4<-paste0(DX4,"'")

var_names4<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Any_tumor_without_metastasis")
Condition4<- as.character()
Condition4<-paste0(Condition4,var_names4)

###################### Hypothyroidism #######################

Dx5<-c('2409%', '243%', '244%','2461%', '2468%','E00%', 'E01%', 'E02%','E03%', 'E890%')
DX5<- as.character()
for(i in 1:length(Dx5)){
  DX5<-paste0(DX5,",",Dx5[i])
}
DX5<-sub('^.',"'", DX5)
DX5<-gsub(",", "','",DX5)
DX5<-paste0(DX5,"'")

var_names5<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Hypothyroidism")
Condition5<- as.character()
Condition5<-paste0(Condition5,var_names5)

###################### Renal_disease #######################

Dx6<-c('40301%','40311%','40391%','40402%','40403%','40412%','40413%','40492%',
       'N18%','N19%', 'N250%', 'Z49%','Z940%', 'Z992%')
DX6<- as.character()
for(i in 1:length(Dx6)){
  DX6<-paste0(DX6,",",Dx6[i])
}
DX6<-sub('^.',"'", DX6)
DX6<-gsub(",", "','",DX6)
DX6<-paste0(DX6,"'")

var_names6<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Renal_disease")
Condition6<- as.character()
Condition6<-paste0(Condition6,var_names6)

###################### Fluid_Electrolyte_disorders #######################

Dx7<-c('2536%','276%','E222%','E86%','E87%')
DX7<- as.character()
for(i in 1:length(Dx7)){
  DX7<-paste0(DX7,",",Dx7[i])
}
DX7<-sub('^.',"'", DX7)
DX7<-gsub(",", "','",DX7)
DX7<-paste0(DX7,"'")

var_names7<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Fluid_Electrolyte_disorders")
Condition7<- as.character()
Condition7<-paste0(Condition7,var_names7)


###################### Peripheral_vascular_disorders #######################

Dx8<-c('093%','440%','441%','4431%','4432%','4433%','4434%','4435%','4436%','4437%','4438%','4439%','4471%','5571%','5579%','V434%',
       'A520%','I70%','I71%','I72%','I730%','I731%','I738%','I739%','I771%','I790%','K551%','K558%','K559%','Z958%','Z959%')
DX8<- as.character()
for(i in 1:length(Dx8)){
  DX8<-paste0(DX8,",",Dx8[i])
}
DX8<-sub('^.',"'", DX8)
DX8<-gsub(",", "','",DX8)
DX8<-paste0(DX8,"'")

var_names8<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Peripheral_vascular_disorders")
Condition8<- as.character()
Condition8<-paste0(Condition8,var_names8)

###################### Myocardial_infarction #######################

Dx9<-c('410%','412%','I21%','I22%','I252%')
DX9<- as.character()
for(i in 1:length(Dx9)){
  DX9<-paste0(DX9,",",Dx9[i])
}
DX9<-sub('^.',"'", DX9)
DX9<-gsub(",", "','",DX9)
DX9<-paste0(DX9,"'")

var_names9<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Myocardial_infarction")
Condition9<- as.character()
Condition9<-paste0(Condition9,var_names9)

###################### Congestive_heart_failure #######################

Dx10<-c('39891%','40201%','40211%','40291%','40401%','40403%',
        '40411%','40413%','40491%','40493%','428%',
        'I099%','I110%','I130%','I132%','I255%','I420%','I425%',
        'I426%','I427%','I428%','I429%','I43%','P290%','I50%')
DX10<- as.character()
for(i in 1:length(Dx10)){
  DX10<-paste0(DX10,",",Dx10[i])
}
DX10<-sub('^.',"'", DX10)
DX10<-gsub(",", "','",DX10)
DX10<-paste0(DX10,"'")

var_names10<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Congestive_heart_failure")
Condition10<- as.character()
Condition10<-paste0(Condition10,var_names10)

###################### Obesity #######################

Dx11<-c('2780%','E66%')
DX11<- as.character()
for(i in 1:length(Dx11)){
  DX11<-paste0(DX11,",",Dx11[i])
}
DX11<-sub('^.',"'", DX11)
DX11<-gsub(",", "','",DX11)
DX11<-paste0(DX11,"'")

var_names11<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Obesity")
Condition11<- as.character()
Condition11<-paste0(Condition11,var_names11)

###################### Valvular_disease #######################

Dx12<-c('394%','395%','396%','397%','424%','7463%','7464%','7465%','7466%','V422%','V433%',
        'I05%','I06%','I07%','I08%','I091%','I098%','I34%','I35%','I36%','I37%','I38%','I39%',
        'Q230%','Q231%','Q232%','Q233%','Q238%','Q239%','Z952%','Z953%','Z954%')
DX12<- as.character()
for(i in 1:length(Dx12)){
  DX12<-paste0(DX12,",",Dx12[i])
}
DX12<-sub('^.',"'", DX12)
DX12<-gsub(",", "','",DX12)
DX12<-paste0(DX12,"'")

var_names12<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Valvular_disease")
Condition12<- as.character()
Condition12<-paste0(Condition12,var_names12)

###################### Metastatic_cancer #######################

Dx13<-c('196%','197%','198%','199%','C77%','C78%','C79%','C80%')
DX13<- as.character()
for(i in 1:length(Dx13)){
  DX13<-paste0(DX13,",",Dx13[i])
}
DX13<-sub('^.',"'", DX13)
DX13<-gsub(",", "','",DX13)
DX13<-paste0(DX13,"'")

var_names13<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Metastatic_cancer")
Condition13<- as.character()
Condition13<-paste0(Condition13,var_names13)

###################### Dementia #######################

Dx14<-c('290%','2941%','3310%','3312%','F00%','F01%','F02%','F03%','F051%','G30%','G311%')
DX14<- as.character()
for(i in 1:length(Dx14)){
  DX14<-paste0(DX14,",",Dx14[i])
}
DX14<-sub('^.',"'", DX14)
DX14<-gsub(",", "','",DX14)
DX14<-paste0(DX14,"'")

var_names14<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Dementia")
Condition14<- as.character()
Condition14<-paste0(Condition14,var_names14)

###################### Cerebrovascular_disease #######################

Dx15<-c('430%','431%','432%','433%','434%','435%','436%','437%','438%',
        'G45%','G46%','I60%','I61%','I62%','I63%','I64%','I65%','I66%','I67%','I68%','I69%')
DX15<- as.character()
for(i in 1:length(Dx15)){
  DX15<-paste0(DX15,",",Dx15[i])
}
DX15<-sub('^.',"'", DX15)
DX15<-gsub(",", "','",DX15)
DX15<-paste0(DX15,"'")

var_names15<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Cerebrovascular_disease")
Condition15<- as.character()
Condition15<-paste0(Condition15,var_names15)

###################### Depression #######################

Dx16<-c('2961%', '2963%', '3004%', '309%', '311%',
        'F204%', 'F313%', 'F314%','F315%','F32%', 'F33%', 'F341%','F412%', 'F432%')
DX16<- as.character()
for(i in 1:length(Dx16)){
  DX16<-paste0(DX16,",",Dx16[i])
}
DX16<-sub('^.',"'", DX16)
DX16<-gsub(",", "','",DX16)
DX16<-paste0(DX16,"'")

var_names16<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Depression")
Condition16<- as.character()
Condition16<-paste0(Condition16,var_names16)

###################### Neurological_disorders #######################

Dx17<-c('3319%', '3320%', '3321%','3334%', '3335%','334%','335%', '3362%','340%', '341%', '345%','3481%', '3483%', '7803%','7843%',
        'G10%','G11%','G12%','G13%','G20%', 'G21%','G22%','G254%', 'G255%', 'G312%','G318%',
        'G319%','G32%','G35%', 'G36%', 'G37%','G40%', 'G41%', 'G931%','G934%', 'R470%', 'R56%')
DX17<- as.character()
for(i in 1:length(Dx17)){
  DX17<-paste0(DX17,",",Dx17[i])
}
DX17<-sub('^.',"'", DX17)
DX17<-gsub(",", "','",DX17)
DX17<-paste0(DX17,"'")

var_names17<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Neurological_disorders")
Condition17<- as.character()
Condition17<-paste0(Condition17,var_names17)

###################### Alcohol_abuse #######################

Dx18<-c('2652%', '2911%','2912%','2913%','2915%','2916%','2917%','2918%','2919%','303%','3050%','3575%', '4255%','5353%', '5710%','5713%','980%',
        'F10%', 'E52%', 'G621%','I426%', 'K292%', 'K700%','K703%', 'K709%', 'T51%','Z502%', 'Z714%', 'Z721%')
DX18<- as.character()
for(i in 1:length(Dx18)){
  DX18<-paste0(DX18,",",Dx18[i])
}
DX18<-sub('^.',"'", DX18)
DX18<-gsub(",", "','",DX18)
DX18<-paste0(DX18,"'")

var_names18<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Alcohol_abuse")
Condition18<- as.character()
Condition18<-paste0(Condition18,var_names18)


###################### Liver_disease #######################

Dx19<-c('0702%','0703%','0704%','0705%','571%','5733%','5734%','5739%','V427%','4560%','4561%','4562%','5723%','5728%',
        'B18%','K700%','K701%','K702%','K703%','K709%','K713%','K714%','K715%','K716%','K717%',
        'K73%','K74%','K754%','K760%','K761%','K762%','K763%','K764%','K768%','K769%','Z944%',
        'I85%','I864%','I982%','K711%','K721%','K729%','K765%','K766%','K767%')
DX19<- as.character()
for(i in 1:length(Dx19)){
  DX19<-paste0(DX19,",",Dx19[i])
}
DX19<-sub('^.',"'", DX19)
DX19<-gsub(",", "','",DX19)
DX19<-paste0(DX19,"'")

var_names19<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Liver_disease")
Condition19<- as.character()
Condition19<-paste0(Condition19,var_names19)

###################### Psychoses #######################

Dx20<-c('2938%', '295%','297%', '298%','2991%',
        'F20%', 'F22%','F23%','F24%','F25%','F28%','F29%', 'F302%','F312%', 'F315%')
DX20<- as.character()
for(i in 1:length(Dx20)){
  DX20<-paste0(DX20,",",Dx20[i])
}
DX20<-sub('^.',"'", DX20)
DX20<-gsub(",", "','",DX20)
DX20<-paste0(DX20,"'")

var_names20<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Psychoses")
Condition20<- as.character()
Condition20<-paste0(Condition20,var_names20)

###################### Pulmonary_circulation_disorders #######################

Dx21<-c('4150%', '4151%', '416%','4170%', '4178%', '4179%',
        'I26%','I27%', 'I280%','I288%','I289%')
DX21<- as.character()
for(i in 1:length(Dx21)){
  DX21<-paste0(DX21,",",Dx21[i])
}
DX21<-sub('^.',"'", DX21)
DX21<-gsub(",", "','",DX21)
DX21<-paste0(DX21,"'")

var_names21<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Pulmonary_circulation_disorders")
Condition21<- as.character()
Condition21<-paste0(Condition21,var_names21)

###################### Rheumatoid_arthcollagen_vascularD #######################

Dx22<-c('1361%','446%','7010%','7100%','7101%','7102%','7103%','7104%','7105%','7108%',
        '7109%','7112%','714%','7193%','720%','725%','7285%','4465%',
        'L900%', 'L940%', 'L941%','L943%', 'M05%', 'M06%','M08%', 'M120%', 'M123%',
        'M30%', 'M31%','M32%','M33%','M34%', 'M35%', 'M45%','M460%',
        'M05%','M06%','M315%','M32%','M33%','M34%','M350%','M351%','M353%')
DX22<- as.character()
for(i in 1:length(Dx22)){
  DX22<-paste0(DX22,",",Dx22[i])
}
DX22<-sub('^.',"'", DX22)
DX22<-gsub(",", "','",DX22)
DX22<-paste0(DX22,"'")

var_names22<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Rheumatoid_arthcollagen_vascularD")
Condition22<- as.character()
Condition22<-paste0(Condition22,var_names22)

###################### Coagulopathy #######################

Dx23<-c('286%','2871%','2873%','2874%','2875%',
        'D65%','D66%','D67%','D68%','D691%','D693%','D694%','D695%','D696%')
DX23<- as.character()
for(i in 1:length(Dx23)){
  DX23<-paste0(DX23,",",Dx23[i])
}
DX23<-sub('^.',"'", DX23)
DX23<-gsub(",", "','",DX23)
DX23<-paste0(DX23,"'")

var_names23<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Coagulopathy")
Condition23<- as.character()
Condition23<-paste0(Condition23,var_names23)

###################### Weight_loss #######################

Dx24<-c('260%','261%','263%','7832%','7994%',
        'E40%','E41%','E42%','E43%','E44%','E45%','E46%','R634%','R64%')
DX24<- as.character()
for(i in 1:length(Dx24)){
  DX24<-paste0(DX24,",",Dx24[i])
}
DX24<-sub('^.',"'", DX24)
DX24<-gsub(",", "','",DX24)
DX24<-paste0(DX24,"'")

var_names24<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Weight_loss")
Condition24<- as.character()
Condition24<-paste0(Condition24,var_names24)

###################### Drug_abuse #######################

Dx25<-c('292%', '304%', '3052%','3053%','3054%','3055%','3056%','3057%','3058%','3059%','V654%',
        'F11%','F12%','F13%','F14%','F15%','F16%', 'F18%','F19%', 'Z715%', 'Z722%')
DX25<- as.character()
for(i in 1:length(Dx25)){
  DX25<-paste0(DX25,",",Dx25[i])
}
DX25<-sub('^.',"'", DX25)
DX25<-gsub(",", "','",DX25)
DX25<-paste0(DX25,"'")

var_names25<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Drug_abuse")
Condition25<- as.character()
Condition25<-paste0(Condition25,var_names25)

###################### Paralysis #######################

Dx26<-c('3341%','342%','343%','3440%','3441%','3442%','3443%','3444%','3445%','3446%','3448%','3449%',
        'G041%','G114%','G80%','G81%','G82%','G83%')
DX26<- as.character()
for(i in 1:length(Dx26)){
  DX26<-paste0(DX26,",",Dx26[i])
}
DX26<-sub('^.',"'", DX26)
DX26<-gsub(",", "','",DX26)
DX26<-paste0(DX26,"'")

var_names26<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Paralysis")
Condition26<- as.character()
Condition26<-paste0(Condition26,var_names26)

###################### Blood_loss_anemia #######################

Dx27<-c('28090%','D500%')
DX27<- as.character()
for(i in 1:length(Dx27)){
  DX27<-paste0(DX27,",",Dx27[i])
}
DX27<-sub('^.',"'", DX27)
DX27<-gsub(",", "','",DX27)
DX27<-paste0(DX27,"'")

var_names27<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Blood_loss_anemia")
Condition27<- as.character()
Condition27<-paste0(Condition27,var_names27)

###################### Ulcer_disease #######################

Dx28<-c('531%','532%','533%','534%',
        'K25%','K26%','K27%','K28%')
DX28<- as.character()
for(i in 1:length(Dx28)){
  DX28<-paste0(DX28,",",Dx28[i])
}
DX28<-sub('^.',"'", DX28)
DX28<-gsub(",", "','",DX28)
DX28<-paste0(DX28,"'")

var_names28<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Ulcer_disease")
Condition28<- as.character()
Condition28<-paste0(Condition28,var_names28)

###################### AIDS_HIV #######################

Dx29<-c('042%','044%',
        'B20%','B21%','B22%','B23%','B24%')
DX29<- as.character()
for(i in 1:length(Dx29)){
  DX29<-paste0(DX29,",",Dx29[i])
}
DX29<-sub('^.',"'", DX29)
DX29<-gsub(",", "','",DX29)
DX29<-paste0(DX29,"'")

var_names29<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","AIDS_HIV")
Condition29<- as.character()
Condition29<-paste0(Condition29,var_names29)

###################### Hypertension #######################

Dx30<-c('401%','402%','403%','404%','405%','4372%',
        'I10%','I11%','I12%','I13%','I15%','I674%')
DX30<- as.character()
for(i in 1:length(Dx30)){
  DX30<-paste0(DX30,",",Dx30[i])
}
DX30<-sub('^.',"'", DX30)
DX30<-gsub(",", "','",DX30)
DX30<-paste0(DX30,"'")

var_names30<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Hypertension")
Condition30<- as.character()
Condition30<-paste0(Condition30,var_names30)

###################### Diabetes_uncomplicated #######################

Dx31<-c('2500%','2501%','2502%',
        'E100%','E101%','E109%','E110%','E111%','E119%','E130%','E131%','E139%','E140%','E141%','E149%')
DX31<- as.character()
for(i in 1:length(Dx31)){
  DX31<-paste0(DX31,",",Dx31[i])
}
DX31<-sub('^.',"'", DX31)
DX31<-gsub(",", "','",DX31)
DX31<-paste0(DX31,"'")

var_names31<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Diabetes_uncomplicated")
Condition31<- as.character()
Condition31<-paste0(Condition31,var_names31)


###################### Diabetes_complicated #######################

Dx32<-c('25030%','2504%','2505%','2506%','2507%','2508%','2509%',
        'E102%','E103%','E104%','E105%','E106%','E107%','E108%','E112%','E113%','E114%','E115%','E116%','E117%','E118%',
        'E132%','E133%','E134%','E135%','E136%','E137%','E138%','E142%','E143%','E144%','E145%','E146%','E147%','E148%')
DX32<- as.character()
for(i in 1:length(Dx32)){
  DX32<-paste0(DX32,",",Dx32[i])
}
DX32<-sub('^.',"'", DX32)
DX32<-gsub(",", "','",DX32)
DX32<-paste0(DX32,"'")

var_names32<-c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG","Diabetes_complicated")
Condition32<- as.character()
Condition32<-paste0(Condition32,var_names32)

rm(Dx1,Dx2,Dx3,Dx4,Dx5,Dx6,Dx7,Dx8,Dx9,Dx10,Dx11,Dx12,Dx13,Dx14,Dx15,Dx16,Dx17,Dx18,Dx19,Dx20,Dx21,Dx22,Dx23,Dx24,Dx25,Dx26,Dx27,Dx28,Dx29,Dx30,Dx31,Dx32)

######################  ######################  ######################  ######################  ######################  ######################
###################### Evenement Obstétrique #######################
Dx_obst<-c('641%','642%','643%','644%','645%','646%','647%','648%','649%','650%','651%','652%','653%','654%','655%','656%','657%','658%','659%',
           '660%','661%','662%','663%','664%','665%','666%','667%','668%','669%','670%','671%','672%','673%','674%','675%','676%','V27%','O1%',
           'O21%','O22%','O23%','O24%','O25%','O26%','O27%','O28%','O29%','O30%','O31%','O32%','O33%','O34%','O35%','O36%','O37%','O38%','O39%',
           'O40%','O41%','O42%','O43%','O44%','O45%','O46%','O47%','O48%','O49%','O50%','O51%','O52%','O53%','O54%','O55%','O56%','O57%','O58%','O59%',
           'O60%','O61%','O62%','O63%','O64%','O65%','O66%','O67%','O68%','O69%','O70%','O71%','O72%','O73%','O74%','O75%','O76%','O77%','O78%','O79%',
           'O80%','O81%','O82%','O83%','O84%','O85%','O86%','O87%','O88%','O89%','O90%','O91%','O92%','O93%','O94%','O95%','O98%','O98%','Z37')
DX_obst<- as.character()
for(i in 1:length(Dx_obst)){
  DX_obst<-paste0(DX_obst,",",Dx_obst[i])
}
DX_obst<-sub('^.',"'", DX_obst)
DX_obst<-gsub(",", "','",DX_obst)
DX_obst<-paste0(DX_obst,"'")


####################### Fonctions ####################### #######################

GetFinalData<-function(nom_BD,DIAG,DIAG_obst,lookback=c(2,3,5),Vars_name){
  if (lookback==2){
    DT<-dbGetQuery(con, paste0("
                             SELECT b.*,
                             Add_Months(B.DAT_Index,-24) AS DAT_lookback,
                             c.DateDx, c.Source_DIAG
                             FROM DONNE_INESSS.",nom_BD," AS B
                             LEFT JOIN (
                             SELECT DISTINCT
                             A.SMOD_NO_INDIV_BEN_BANLS AS ID,
                             A.SMOD_DAT_SERV AS DateDx,
                             'SMOD' AS Source_DIAG
                             FROM Prod.I_SMOD_SERV_MD_CM  AS  A
                             WHERE (A.SMOD_COD_DIAGN_PRIMR like any (",DIAG,"))
                             AND A.SMOD_COD_STA_DECIS IN ('PAY')
                             UNION ALL
                             SELECT DISTINCT ID, DateDx, Source_DIAG
                             from (
                             SELECT DISTINCT
                             A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                             A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDx,
                             'MED' AS Source_DIAG
                             FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM AS A
                             WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,")
                             AND A.SHOP_TYP_DIAGN_SEJ_HOSP IN ('A','P','S')
                             UNION ALL
                             SELECT DISTINCT
                             A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                             A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDx,
                             'MED' AS Source_DIAG
                             FROM RES_SSS.V_SEJ_SERV_HOSP_CM  AS  A
                             WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,")
                             ) k
                             UNION ALL
                             SELECT DISTINCT
                             A.SURG_NO_INDIV_BEN_BANLS AS ID,
                             (Cast(A.SURG_DH_DEPAR_USAG_DURG AS DATE FORMAT 'yyyy-mm-dd')) AS DateDx,
                             'BDCU' AS Source_DIAG
                             FROM RES_SSS.V_EPISO_SOIN_DURG_CM   AS  A
                             WHERE (A.SURG_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,"))
                             )c
                             ON b.ID = c.ID
                             WHERE (c.DateDx BETWEEN DAT_lookback AND b.DAT_Index)
                             ORDER BY b.ID, DateDx
                             "))
    DT<-as.data.table(DT)
  }
  else if (lookback==3){
    DT<-dbGetQuery(con, paste0("
                             SELECT b.*,
                             Add_Months(B.DAT_Index,-36) AS DAT_lookback,
                             c.DateDx, c.Source_DIAG
                             FROM DONNE_INESSS.",nom_BD," AS B
                             LEFT JOIN (
                             SELECT DISTINCT
                             A.SMOD_NO_INDIV_BEN_BANLS AS ID,
                             A.SMOD_DAT_SERV AS DateDx,
                             'SMOD' AS Source_DIAG
                             FROM Prod.I_SMOD_SERV_MD_CM  AS  A
                             WHERE (A.SMOD_COD_DIAGN_PRIMR like any (",DIAG,"))
                             AND A.SMOD_COD_STA_DECIS IN ('PAY')
                             UNION ALL
                             SELECT DISTINCT ID, DateDx, Source_DIAG
                             from (
                             SELECT DISTINCT
                             A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                             A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDx,
                             'MED' AS Source_DIAG
                             FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM AS A
                             WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,")
                             AND A.SHOP_TYP_DIAGN_SEJ_HOSP IN ('A','P','S')
                             UNION ALL
                             SELECT DISTINCT
                             A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                             A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDx,
                             'MED' AS Source_DIAG
                             FROM RES_SSS.V_SEJ_SERV_HOSP_CM  AS  A
                             WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,")
                             ) k
                             UNION ALL
                             SELECT DISTINCT
                             A.SURG_NO_INDIV_BEN_BANLS AS ID,
                             (Cast(A.SURG_DH_DEPAR_USAG_DURG AS DATE FORMAT 'yyyy-mm-dd')) AS DateDx,
                             'BDCU' AS Source_DIAG
                             FROM RES_SSS.V_EPISO_SOIN_DURG_CM   AS  A
                             WHERE (A.SURG_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,"))
                             )c
                             ON b.ID = c.ID
                             WHERE (c.DateDx BETWEEN DAT_lookback AND b.DAT_Index)
                             ORDER BY b.ID, DateDx
                             "))
    DT<-as.data.table(DT)

  }

  else{
    DT<-dbGetQuery(con, paste0("
                             SELECT b.*,
                             Add_Months(B.DAT_Index,-60) AS DAT_lookback,
                             c.DateDx, c.Source_DIAG
                             FROM DONNE_INESSS.",nom_BD," AS B
                             LEFT JOIN (
                             SELECT DISTINCT
                             A.SMOD_NO_INDIV_BEN_BANLS AS ID,
                             A.SMOD_DAT_SERV AS DateDx,
                             'SMOD' AS Source_DIAG
                             FROM Prod.I_SMOD_SERV_MD_CM  AS  A
                             WHERE (A.SMOD_COD_DIAGN_PRIMR like any (",DIAG,"))
                             AND A.SMOD_COD_STA_DECIS IN ('PAY')
                             UNION ALL
                             SELECT DISTINCT ID, DateDx, Source_DIAG
                             from (
                             SELECT DISTINCT
                             A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                             A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDx,
                             'MED' AS Source_DIAG
                             FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM AS A
                             WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,")
                             AND A.SHOP_TYP_DIAGN_SEJ_HOSP IN ('A','P','S')
                             UNION ALL
                             SELECT DISTINCT
                             A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                             A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDx,
                             'MED' AS Source_DIAG
                             FROM RES_SSS.V_SEJ_SERV_HOSP_CM  AS  A
                             WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,")
                             ) k
                             UNION ALL
                             SELECT DISTINCT
                             A.SURG_NO_INDIV_BEN_BANLS AS ID,
                             (Cast(A.SURG_DH_DEPAR_USAG_DURG AS DATE FORMAT 'yyyy-mm-dd')) AS DateDx,
                             'BDCU' AS Source_DIAG
                             FROM RES_SSS.V_EPISO_SOIN_DURG_CM   AS  A
                             WHERE (A.SURG_COD_DIAGN_MDCAL_CLINQ like any (",DIAG,"))
                             )c
                             ON b.ID = c.ID
                             WHERE (c.DateDx BETWEEN DAT_lookback AND b.DAT_Index)
                             ORDER BY b.ID, DateDx
                             "))
    DT<-as.data.table(DT)
  }

  if (lookback==2){
    DT_obst<-dbGetQuery(con, paste0("
                                  SELECT b.*,
                                  Add_Months(B.DAT_Index,-24) AS DAT_lookback,
                                  c.DateDxObs, c.Source_DIAG_Obs
                                  FROM DONNE_INESSS.",nom_BD," AS B
                                  LEFT JOIN (
                                  SELECT DISTINCT
                                  A.SMOD_NO_INDIV_BEN_BANLS AS ID,
                                  A.SMOD_DAT_SERV AS DateDxObs,
                                  'SMOD' AS Source_DIAG_Obs
                                  FROM Prod.I_SMOD_SERV_MD_CM  AS  A
                                  WHERE (A.SMOD_COD_DIAGN_PRIMR like any (",DIAG_obst,"))
                                  AND A.SMOD_COD_STA_DECIS IN ('PAY')
                                  UNION ALL
                                  SELECT DISTINCT ID, DateDxObs, Source_DIAG_Obs
                                  from (
                                  SELECT DISTINCT
                                  A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                                  A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDxObs,
                                  'MED' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM AS A
                                  WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,")
                                  AND A.SHOP_TYP_DIAGN_SEJ_HOSP IN ('A','P','S')
                                  UNION ALL
                                  SELECT DISTINCT
                                  A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                                  A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDxObs,
                                  'MED' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_SEJ_SERV_HOSP_CM  AS  A
                                  WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,")
                                  ) k
                                  UNION ALL
                                  SELECT DISTINCT
                                  A.SURG_NO_INDIV_BEN_BANLS AS ID,
                                  (Cast(A.SURG_DH_DEPAR_USAG_DURG AS DATE FORMAT 'yyyy-mm-dd')) AS DateDxObs,
                                  'BDCU' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_EPISO_SOIN_DURG_CM   AS  A
                                  WHERE (A.SURG_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,"))
                                  )c
                                  ON b.ID = c.ID
                                  WHERE (c.DateDxObs BETWEEN DAT_lookback AND b.DAT_Index)
                                  ORDER BY b.ID, DateDxObs
                                  "))
    DT_obst<-as.data.table(DT_obst)
  }
  else if (lookback==3){
    DT_obst<-dbGetQuery(con, paste0("
                                  SELECT b.*,
                                  Add_Months(B.DAT_Index,-36) AS DAT_lookback,
                                  c.DateDxObs, c.Source_DIAG_Obs
                                  FROM DONNE_INESSS.",nom_BD," AS B
                                  LEFT JOIN (
                                  SELECT DISTINCT
                                  A.SMOD_NO_INDIV_BEN_BANLS AS ID,
                                  A.SMOD_DAT_SERV AS DateDxObs,
                                  'SMOD' AS Source_DIAG_Obs
                                  FROM Prod.I_SMOD_SERV_MD_CM  AS  A
                                  WHERE (A.SMOD_COD_DIAGN_PRIMR like any (",DIAG_obst,"))
                                  AND A.SMOD_COD_STA_DECIS IN ('PAY')
                                  UNION ALL
                                  SELECT DISTINCT ID, DateDxObs, Source_DIAG_Obs
                                  from (
                                  SELECT DISTINCT
                                  A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                                  A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDxObs,
                                  'MED' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM AS A
                                  WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,")
                                  AND A.SHOP_TYP_DIAGN_SEJ_HOSP IN ('A','P','S')
                                  UNION ALL
                                  SELECT DISTINCT
                                  A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                                  A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDxObs,
                                  'MED' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_SEJ_SERV_HOSP_CM  AS  A
                                  WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,")
                                  ) k
                                  UNION ALL
                                  SELECT DISTINCT
                                  A.SURG_NO_INDIV_BEN_BANLS AS ID,
                                  (Cast(A.SURG_DH_DEPAR_USAG_DURG AS DATE FORMAT 'yyyy-mm-dd')) AS DateDxObs,
                                  'BDCU' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_EPISO_SOIN_DURG_CM   AS  A
                                  WHERE (A.SURG_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,"))
                                  )c
                                  ON b.ID = c.ID
                                  WHERE (c.DateDxObs BETWEEN DAT_lookback AND b.DAT_Index)
                                  ORDER BY b.ID, DateDxObs
                                  "))
    DT_obst<-as.data.table(DT_obst)
  }

  else{
    DT_obst<-dbGetQuery(con, paste0("
                                  SELECT b.*,
                                  Add_Months(B.DAT_Index,-60) AS DAT_lookback,
                                  c.DateDxObs, c.Source_DIAG_Obs
                                  FROM DONNE_INESSS.",nom_BD," AS B
                                  LEFT JOIN (
                                  SELECT DISTINCT
                                  A.SMOD_NO_INDIV_BEN_BANLS AS ID,
                                  A.SMOD_DAT_SERV AS DateDxObs,
                                  'SMOD' AS Source_DIAG_Obs
                                  FROM Prod.I_SMOD_SERV_MD_CM  AS  A
                                  WHERE (A.SMOD_COD_DIAGN_PRIMR like any (",DIAG_obst,"))
                                  AND A.SMOD_COD_STA_DECIS IN ('PAY')
                                  UNION ALL
                                  SELECT DISTINCT ID, DateDxObs, Source_DIAG_Obs
                                  from (
                                  SELECT DISTINCT
                                  A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                                  A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDxObs,
                                  'MED' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM AS A
                                  WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,")
                                  AND A.SHOP_TYP_DIAGN_SEJ_HOSP IN ('A','P','S')
                                  UNION ALL
                                  SELECT DISTINCT
                                  A.SHOP_NO_INDIV_BEN_BANLS AS ID,
                                  A.SHOP_DAT_DEPAR_SEJ_HOSP AS DateDxObs,
                                  'MED' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_SEJ_SERV_HOSP_CM  AS  A
                                  WHERE A.SHOP_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,")
                                  ) k
                                  UNION ALL
                                  SELECT DISTINCT
                                  A.SURG_NO_INDIV_BEN_BANLS AS ID,
                                  (Cast(A.SURG_DH_DEPAR_USAG_DURG AS DATE FORMAT 'yyyy-mm-dd')) AS DateDxObs,
                                  'BDCU' AS Source_DIAG_Obs
                                  FROM RES_SSS.V_EPISO_SOIN_DURG_CM   AS  A
                                  WHERE (A.SURG_COD_DIAGN_MDCAL_CLINQ like any (",DIAG_obst,"))
                                  )c
                                  ON b.ID = c.ID
                                  WHERE (c.DateDxObs BETWEEN DAT_lookback AND b.DAT_Index)
                                  ORDER BY b.ID, DateDxObs
                                  "))
    DT_obst<-as.data.table(DT_obst)
  }
  Dx_All<-setDT(merge(DT,DT_obst,by="ID",all.x = T, allow.cartesian=TRUE))
  setorder(Dx_All,ID,DateDx)
  idx<-Dx_All[,.I[!is.na(DateDxObs) & (DateDxObs-DateDx<=180)& (DateDxObs-DateDx>=-120)]]
  Dx_All<-unique(Dx_All[!idx,.(ID,DateDx,Source_DIAG)])
  setorder(Dx_All,ID,DateDx)
  Dx_All<-as.data.table(Dx_All)

  dt <- as.data.table(Dx_All)
  if (!is.Date(dt$DateDx))
    dt[, DateDX := as_date(DateDx)]
  setkey(dt, ID, DateDx)

  dt[
    dt[, .I[.N > 1], .(ID)]$V1,
    diff := as.integer(DateDx - shift(DateDx)),
    .(ID)
    ][is.na(diff), diff := 0L]
  dt[
    , cum_diff := Reduce(function (x, y) {
      z <- x + y
      if (z > 730) return(0L) else return(z)
    }, x = diff, accumulate = TRUE),
    .(ID)
    ]

  dt[, `:=` (diff_conf = FALSE,
             cum_diff_conf = FALSE)]
  dt[
    Source_DIAG == "MED",
    `:=` (diff_conf = TRUE,
          cum_diff_conf = TRUE)
    ]
  dt[
    Source_DIAG %in% c("SMOD", "BDCU")
    & (30 <= diff & diff <= 730 | 30 <= cum_diff & cum_diff <= 730),
    `:=` (diff_conf = TRUE,
          cum_diff_conf = TRUE)
    ]
  dt <- dt[diff_conf == TRUE | cum_diff_conf == TRUE]
  if (nrow(dt)) {
    dt <- dt[dt[, .I[1], .(ID)]$V1]

    dt[
      diff_conf == TRUE, conf_njours := diff
      ][
        cum_diff_conf == TRUE, conf_njours := cum_diff
        ]

    idx <- dt[, .I[
      diff_conf == TRUE & cum_diff_conf == TRUE
      & diff_conf != cum_diff_conf
      ], .(ID)]$V1
    if (length(idx)) {
      dt[diff_conf < cum_diff_conf, conf_njours := cum_diff]
      dt[diff_conf > cum_diff_conf, conf_njours := diff]
    }

    dt[, DAT_Rep := DateDx - conf_njours]
    dt <- dt[
      , .(DAT_Rep,
          DAT_Conf = DateDx),
      .(ID)
      ]
    dt[, YEAR_DAT_Rep := year(DAT_Rep)]
  }

  dt<-as.data.table(dt)
  DT<-DT %>%
    rename(DAT_Conf=DateDx)
  dt<-left_join(dt,DT,by=c("ID","DAT_Conf")) %>%
    mutate(Maladie=1)

  colnames(dt)<-Vars_name

  return(dt<-as.data.table(dt))

}



Comorbidities_Index_DATIndex<-function(nom_BD,lookbacks=c(2,3,5)){
  if (lookbacks==2){
  Data1<-GetFinalData(nom_BD,DX1,DX_obst,2,var_names1)
  Data2<-GetFinalData(nom_BD,DX2,DX_obst,2,var_names2)
  Data3<-GetFinalData(nom_BD,DX3,DX_obst,2,var_names3)
  Data4<-GetFinalData(nom_BD,DX4,DX_obst,2,var_names4)
  Data5<-GetFinalData(nom_BD,DX5,DX_obst,2,var_names5)
  Data6<-GetFinalData(nom_BD,DX6,DX_obst,2,var_names6)
  Data7<-GetFinalData(nom_BD,DX7,DX_obst,2,var_names7)
  Data8<-GetFinalData(nom_BD,DX8,DX_obst,2,var_names8)
  Data9<-GetFinalData(nom_BD,DX9,DX_obst,2,var_names9)
  Data10<-GetFinalData(nom_BD,DX10,DX_obst,2,var_names10)
  Data11<-GetFinalData(nom_BD,DX11,DX_obst,2,var_names11)
  Data12<-GetFinalData(nom_BD,DX12,DX_obst,2,var_names12)
  Data13<-GetFinalData(nom_BD,DX13,DX_obst,2,var_names13)
  Data14<-GetFinalData(nom_BD,DX14,DX_obst,2,var_names14)
  Data15<-GetFinalData(nom_BD,DX15,DX_obst,2,var_names15)
  Data16<-GetFinalData(nom_BD,DX16,DX_obst,2,var_names16)
  Data17<-GetFinalData(nom_BD,DX17,DX_obst,2,var_names17)
  Data18<-GetFinalData(nom_BD,DX18,DX_obst,2,var_names18)
  Data19<-GetFinalData(nom_BD,DX19,DX_obst,2,var_names19)
  Data20<-GetFinalData(nom_BD,DX20,DX_obst,2,var_names20)
  Data21<-GetFinalData(nom_BD,DX21,DX_obst,2,var_names21)
  Data22<-GetFinalData(nom_BD,DX22,DX_obst,2,var_names22)
  Data23<-GetFinalData(nom_BD,DX23,DX_obst,2,var_names23)
  Data24<-GetFinalData(nom_BD,DX24,DX_obst,2,var_names24)
  Data25<-GetFinalData(nom_BD,DX25,DX_obst,2,var_names25)
  Data26<-GetFinalData(nom_BD,DX26,DX_obst,2,var_names26)
  Data27<-GetFinalData(nom_BD,DX27,DX_obst,2,var_names27)
  Data28<-GetFinalData(nom_BD,DX28,DX_obst,2,var_names28)
  Data29<-GetFinalData(nom_BD,DX29,DX_obst,2,var_names29)
  Data30<-GetFinalData(nom_BD,DX30,DX_obst,2,var_names30)
  Data31<-GetFinalData(nom_BD,DX31,DX_obst,2,var_names31)
  Data32<-GetFinalData(nom_BD,DX32,DX_obst,2,var_names32)

  DT<- Reduce(function(x, y) merge(x, y, by = c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG"),all=TRUE),
              list(Data1, Data2, Data3,Data4,Data5,Data6,Data7,Data8,Data9,Data10,
                   Data11,Data12,Data13,Data14,Data15,Data16,Data17,Data18,Data19,Data20,
                   Data21,Data22,Data23,Data24,Data25,Data26,Data27,Data28,Data29,Data30,Data31,Data32))
  DT<-setkey(DT,ID)

  }
  else if(lookbacks==3){
    Data1<-GetFinalData(nom_BD,DX1,DX_obst,3,var_names1)
    Data2<-GetFinalData(nom_BD,DX2,DX_obst,3,var_names2)
    Data3<-GetFinalData(nom_BD,DX3,DX_obst,3,var_names3)
    Data4<-GetFinalData(nom_BD,DX4,DX_obst,3,var_names4)
    Data5<-GetFinalData(nom_BD,DX5,DX_obst,3,var_names5)
    Data6<-GetFinalData(nom_BD,DX6,DX_obst,3,var_names6)
    Data7<-GetFinalData(nom_BD,DX7,DX_obst,3,var_names7)
    Data8<-GetFinalData(nom_BD,DX8,DX_obst,3,var_names8)
    Data9<-GetFinalData(nom_BD,DX9,DX_obst,3,var_names9)
    Data10<-GetFinalData(nom_BD,DX10,DX_obst,3,var_names10)
    Data11<-GetFinalData(nom_BD,DX11,DX_obst,3,var_names11)
    Data12<-GetFinalData(nom_BD,DX12,DX_obst,3,var_names12)
    Data13<-GetFinalData(nom_BD,DX13,DX_obst,3,var_names13)
    Data14<-GetFinalData(nom_BD,DX14,DX_obst,3,var_names14)
    Data15<-GetFinalData(nom_BD,DX15,DX_obst,3,var_names15)
    Data16<-GetFinalData(nom_BD,DX16,DX_obst,3,var_names16)
    Data17<-GetFinalData(nom_BD,DX17,DX_obst,3,var_names17)
    Data18<-GetFinalData(nom_BD,DX18,DX_obst,3,var_names18)
    Data19<-GetFinalData(nom_BD,DX19,DX_obst,3,var_names19)
    Data20<-GetFinalData(nom_BD,DX20,DX_obst,3,var_names20)
    Data21<-GetFinalData(nom_BD,DX21,DX_obst,3,var_names21)
    Data22<-GetFinalData(nom_BD,DX22,DX_obst,3,var_names22)
    Data23<-GetFinalData(nom_BD,DX23,DX_obst,3,var_names23)
    Data24<-GetFinalData(nom_BD,DX24,DX_obst,3,var_names24)
    Data25<-GetFinalData(nom_BD,DX25,DX_obst,3,var_names25)
    Data26<-GetFinalData(nom_BD,DX26,DX_obst,3,var_names26)
    Data27<-GetFinalData(nom_BD,DX27,DX_obst,3,var_names27)
    Data28<-GetFinalData(nom_BD,DX28,DX_obst,3,var_names28)
    Data29<-GetFinalData(nom_BD,DX29,DX_obst,3,var_names29)
    Data30<-GetFinalData(nom_BD,DX30,DX_obst,3,var_names30)
    Data31<-GetFinalData(nom_BD,DX31,DX_obst,3,var_names31)
    Data32<-GetFinalData(nom_BD,DX32,DX_obst,3,var_names32)

    DT<- Reduce(function(x, y) merge(x, y, by = c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG"),all=TRUE),
                list(Data1, Data2, Data3,Data4,Data5,Data6,Data7,Data8,Data9,Data10,
                     Data11,Data12,Data13,Data14,Data15,Data16,Data17,Data18,Data19,Data20,
                     Data21,Data22,Data23,Data24,Data25,Data26,Data27,Data28,Data29,Data30,Data31,Data32))
    DT<-setkey(DT,ID)
  }
  else{
    Data1<-GetFinalData(nom_BD,DX1,DX_obst,5,var_names1)
    Data2<-GetFinalData(nom_BD,DX2,DX_obst,5,var_names2)
    Data3<-GetFinalData(nom_BD,DX3,DX_obst,5,var_names3)
    Data4<-GetFinalData(nom_BD,DX4,DX_obst,5,var_names4)
    Data5<-GetFinalData(nom_BD,DX5,DX_obst,5,var_names5)
    Data6<-GetFinalData(nom_BD,DX6,DX_obst,5,var_names6)
    Data7<-GetFinalData(nom_BD,DX7,DX_obst,5,var_names7)
    Data8<-GetFinalData(nom_BD,DX8,DX_obst,5,var_names8)
    Data9<-GetFinalData(nom_BD,DX9,DX_obst,5,var_names9)
    Data10<-GetFinalData(nom_BD,DX10,DX_obst,5,var_names10)
    Data11<-GetFinalData(nom_BD,DX11,DX_obst,5,var_names11)
    Data12<-GetFinalData(nom_BD,DX12,DX_obst,5,var_names12)
    Data13<-GetFinalData(nom_BD,DX13,DX_obst,5,var_names13)
    Data14<-GetFinalData(nom_BD,DX14,DX_obst,5,var_names14)
    Data15<-GetFinalData(nom_BD,DX15,DX_obst,5,var_names15)
    Data16<-GetFinalData(nom_BD,DX16,DX_obst,5,var_names16)
    Data17<-GetFinalData(nom_BD,DX17,DX_obst,5,var_names17)
    Data18<-GetFinalData(nom_BD,DX18,DX_obst,5,var_names18)
    Data19<-GetFinalData(nom_BD,DX19,DX_obst,5,var_names19)
    Data20<-GetFinalData(nom_BD,DX20,DX_obst,5,var_names20)
    Data21<-GetFinalData(nom_BD,DX21,DX_obst,5,var_names21)
    Data22<-GetFinalData(nom_BD,DX22,DX_obst,5,var_names22)
    Data23<-GetFinalData(nom_BD,DX23,DX_obst,5,var_names23)
    Data24<-GetFinalData(nom_BD,DX24,DX_obst,5,var_names24)
    Data25<-GetFinalData(nom_BD,DX25,DX_obst,5,var_names25)
    Data26<-GetFinalData(nom_BD,DX26,DX_obst,5,var_names26)
    Data27<-GetFinalData(nom_BD,DX27,DX_obst,5,var_names27)
    Data28<-GetFinalData(nom_BD,DX28,DX_obst,5,var_names28)
    Data29<-GetFinalData(nom_BD,DX29,DX_obst,5,var_names29)
    Data30<-GetFinalData(nom_BD,DX30,DX_obst,5,var_names30)
    Data31<-GetFinalData(nom_BD,DX31,DX_obst,5,var_names31)
    Data32<-GetFinalData(nom_BD,DX32,DX_obst,5,var_names32)

    DT<- Reduce(function(x, y) merge(x, y, by = c("ID","DAT_Rep","DAT_Conf","YEAR_DAT_Rep","DAT_Index","DAT_lookback","Source_DIAG"),all=TRUE),
                list(Data1, Data2, Data3,Data4,Data5,Data6,Data7,Data8,Data9,Data10,
                     Data11,Data12,Data13,Data14,Data15,Data16,Data17,Data18,Data19,Data20,
                     Data21,Data22,Data23,Data24,Data25,Data26,Data27,Data28,Data29,Data30,Data31,Data32))
    DT<-setkey(DT,ID)
  }

  DT<-DT %>%
    mutate(Chronic_pulmonary_disease1=1*Chronic_pulmonary_disease,
           Diabetes_uncomplicated1=0*Diabetes_uncomplicated,
           Any_tumor_without_metastasis1=2*Any_tumor_without_metastasis,
           Renal_disease1=1*Renal_disease,
           Myocardial_infarction1=2*Myocardial_infarction,
           Congestive_heart_failure1=2*Congestive_heart_failure,
           Valvular_disease1=0*Valvular_disease,
           Metastatic_cancer1=5*Metastatic_cancer,
           Dementia1=2*Dementia,
           Cerebrovascular_disease1=2*Cerebrovascular_disease,
           Liver_disease1=2*Liver_disease,
           Diabetes_complicated1=0*Diabetes_complicated,
           Rheumatoid_arthcollagen_vascularD1=1*Rheumatoid_arthcollagen_vascularD,
           Paralysis1=1*Paralysis,
           Ulcer_disease1=0*Ulcer_disease,
           AIDS_HIV1=6*AIDS_HIV) %>%
    mutate(Charlson_index=rowSums(.[,c(
      "Chronic_pulmonary_disease1",
      "Diabetes_uncomplicated1",
      "Any_tumor_without_metastasis1",
      "Renal_disease1",
      "Myocardial_infarction1",
      "Congestive_heart_failure1",
      "Valvular_disease1",
      "Metastatic_cancer1",
      "Dementia1",
      "Cerebrovascular_disease1",
      "Liver_disease1",
      "Diabetes_complicated1",
      "Rheumatoid_arthcollagen_vascularD1",
      "Paralysis1",
      "Ulcer_disease1",
      "AIDS_HIV1"
    )], na.rm=TRUE)) %>%
    mutate(Hypertension1=0*Hypertension,
           Cardiac_arrhythmias1=0*Cardiac_arrhythmias,
           Deficiency_anemia1=0*Deficiency_anemia,
           Hypothyroidism1=0*Hypothyroidism,
           Fluid_Electrolyte_disorders1=2*Fluid_Electrolyte_disorders,
           Peripheral_vascular_disorders1=1*Peripheral_vascular_disorders,
           Obesity1=0*Obesity,
           Depression1=0*Depression,
           Neurological_disorders1=2*Neurological_disorders,
           Alcohol_abuse1=0*Alcohol_abuse,
           Psychoses1=1*Psychoses,
           Pulmonary_circulation_disorders1=2*Pulmonary_circulation_disorders,
           Coagulopathy1=2*Coagulopathy,
           Weight_loss1=2*Weight_loss,
           Drug_abuse1=0*Drug_abuse,
           Blood_loss_anemia1=0*Blood_loss_anemia) %>%
    mutate(Elixhauser_index=rowSums(.[,c(
      "Hypertension1",
      "Chronic_pulmonary_disease1",
      "Cardiac_arrhythmias1",
      "Diabetes_uncomplicated1",
      "Deficiency_anemia1",
      "Any_tumor_without_metastasis1",
      "Hypothyroidism1",
      "Renal_disease1",
      "Fluid_Electrolyte_disorders1",
      "Peripheral_vascular_disorders1",
      "Congestive_heart_failure1",
      "Obesity1","Valvular_disease1",
      "Metastatic_cancer1",
      "Depression1",
      "Neurological_disorders1",
      "Alcohol_abuse1",
      "Liver_disease1",
      "Psychoses1",
      "Pulmonary_circulation_disorders1",
      "Rheumatoid_arthcollagen_vascularD1",
      "Coagulopathy1",
      "Weight_loss1",
      "Drug_abuse1",
      "Paralysis1",
      "Blood_loss_anemia1",
      "Ulcer_disease1",
      "AIDS_HIV1",
      "Hypertension1",
      "Diabetes_complicated1"
    )], na.rm=TRUE)) %>%
    mutate(Combined_index=rowSums(.[,c(
      "Chronic_pulmonary_disease1",
      "Cardiac_arrhythmias1",
      "Deficiency_anemia1",
      "Any_tumor_without_metastasis1",
      "Hypothyroidism1",
      "Renal_disease1",
      "Fluid_Electrolyte_disorders1",
      "Peripheral_vascular_disorders1",
      "Myocardial_infarction1",
      "Congestive_heart_failure1",
      "Obesity1",
      "Valvular_disease1",
      "Metastatic_cancer1",
      "Dementia1",
      "Cerebrovascular_disease1",
      "Depression1",
      "Neurological_disorders1",
      "Alcohol_abuse1",
      "Liver_disease1",
      "Psychoses1",
      "Pulmonary_circulation_disorders1",
      "Rheumatoid_arthcollagen_vascularD1",
      "Coagulopathy1",
      "Weight_loss1",
      "Drug_abuse1",
      "Paralysis1",
      "Blood_loss_anemia1",
      "Ulcer_disease1",
      "AIDS_HIV1",
      "Hypertension1",
      "Diabetes_uncomplicated1",
      "Diabetes_complicated1"
    )], na.rm=TRUE)) %>%
    select(-c(Chronic_pulmonary_disease1,Cardiac_arrhythmias1,Deficiency_anemia1,Any_tumor_without_metastasis1,Hypothyroidism1,Renal_disease1,Fluid_Electrolyte_disorders1,
              Peripheral_vascular_disorders1,Myocardial_infarction1,Congestive_heart_failure1,Obesity1,Valvular_disease1,Metastatic_cancer1,Dementia1,Cerebrovascular_disease1,
              Depression1,Neurological_disorders1,Alcohol_abuse1,Liver_disease1,Psychoses1,Pulmonary_circulation_disorders1,Rheumatoid_arthcollagen_vascularD1, Coagulopathy1,
              Weight_loss1,Drug_abuse1,Paralysis1,Blood_loss_anemia1,Ulcer_disease1,AIDS_HIV1,Hypertension1,Diabetes_uncomplicated1,Diabetes_complicated1))

  return(DT<-as.data.table(DT))
}















