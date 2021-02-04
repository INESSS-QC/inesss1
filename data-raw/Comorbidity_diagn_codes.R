library(usethis)
library(stringr)
library(rlist)


# Comorbidity_diagn_codes -------------------------------------------------

### Codes de diagnostiques CIM9 et CIM10.
### Charlson et Elixhauser confondus.
### Ces codes sont à utiliser dans le REGEX de SQL.
Comorbidity_diagn_codes <- list(

  # AIDS/HIV
  aids = list(
    CIM9 = c(paste0("0", 42:44)),
    CIM10 = c(paste0("B", 20:24))
  ),

  # Alcohol abuse
  alcohol = list(
    CIM9 = c(2652, 2911:2913, 2915:2919, 303, 3050, 3575, 4255,
             5353, 5710:5713, 980),
    CIM10 = c("F10", "E52", "G621", "I426",
              paste0("K", c(292, 700, 703, 709)),
              "T51", "Z502", "Z714", "Z721")
  ),

  # Blood loss anemina
  blane = list(
    CIM9 = c(28090),
    CIM10 = c("D500")
  ),

  # Cancer (without metastasis)
  canc = list(
    CIM9 = c(
      # Solid Tumor without metastasis
      140:172, 174, 175, 179:195,
      # Lymphoma
      200:202, 2030, 2386, 2733
    ),
    CIM10 = c(
      # Solid tumor without metastasis
      paste0("C0", 1:9), paste0("C", c(10:26, 30:34, 37:41, 43, 45:58, 60:76)),
      # Lymphoma
      paste0("C", c(81:85, 88, 900, 902, 96))
    )
  ),

  # Cardiac arrhythmias
  carit = list(
    CIM9 = c(4260, 42610, 42612, 42613, 4267, 4269:4274, 4276:4279, 7850, 99601, 99604,
             "V450", "V533"),
    CIM10 = c(paste0("I", c(441:443, 456, 459, 47:49)),
              "R000", "R001", "R008",
              "T821",
              "Z450", "Z950")
  ),

  # Cerebrovascular disease
  cevd = list(
    CIM9 = c(430:438),
    CIM10 = c("G45", "G46",
              paste0("I", 60:69))
  ),

  # Congestive heart failure
  chf = list(
    CIM9 = c(39891, 40201, 4021, 40291, 40401, 40403, 4041, 40491, 40493, 428),
    CIM10 = c("I099", paste0("I", c(110, 130, 132, 255, 420, 425:429, 43, 50)),
              "P290")
  ),

  # Coagulopathy
  coag = list(
    CIM9 = c(286, 2871, 2873:2875),
    CIM10 = c(paste0("D", c(65:68, 691, 693:696)))
  ),

  # Chronic pulmonary disease
  copd = list(
    CIM9 = c(490:505, 5064, 5081),
    CIM10 = c(paste0("I", c(278, 279)),
              paste0("J", c(40:47, 60:67, 684, 701, 703)))
  ),

  # Deficiency Anemia
  dane = list(
    CIM9 = c(28091:28099, 281, 2859),
    CIM10 = c(paste0("D", c(501, 508, 509, 51:53, 63, 649)))
  ),

  # Dementia
  dementia = list(
    CIM9 = c(290, 2941, 3310, 3312),
    CIM10 = c(paste0("F0", c(0:3, 51)),
              paste0("G", c(30, 311)))
  ),

  # Depression
  depre = list(
    CIM9 = c(2961, 2963, 3004, 309, 311),
    CIM10 = c(paste0("F", c(204, 313:315, 32:33, 341, 412, 432)))
  ),

  # Diabetes without complications
  diab = list(
    CIM9 = c(2500:2502),
    CIM10 = c(paste0("E", c(100:101, 109:111, 119, 130:131, 139:141, 149)))
  ),

  # Diabetes with complications
  diabwc = list(
    CIM9 = c(2503:2509),
    CIM10 = c(paste0("E", c(102:108, 112:118, 132:138, 142:148)))
  ),

  # Drug abuse
  drug = list(
    CIM9 = c(292, 304, 3052:3059,
             "V654"),
    CIM10 = c(paste0("F", c(11:16, 18:19)),
              "Z715", "Z722")
  ),

  # Fluid and electrolyte disorders
  fed = list(
    CIM9 = c(2536, 276),
    CIM10 = c(paste0("E", c(222, 86, 87)))
  ),

  # Hypertension
  hyp = list(
    CIM9 = c(401:405, 4372),
    CIM10 = c(paste0("I", c(10:13, 15, 674)))
  ),

  # Hypothyroidism
  hypothy = list(
    CIM9 = c(2409, 243, 244, 2461, 2468),
    CIM10 = c(paste0("E0", 0:3), "E890")
  ),

  # Liver disease
  ld = list(
    CIM9 = c(
      # Mild liver disease
      "0702", "0703", "0704", "0705", 571, 5733:5734, 5739,
      "V427",
      # Moderate or severe liver disease
      4560:4562, 5723, 5728
    ),
    CIM10 = c(
      # Mild liver disease
      "B18",
      paste0("K", c(700:703, 709, 713:717, 73:74, 754, 760:761, 763:764, 768:769)),
      "Z944",
      # Moderate or severe liver disease
      "I85", "I864", "I982",
      paste0("K", c(711, 721, 729, 765:766))
    )
  ),

  # Metastatic cancer
  metacanc = list(
    CIM9 = c(196:199),
    CIM10 = c(paste0("C", c(77:80)))
  ),

  # Myocardial Infarction
  mi = list(
    CIM9 = c(410, 412),
    CIM10 = c(paste0("I", c(21, 22, 252)))
  ),

  # Neurological disorders
  nd = list(
    CIM9 = c(3319, 3320, 3321, 3334:3335, 334:335, 3362, 340:341, 345, 3481, 3483,
             7803, 7843),
    CIM10 = c(paste0("G", c(10:13, 20:22, 254:255, 312, 318:319, 32, 35:37, 41:41,
                            931, 934)),
              "R470", "R56")
  ),

  # Obesity
  obes = list(
    CIM9 = c(2780),
    CIM10 = c("E66")
  ),

  # Paralysis
  para = list(
    CIM9 = c(3341, 342:343, 3440:3446, 3448:3449),
    CIM10 = c("G041", paste0("G", c(114, 80:83)))
  ),

  # Pulmonary circulation disorders
  pcd = list(
    CIM9 = c(4150:4151, 415, 4710, 4178:4179),
    CIM10 = c(paste0("I", c(26:27, 280, 288, 289)))
  ),

  # Psychose
  psycho = list(
    CIM9 = c(2938, 295, 297, 298, 2991),
    CIM10 = c(paste0("F", c(20, 22:25, 28:29, 302, 312, 315)))
  ),

  # Peripheral vascular disease
  pvd = list(
    CIM9 = c("093", 440:441, 4431:4439, 4471, 5571, 5579,
             "V434"),
    CIM10 = c("A520",
              paste0("I", c(70:72, 730:731, 738:739, 771, 790)),
              paste0("K", c(551, 558, 559)),
              paste0("Z", c(958, 959)))
  ),

  # Renal disease
  rend = list(
    CIM9 = c(4030, 4031, 4039, 4040, 40412, 4041, 4049, 4049,
             585, 586, 5880,
             "V420", "V451", "V56"),
    CIM10 = c(paste0("I", c(120, 131)),
              paste0("N", c(18, 19, 250)),
              paste0("Z", c(49, 940, 992)))
  ),

  # Rheumatoid arthritis/collaged vascular disease
  rheumd = list(
    CIM9 = c(1361, 446, 7010, 7100:7105, 7108:7109, 7112, 714, 7193, 720, 725, 7285),
    CIM10 = c("L900", "L940", "L941", "L943",
              paste0("M0", c(5:6, 8)), paste0("M", c(120, 123, 30:35, 45, 460, 461, 468, 469)))
  ),

  # Ulcer disease
  ud = list(
    CIM9 = c(531:534),
    CIM10 = c(paste0("K", 25:28))
  ),

  # Valvular disease
  valv = list(
    CIM9 = c(394:397, 424, 7463, 7466,
             "V422", "V433"),
    CIM10 = c(paste0("I0", c(5:8, 91, 98)), paste0("I", c(34:39)),
              paste0("Q", c(230:233, 238, 239, 952:954)))
  ),

  # Weight loss
  wloss = list(
    CIM9 = c(260:263, 7832, 7994),
    CIM10 = c(paste0("E", 40:46),
              "R634", "R64")
  )

)


# Charlson_diagn_codes ----------------------------------------------------

Charlson_diagn_codes <- list(
  # AIDS/HIV
  aids = Comorbidity_diagn_codes$aids,
  # Cancer (without metastasis)
  canc = Comorbidity_diagn_codes$canc,
  # Cerebrovascular disease
  cevd = Comorbidity_diagn_codes$cevd,
  # Congestive heart failure
  chf = Comorbidity_diagn_codes$chf,
  # Chronic pulmonary disease
  copd = Comorbidity_diagn_codes$copd,
  # Dementia
  dementia = Comorbidity_diagn_codes$dementia,
  # Diabetes without complications
  diab = Comorbidity_diagn_codes$diab,
  # Diabetes with complications
  diabwc = Comorbidity_diagn_codes$diabwc,
  # Liver disease
  ld = Comorbidity_diagn_codes$ld,
  # Metastatic cancer
  metacanc = Comorbidity_diagn_codes$metacanc,
  # Myocardial Infarction
  mi = Comorbidity_diagn_codes$mi,
  # Paralysis
  para = Comorbidity_diagn_codes$para,
  # Renal disease
  rend = Comorbidity_diagn_codes$rend,
  # Rheumatoid arthritis/collaged vascular disease
  rheumd = list(
    CIM9 = c(4465, 7100:7104, 7140:7142, 7148, 725),
    CIM10 = c("M05", "M06", paste0("M", c(315, 32:34, 350:351, 353)))
  ),
  # Ulcer disease
  ud = Comorbidity_diagn_codes$ud,
  # Valvular disease
  valv = Comorbidity_diagn_codes$valv
)



# Elixhauser_diagn_codes --------------------------------------------------

Elixhauser_diagn_codes <- list(
  # AIDS/HIV
  aids = Comorbidity_diagn_codes$aids,
  # Alcohol abuse
  alcohol = Comorbidity_diagn_codes$alcohol,
  # Blood loss anemina
  blane = Comorbidity_diagn_codes$blane,
  # Cancer (without metastasis)
  canc = Comorbidity_diagn_codes$canc,
  # Cardiac arrhythmias
  carit = Comorbidity_diagn_codes$carit,
  # Congestive heart failure
  chf = Comorbidity_diagn_codes$chf,
  # Coagulopathy
  coag = Comorbidity_diagn_codes$coag,
  # Chronic pulmonary disease
  copd = Comorbidity_diagn_codes$copd,
  # Deficiency Anemia
  dane = Comorbidity_diagn_codes$dane,
  # Depression
  depre = Comorbidity_diagn_codes$depre,
  # Diabetes without complications
  diab = Comorbidity_diagn_codes$diab,
  # Diabetes with complications
  diabwc = Comorbidity_diagn_codes$diabwc,
  # Drug abuse
  drug = Comorbidity_diagn_codes$drug,
  # Fluid and electrolyte disorders
  fed = Comorbidity_diagn_codes$fed,
  # Hypertension
  hyp = Comorbidity_diagn_codes$hyp,
  # Hypothyroidism
  hypothy = Comorbidity_diagn_codes$hypothy,
  # Liver disease
  ld = Comorbidity_diagn_codes$ld,
  # Metastatic cancer
  metacanc = Comorbidity_diagn_codes$metacanc,
  # Neurological disorders
  nd = Comorbidity_diagn_codes$nd,
  # Obesity
  obes = Comorbidity_diagn_codes$obes,
  # Paralysis
  para = Comorbidity_diagn_codes$para,
  # Pulmonary circulation disorders
  pcd = Comorbidity_diagn_codes$pcd,
  # Psychose
  psycho = Comorbidity_diagn_codes$psycho,
  # Peripheral vascular disease
  pvd = Comorbidity_diagn_codes$pvd,
  # Renal disease
  rend = Comorbidity_diagn_codes$rend,
  # Rheumatoid arthritis/collaged vascular disease
  rheumd = Comorbidity_diagn_codes$rheumd,
  # Ulcer disease
  ud = list(
    CIM9 = c(5317, 5319, 5327, 5329, 5337, 5339, 5347, 5349),
    CIM10 = c(paste0("K", c(257, 259, 267, 269, 277, 279, 287, 289)))
  ),
  # Valvular disease
  valv = Comorbidity_diagn_codes$valv,
  # Weight loss
  wloss = Comorbidity_diagn_codes$wloss
)


# SQL regex ---------------------------------------------------------------

### Ajouter un '%' après chaque code pour pouvoir les utiliser dans du SQL regex
for (k in c("Comorbidity_diagn_codes", "Charlson_diagn_codes", "Elixhauser_diagn_codes")) {
  dt <- get(k)
  for (i in 1:length(dt)) {
    for (j in c("CIM9", "CIM10")) {
      dt[[i]][[j]] <- paste0(dt[[i]][[j]], "%")
    }
  }
  assign(k, dt)
}


# Save datas --------------------------------------------------------------

use_data(Comorbidity_diagn_codes,
         Charlson_diagn_codes,
         Elixhauser_diagn_codes,
         overwrite = TRUE)
