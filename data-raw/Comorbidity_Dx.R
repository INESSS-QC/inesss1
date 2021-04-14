library(usethis)
library(stringr)

# Combine_Dx_CCI_INSPQ18 -------------------------------------------------

### Codes de diagnostics CIM9 et CIM10.
### Charlson et Elixhauser confondus.
### Ces codes sont à utiliser dans le REGEX de SQL.
Combine_Dx_CCI_INSPQ18 <- list(

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


# Charlson_Dx_CCI_INSPQ18 ----------------------------------------------------

Charlson_Dx_CCI_INSPQ18 <- list(
  # AIDS/HIV
  aids = Combine_Dx_CCI_INSPQ18$aids,
  # Cancer (without metastasis)
  canc = Combine_Dx_CCI_INSPQ18$canc,
  # Cerebrovascular disease
  cevd = Combine_Dx_CCI_INSPQ18$cevd,
  # Congestive heart failure
  chf = Combine_Dx_CCI_INSPQ18$chf,
  # Chronic pulmonary disease
  copd = Combine_Dx_CCI_INSPQ18$copd,
  # Dementia
  dementia = Combine_Dx_CCI_INSPQ18$dementia,
  # Diabetes without complications
  diab = Combine_Dx_CCI_INSPQ18$diab,
  # Diabetes with complications
  diabwc = Combine_Dx_CCI_INSPQ18$diabwc,
  # Liver disease
  ld = Combine_Dx_CCI_INSPQ18$ld,
  # Metastatic cancer
  metacanc = Combine_Dx_CCI_INSPQ18$metacanc,
  # Myocardial Infarction
  mi = Combine_Dx_CCI_INSPQ18$mi,
  # Paralysis
  para = Combine_Dx_CCI_INSPQ18$para,
  # Renal disease
  rend = Combine_Dx_CCI_INSPQ18$rend,
  # Rheumatoid arthritis/collaged vascular disease
  rheumd = list(
    CIM9 = c(4465, 7100:7104, 7140:7142, 7148, 725),
    CIM10 = c("M05", "M06", paste0("M", c(315, 32:34, 350:351, 353)))
  ),
  # Ulcer disease
  ud = Combine_Dx_CCI_INSPQ18$ud,
  # Valvular disease
  valv = Combine_Dx_CCI_INSPQ18$valv
)



# Elixhauser_Dx_CCI_INSPQ18 --------------------------------------------------

Elixhauser_Dx_CCI_INSPQ18 <- list(
  # AIDS/HIV
  aids = Combine_Dx_CCI_INSPQ18$aids,
  # Alcohol abuse
  alcohol = Combine_Dx_CCI_INSPQ18$alcohol,
  # Blood loss anemina
  blane = Combine_Dx_CCI_INSPQ18$blane,
  # Cancer (without metastasis)
  canc = Combine_Dx_CCI_INSPQ18$canc,
  # Cardiac arrhythmias
  carit = Combine_Dx_CCI_INSPQ18$carit,
  # Congestive heart failure
  chf = Combine_Dx_CCI_INSPQ18$chf,
  # Coagulopathy
  coag = Combine_Dx_CCI_INSPQ18$coag,
  # Chronic pulmonary disease
  copd = Combine_Dx_CCI_INSPQ18$copd,
  # Deficiency Anemia
  dane = Combine_Dx_CCI_INSPQ18$dane,
  # Depression
  depre = Combine_Dx_CCI_INSPQ18$depre,
  # Diabetes without complications
  diab = Combine_Dx_CCI_INSPQ18$diab,
  # Diabetes with complications
  diabwc = Combine_Dx_CCI_INSPQ18$diabwc,
  # Drug abuse
  drug = Combine_Dx_CCI_INSPQ18$drug,
  # Fluid and electrolyte disorders
  fed = Combine_Dx_CCI_INSPQ18$fed,
  # Hypertension
  hyp = Combine_Dx_CCI_INSPQ18$hyp,
  # Hypothyroidism
  hypothy = Combine_Dx_CCI_INSPQ18$hypothy,
  # Liver disease
  ld = Combine_Dx_CCI_INSPQ18$ld,
  # Metastatic cancer
  metacanc = Combine_Dx_CCI_INSPQ18$metacanc,
  # Neurological disorders
  nd = Combine_Dx_CCI_INSPQ18$nd,
  # Obesity
  obes = Combine_Dx_CCI_INSPQ18$obes,
  # Paralysis
  para = Combine_Dx_CCI_INSPQ18$para,
  # Pulmonary circulation disorders
  pcd = Combine_Dx_CCI_INSPQ18$pcd,
  # Psychose
  psycho = Combine_Dx_CCI_INSPQ18$psycho,
  # Peripheral vascular disease
  pvd = Combine_Dx_CCI_INSPQ18$pvd,
  # Renal disease
  rend = Combine_Dx_CCI_INSPQ18$rend,
  # Rheumatoid arthritis/collaged vascular disease
  rheumd = Combine_Dx_CCI_INSPQ18$rheumd,
  # Ulcer disease
  ud = list(
    CIM9 = c(5317, 5319, 5327, 5329, 5337, 5339, 5347, 5349),
    CIM10 = c(paste0("K", c(257, 259, 267, 269, 277, 279, 287, 289)))
  ),
  # Valvular disease
  valv = Combine_Dx_CCI_INSPQ18$valv,
  # Weight loss
  wloss = Combine_Dx_CCI_INSPQ18$wloss
)


# Charlson_Dx_UManitoba16 -------------------------------------------------

Charlson_Dx_UManitoba16 <- list(
  # HIV/AIDS
  aids = list(
    CIM9 = c(paste0("0", 42:44)),
    CIM10 = c(paste0("B", c(20:22, 24)))
  ),
  # Cancer
  canc = list(
    CIM9 = c(140:172,
             174:195, # 174:194, 1950:1958,
             200:208,
             238 # 2386
             ),
    CIM10 = c(paste0("C0", c(0:9)),
              paste0("C", c(10:26, 30:34, 37:41, 43, 45:58, 60:76, 81:85, 88, 90:97)))
  ),
  # Congestive Heart Failure
  chf = list(
    CIM9 = c(398, # 39891,
             402, # 40201, 40211, 40291,
             40401, 40403, 40411, 40413, 40491, 40493,
             425, # 4254:4259,
             428),
    CIM10 = c(paste0("I", c("099", 110, 130, 132, 255, 420, 425:429, 43, 50)),
              "P290")
  ),
  # Chronic Pulmonary Disease
  cpd = list(
    CIM9 = c(416, # 4168, 4169,
             490:505, # 490:496, 500:505
             5064, 5081, 5088),
    CIM10 = c(paste0("I", c(278, 279)),
              paste0("J", c(40:47, 60:67, 684, 701, 703)))
  ),
  # Connective Tissue Disease - Rheumatic Disease
  ctdrd = list(
    CIM9 = c(446, # 4465,
             710, # 710:7104,
             714, # 7140:7142, 7148,
             725),
    CIM10 = c(paste0("M0", 5:6),
              paste0("M", c(315, 32:34, 351, 353, 360)))
  ),
  # Cerebrovascular Disease
  cvd = list(
    CIM9 = c(36234, 430:438),
    CIM10 = c(paste0("G", c(45, 46)),
              "H340",
              paste0("I", 60:69))
  ),
  # Dementia
  dementia = list(
    CIM9 = c(290,
             294, # 2941,
             331 #3312
             ),
    CIM10 = c(paste0("F0", c(1:3, 51)),
              paste0("G", c(30, 311)))
  ),
  # Diabetes with Chronic Complications
  diab = list(
    CIM9 = c(2504:2507),
    CIM10 = c(paste0("E", c(102:105, 107, 112:115, 117,
                            122:125, 127, 132:135, 137,
                            142:145, 147)))
  ),
  # Diabetes without Chronic Complications
  diabwc = list(
    CIM9 = c(250), # 2500:2503, 2508, 2509
    CIM10 = c(paste0("E", c(100, 101, 106, 108, 109, 110, 111, 116, 118, 119,
                            120, 121, 126, 128, 129, 130, 131, 136, 138, 139,
                            140, 141, 146, 148, 149)))
  ),
  # Liver disease - Mild
  ld1 = list(
    CIM9 = c("070", # paste0("0", c(7022, 7023, 7032, 7033, 7044, 7054, 706, 709)),
             570, 571,
             573, # 5733, 5734, 5738, 5739,
             "V427"),
    CIM10 = c("B18",
              paste0("K", c(700:703, 709, 713:715, 717, 73, 74,
                            760, 762:764, 768, 769)),
              "Z944")
  ),
  # Moderate or Severe Liver Disease
  ld2 = list(
    CIM9 = c(456, # 4560:4562,
             572 # 5722:5724, 5728),
            ),
    CIM10 = c(paste0("I", c(850:859, 864, 982)),
              paste0("K", c(704, 711, 721, 729, 765:767)))
  ),
  # Metastatic Carcinoma
  mc = list(
    CIM9 = c(196:199),
    CIM10 = c(paste0("C", 77:80))
  ),
  # Myocardial Infarction
  mi = list(
    CIM9 = c(410, 412),
    CIM10 = paste0("I", c(21, 22, 252))
  ),
  # Paraplegia and Hemiplegia
  ph = list(
    CIM9 = c(334, # 3341,
             342:344 # 342, 343, 3440:3446, 3449)
            ),
    CIM10 = c("G041", paste0("G", c(114, 801, 802, 81, 82, 830:834, 839)))
  ),
  # Peptic Ulcer Disease
  pud = list(
    CIM9 = c(531:534),
    CIM10 = c(paste0("K", 25:28))
  ),
  # Peripheral Vascular Disease
  pvd = list(
    CIM9 = c("0930",
             4373, 440, 441,
             443, # 4431:4439,
             447, # 4471,
             557, # 5571, 5579,
             "V433"),
    CIM10 = c(paste0("I", c(70, 71, 731, 738, 739, 771, 790, 792)),
              paste0("K", c(551, 558, 559)),
              paste0("Z", c(958, 959)))
  ),
  # Renal Disease
  rd = list(
    CIM9 = c(403, # 40301, 40311, 40391,
             40402, 40403, 40412, 40413, 40492, 40493,
             582,
             583, # 5830:5837,
             585, 586,
             588, # 5880,
             "V420", "V451", "V56"),
    CIM10 = c(paste0("I", c(120, 131)),
              paste0("N0", 32:37, 52:57),
              paste0("N", c(18, 19, 250)),
              paste0("Z", 490:492, 940, 992))
  )
)

# SQL regex ---------------------------------------------------------------

### Ajouter un '%' après chaque code pour pouvoir les utiliser dans du SQL regex
for (k in c("Combine_Dx_CCI_INSPQ18",
            "Charlson_Dx_CCI_INSPQ18",
            "Elixhauser_Dx_CCI_INSPQ18",
            "Charlson_Dx_UManitoba16")) {
  dt <- get(k)
  for (i in 1:length(dt)) {
    for (j in c("CIM9", "CIM10")) {
      dt[[i]][[j]] <- paste0(dt[[i]][[j]], "%")
    }
  }
  assign(k, dt)
}


# Save datas --------------------------------------------------------------

attr(Combine_Dx_CCI_INSPQ18, "MaJ") <- Sys.Date()
attr(Charlson_Dx_CCI_INSPQ18, "MaJ") <- Sys.Date()
attr(Elixhauser_Dx_CCI_INSPQ18, "MaJ") <- Sys.Date()
attr(Charlson_Dx_UManitoba16, "MaJ") <- Sys.Date()

use_data(Combine_Dx_CCI_INSPQ18,
         Charlson_Dx_CCI_INSPQ18,
         Elixhauser_Dx_CCI_INSPQ18,
         Charlson_Dx_UManitoba16,
         overwrite = TRUE)

rm(Combine_Dx_CCI_INSPQ18, Charlson_Dx_CCI_INSPQ18, Elixhauser_Dx_CCI_INSPQ18, Charlson_Dx_UManitoba16)
