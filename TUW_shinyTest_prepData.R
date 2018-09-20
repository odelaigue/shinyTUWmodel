
#! ===============================
#!
#! Description       : Data preparation for TUW model Shiny app
#! 
#! Authors           : Olivier Delaigue <olivier.delaigue@irstea.fr>
#! 
#! Creation date     : 2018-09-20
#! 
#! Commentaire       : 
#!
#! ===============================






## ------------------------------ Packages

library(TUWmodel)






## ------------------------------ Data preparatiton

data(example_TUWmodel)

basin1 <- list(ts       = data.frame(prec = apply(P_Vils  , 1, weighted.mean, w = areas_Vils),
                                     airt = apply(T_Vils  , 1, weighted.mean, w = areas_Vils),
                                     ep   = apply(PET_Vils, 1, weighted.mean, w = areas_Vils),
                                     qobs = Q_Vils[seq_len(nrow(P_Vils))]),
               area     = sum(areas_Vils),
               paramTUW = c(1.02, 1.70, 2, 0, -0.336, 0.934, 121, 2.52,
                            0.473, 9.06, 142, 50.1, 2.38, 10, 25))

basin2 <- list(ts       = data.frame(prec = apply(P_Vils  , 1, weighted.mean, w = areas_Vils) * 1.2,
                                     airt = apply(T_Vils  , 1, weighted.mean, w = areas_Vils) + 5,
                                     ep   = apply(PET_Vils, 1, weighted.mean, w = areas_Vils) * 1.1,
                                     qobs = Q_Vils[seq_len(nrow(P_Vils))] * 1.2),
               area     = sum(areas_Vils) + 5,
               paramTUW = c(5, 4, 3, 0, 0, 0, 200, 2,
                            1, 9, 200, 50, 4, 10, 25))

basin_list <- list(Basin1 = basin1, Basin2 = basin2)

# save(basin1, file = "Basin1.RData")
# save(basin2, file = "Basin2.RData")

save(basin_list, file = "Basin_list.RData")
