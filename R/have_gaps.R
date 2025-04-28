#' Evidențiază orarele profesorilor care au ferestre
#'
#' @param OZ Matrice-orar; liniile sunt numite după profesori/cuplaje; pe
#'     fiecare linie sunt înregistrate pe ore clasele la care intră profesorul
#'     (sau '-' dacă este liber); fiecare clasă apare o singură dată, pe 
#'     fiecare coloană de rang cel mult egal cu numărul de ore/zi al clasei.
#'
#' @return Subsetul liniilor cu ferestre; pentru cazul ferestrelor "ascunse"
#'     (induse de angajarea în cuplaje), este anexat șablonul tuturor orelor
#'     în care este implicat profesorul respectiv.
#'
#' @export
#'
#' @examples
#' HG <- have_gaps(MOZ) 

have_gaps <- function(OZ) {
    Vsb <- bin_patt(OZ)  # transformă liniile orare în șabloane binare
    B1 <- which(Vsb %in% names(SBC))  # indecșii celor cu ferestre
    B2 <- Vsb[B1]  # prof_cu_ferestre --> șablonul_binar_al_orarului_său
    OZ <- OZ %>% as.data.frame() %>% 
          tibble::rownames_to_column("prof") %>%
          mutate(ore = "")
    for(P in OZ$prof) {
        if(nchar(P) == 6 & P %in% names(B2)) {
            P1 <- substr(P, 1, 3); P2 <- substr(P, 4, 6)
            # dacă P1 sau P2 este profesor "extern" (cu ore numai în cuplaj)
            if(! (P1 %in% OZ$prof || P2 %in% OZ$prof))
                OZ[OZ$prof == P, "ore"] <- byte_line(B2[[P]])
        } else {
            cpl <- OZ[grepl(P, OZ$prof), ]
            if(nrow(cpl) == 1) {  # P (cu ferestre) neangajat în cuplaje
                if(P %in% names(B2))
                    OZ[OZ$prof==P, "ore"] <- byte_line(B2[[P]])
            } else {  # P angajat în cuplaje
                ptt <- sum(Vsb[cpl$prof])
                if(cnt_holes(ptt) > 0)
                    OZ[OZ$prof == P, "ore"] <- byte_line(ptt)
            }
        }
    }
    OZ %>% filter(.data$ore != "")
}

