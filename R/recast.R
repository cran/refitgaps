#' Rearanjează clasele indicate oră de oră pe liniile profesorilor
#' (și cuplajelor) într-o matrice-orar (păstrând unicitatea claselor pe
#' coloanele orare și când este cazul, respectând alocarea pe ore existentă
#' pentru lecțiile tuplate), astfel încât numărul total de ferestre (incluzând
#' pe cele induse de cuplaje) să devină după un anumit număr de iterații, 
#' cât se poate de mic.
#'
#' "programul principal" (exportat) este 'recast()', dar acesta are
#' în interior 6 funcții care, folosind variabilele definite în
#' programul principal (precum și o listă internă de "mutări corectoare"),
#' acționează pe matricea-orar corespunzătoare iterației curente, 
#' pe parcursul căutării unui orar "mai bun" (cu mai puține ferestre).
#'
#' @param matPr Matricea-orar furnizată de către utilizator; liniile
#'     sunt numite după profesori/cuplaje; pe fiecare linie sunt înregistrate
#'     pe ore clasele la care intră profesorul (sau '-' dacă este liber);
#'     fiecare clasă apare o singură dată, pe fiecare coloană de rang mai mic
#'     sau egal cu numărul de ore/zi al acelei clase.
#' @param TPL setul tuplajelor (dacă este cazul); în matricea-orar lecțiile
#'     dintr-un același tuplaj apar în câte o aceeași coloană orară (iar
#'     'recast()' va păstra alocările existente pentru lecțiile tuplate).
#' @param Niter numărul de iterații pentru căutarea unui orar "mai bun".
#' @param GD un număr ideal de ferestre, pentru a stopa căutarea.
#' @return o matrice-orar cu aceleași lecții ca în orarul inițial, dar 
#'     realocate pe orele 1:7 astfel încât numărul total de ferestre este 
#'     de regulă mai mic ca inițial (cât de mic, depinde și de cât de mare 
#'     este 'Niter' și mai depinde și de structura tuplajelor).
#' @export
#'
#' @examples
#' # Alegem 'Niter' mic, pentru o execuție rapidă
#' ORR <- recast(MOZ, TPL, Niter=100)
#' HG <- have_gaps(ORR[[1]])

recast <- function(matPr, TPL = NULL, Niter = 3000, GD = 2) {
    # dependențele de alocare a lecțiilor, induse de cuplaje (dacă există)
    Tw1 <- Tw2 <- pr_ext <- nTw <- NULL
    twins <- depends_on(matPr)
    if(!is.null(twins)) {
        Tw1 <- twins[[1]]; Tw2 <- twins[[2]] 
        nTw <- union(names(Tw1), names(Tw2))
        if(length(twins[[3]]) > 1)
            pr_ext <- twins[[3]]
    }     
    # variabilele introduse mai sus sunt necesare în funcțiile interne
    # pe care le încorporăm succesiv în continuare:
    
    # numărul de ferestre din matricea-orar curentă
    count_gaps <- function(Mop) { 
        Vsb <- bin_patt(Mop)  # vectorul şabloanelor orare
        ng <- 0L
        for(p in names(Tw1))  # cazul celor care intră în vreun cuplaj
            ng <- ng + cnt_holes(sum(c(Vsb[p], Vsb[Tw1[[p]]])))
        for(s in pr_ext)  # ferestre în cuplaje cu profesor fără ore proprii
            ng <- ng + cnt_holes(sum(Vsb[s])) 
        # adăugăm ferestrele celor neimplicaţi în cuplaje    
        ng + sum(unlist(lapply(Vsb[setdiff(names(Vsb), nTw)], 
                               cnt_holes), use.names=FALSE))
    }
    

    # Mută o clasă între două coloane orare, păstrând unicitatea claselor
    # pe coloanele matricei orar curente; returnează matricea-orar rezultată.
    move_cls <- function(Mop, h1, h2, Cls) {
        sem <- FALSE  
        # Secvența de mutări succesive care sunt necesare pentru a aduce o clasă
        # din coloana h1, în coloana h2 (păstrând unicitatea pe coloană pentru
        # clase) este dată de unul dintre circuitele KEMPE existente în graful
        # orientat ale cărui arce unesc o valoare din coloana h1 (clasă sau loc
        # liber '-') cu aceea de pe aceeași linie, din coloana h2
        path_kempe <- function(h1, h2) {
            L1 <- as.vector(Mop[, h1])  # fără numele de linii
            L2 <- as.vector(Mop[, h2])
            if(! (Cls %in% L2 && Cls %in% L1)) 
                return(NULL)  # Cls ar trece dincolo de numărul său de ore
            i <- match(Cls, L1, 0L)  # indexul liniei cu Cls în ora h1
            path <- i
            # înlănțuie clase omonime din h1 și h2 până ce fie închide
            # circuitul, fie ajunge într-un loc liber '-' (semnalând în 'sem')
            repeat {  
                q <- L2[i]
                # lecțiile din tuplaje, trebuind să cadă în câte o aceeași oră,
                # au fost marcate în prealabil cu 'X' 
                if(q == "X") return(NULL)  # refuză mutarea 'X' (clasă tuplată)
                if(q == Cls) break  # avem circuit fără '-'
                if(q == '-') {  # s-a găsit numai lanțul până la '-'
                    # semnalează în exterior (lui 'move_cls()', NU în 
                    # "global-environment") că va trebui completat circuitul
                    sem <<- TRUE
                    break
                }
                i <- match(q, L1, 0L)
                if(i==0) return(NULL)  # q ar trece dincolo de ultima oră
                    path <- c(path, i)  # continuă înlănțuirea claselor omonime
                }
            path  # indecșii liniilor pe care s-au înlănțuit clasele
        }  # END 'path_kempe()'
    
        Pth <- path_kempe(h1, h2)
        if(is.null(Pth)) return(NULL)
        if(sem) {  # completează cu lanțul "invers", din '-' spre Cls
            Pth1 <- path_kempe(h2, h1)
            if(is.null(Pth1)) return(NULL)
            Pth <- c(Pth, Pth1) 
        }
        # interschimbă valorile din cele două coloane, pe liniile din 'Pth'
        Mop[Pth, c(h1, h2)] <- Mop[Pth, c(h2, h1)]

        # Apar suprapuneri ascunse, pe cele două coloane?
        for(prf in names(Tw2))
            for(h in c(h1, h2)) {
                if(Mop[prf, h] != "-" &&
                   sum(! Mop[Tw2[[prf]], h] %in% '-') > 0)
                       return(NULL)
            }

        Mop # matricea-orar rezultată în urma mutării clasei Cls
    }  # END 'path_cls()'  

    # alege la întâmplare un anumit număr de profesori care (în matricea-orar
    # curentă) au ferestre, determină mutările "corectoare" corespunzătoare
    # acestora în lista prealabilă 'SBC' și returnează una oarecare.
    repair_move <- function(Mop) {
        Vsb <- bin_patt(Mop)  # șabloanele binare ale tuturor liniilor
        B1 <- which(Vsb %in% names(SBC))   # șabloanele binare cu ferestre
        sz <- min(length(B1), 20)  # vizăm o parte a liniilor cu ferestre
        lh1 <- lh2 <- vector("integer", 0)
        lql <- vector("character", 0)
        B <- B1 %>% sample(size = sz)
        for(id in B) {
            h12 <- SBC[[as.character(Vsb[id])]]  # mutările corectoare
            H1 <- h12[["h1"]]; H2 <- h12[["h2"]]
            for(i in seq_along(H1)) {
                h1 <- H1[i]; h2 <- H2[i]
                if(max(h1, h2) > ncol(Mop))
                    next  # ocolește mutările în afara coloanelor existente
                if(Mop[id, h1] == "X") next  # refuză mutarea de lecții tuplate
                lql <- c(lql, Mop[id, h1])
                lh1 <- c(lh1, h1)
                lh2 <- c(lh2, h2)
            }
        } 
        wh <- sample(x = 1:length(lh1), size = 1)
        c(lh1[wh], lh2[wh], lql[wh])
    }  # END 'repair_move()'

    # aplică (prin 'move_cls()') mutarea corectoare primită din 'repair_move()'
    # și returnează matricea-orar rezultată, dacă aceasta nu are mai multe
    # ferestre decât cea inițială.
    choose_next <- function(Mop) {
        swp <- repair_move(Mop)  # o mutare corectoare, (h1 h2 cls)
        G <- count_gaps(Mop)
        mor <- move_cls(Mop, swp[1], swp[2], swp[3])
        if(is.null(mor)) return(NULL)
        ng <- count_gaps(mor)
        if(ng > G) return(NULL)
        list(mor, ng)  # matricea după mutare și numărul său de ferestre
    }

    # Caută din aproape în aproape un orar "mai bun" (cu un număr total
    # de ferestre care nu este mai mare decât cel inițial)
    search_better <- function(Mop) {
        ng <- NG1  # NG1 păstrează (în 'recast()') minimul curent de ferestre
        Rep <- 4 
        while(ng > GD) {
            Best <- Mop
            repeat {
                for(i in 1:Niter) {
                    mpn <- choose_next(Best)  # aplică o mutare corectoare
                    if(is.null(mpn)) next
                    ngi <- mpn[[2]]  # numărul de ferestre pe orarul rezultat
                    if(ngi <= ng) { 
                        Best <- mpn[[1]]
                        ng <- ngi
                        NG1 <<- ng  # actualizează minimul de ferestre
                    } 
                }   
                # cat("*", ng, " ")  ## semnalează încheierea unei bucle
                Rep <- Rep - 1
                if(ng <= GD || Rep < 0)
                    return(Best)  # orarul curent n-a mai putut fi îmbunătăţit
            }
        }
        Mop  # returnează chiar Mop-inițial, dacă are cel mult GD ferestre
    }  # END 'search_better()'

    # maschează/demaschează lecțiile tuplate (dacă există)
    mask_unmask <- function(Mop, mu = TRUE) {
        if(is.null(TPL)) return(Mop)
        for(i in 1:nrow(TPL)) {
            Prof <- strsplit(TPL$prof[i], " ")[[1]]
            Cls <- strsplit(TPL$cls[i], " ")[[1]]
            Mop[Prof, TPL$ora[i]] <- if(mu == TRUE) Cls else "X"
        }
        Mop
    }

    # finalizarea "programului principal" din startul funcției 'recast()'
    NG1 <- count_gaps(matPr) # numărul inițial de ferestre
    # cat(NG1) 
    OZ <- mask_unmask(matPr, FALSE)  # maschează lecțiile tuplate
    orr <- search_better(OZ)  # actualizează 'NG1', pentru orarul curent
    # demaschează lecțiile tuplate și returnează într-o listă,
    # matricea-orar finală și numărul său de ferestre:
    list(mask_unmask(orr), NG1)
}
