## Internal functions used for to calculate the number of gaps.

# 'cnt_holes()' returnează numărul de ferestre din orarul unui profesor (dar
# fără a lua în considerație cuplajele în care ar fi implicat).
# SB este șablonul binar al orelor profesorului: octet ai cărui biți în 
# pozițiile 0:6 reprezintă orele 1:7, un bit fiind '1' dacă în ora
# asociată acestuia, profesorul are de făcut o lecție la o anumită clasă.
#
# Variabila internă 'h2bin' conține măștile binare ale orelor 1:7
#
cnt_holes <- function(SB) {
    bits <- which(bitwAnd(SB, h2bin) > 0) # rangurile biților '1'
    n <- length(bits)  # numărul de ore
    bits[n] - bits[1] + 1 - n
} # Numărul de biți '0' aflați între biți '1' (deci, "ferestre")


# 'bin_patt()' produce vectorul șabloanelor binare ale tuturor liniilor 
# din matricea-orar curentă.
# În matricea-orar, numele de linii corespund profesorilor și
# fiecare linie înregistrează clasele alocate pe orele 1:7 profesorului
# (sau '-' dacă în ora respectivă acesta este liber)
#
bin_patt <- function(matPr) 
    apply(matPr, 1, function(Row) sum(h2bin[which(! Row %in% "-")]))


# Pentru un șablon binar dat (corespunzător unei linii din matricea-orar), 
# 'byte_line()' produce un format literal al acestuia (folosind '*' și '-').
# 
byte_line <- function(SB)
    bitwAnd(SB, h2bin) %>% 
        sapply(function(b) if(b) "*" else "-") %>% paste0(collapse = "")

# 'new_sb()' produce șablonul binar rezultat prin mutarea unei lecții (bit '1')
# a profesorului respectiv într-o coloană în care acesta era liber (bit '0'). 
#
new_SB <- function(SB, h1, h2)
    bitwShiftL(1, h1-1) %>% bitwNot() %>% bitwAnd(SB) %>% 
    bitwOr(bitwShiftL(1, h2-1))


# Avem mai multe categorii de profesori: cei care au numai ore proprii; 
# cei care pe lângă ore proprii, au și lecții în cuplaje; cei care (numiți 
# "externi") nu au ore proprii, ci doar în cuplaje. La calculul ferestrelor 
# unuia angajat în cuplaje, avem de ținut seama și de ferestrele cuplajelor. 
#
depends_on <- function(matPr) {
    Prof <- rownames(matPr)
    sgp <- Prof[nchar(Prof) == 3]
    if(length(sgp) == length(Prof)) 
        return(NULL)  # nu există cuplaje
    # lecțiile proprii ale celor implicați în cuplaje trebuie să 
    # nu se suprapună cu lecțiile cuplajelor respective
    Tw1 <- map(sgp, function(P) {
        gp <- Prof[which(grepl(P, Prof))]
        if(length(gp) > 1) setdiff(gp, P)
    }) %>% setNames(sgp) %>% purrr::compact()

    cup <- Prof[nchar(Prof) == 6]
    # pot exista profesori "externi" (fără ore proprii, ci numai în cuplaje)
    pr_ext <- vector("character")   
    Tw2 <- lapply(cup, function(PP) {
        p12 <- vector("character")
        p1 <- substr(PP, 1, 3)
        p2 <- substr(PP, 4, 6)
        if(! (p1 %in% sgp | p2 %in% sgp)) 
            pr_ext <- c(pr_ext, PP)  # 'PP' implică un profesor "extern"
        for(p in c(p1, p2))
            if(p %in% sgp) p12 <- c(p12, p, Tw1[[p]])
        setdiff(p12, PP) %>% unique()
    }) %>% setNames(cup) %>% purrr::compact()
    list(Tw1, Tw2, pr_ext)
}



