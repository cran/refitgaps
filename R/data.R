#' A simple example (from a Romanian school) of a daily schedule
#'
#' Matrice-orar cu 59 linii și 7 coloane. Numele de linie reprezintă profesorii
#' (pe câte 3 caractere) sau cuplajele (pe câte 6 caractere); pe fiecare linie
#' sunt înregistrate clasele (sau '-' dacă este liber în acea oră) la care are
#' de intrat profesorul sau cuplajul respectiv, în orele 1:7 ale zilei.
#' Fiecare clasă apare câte o singură dată, pe fiecare dintre coloanele de rang
#' mai mic sau egal cu numărul de ore/zi (cel mult 7) ale clasei respective.
#'
#' \itemize{
#'     \item A matrix with 59 rows and 7 columns
#'     \item The rownames are the teachers ('prof')
#'     \item The columns are the 1:7 hours of the day
#'     \item The values are the classes of the 'prof' in each hour, or '-' if
#'         the 'prof' is free in that hour
#' }
"MOZ"
#'
#'
#' Tupled lessons for 'MOZ'
#'
#' 6 tuplaje asociate matricei-orar 'MOZ'.
#' Lecțiile dintr-un același tuplaj au fost alocate într-o aceeași oră.
#'
#' @format
#' \describe{
#'     \item{prof}{Vectorul profesorilor sau cuplajelor din tuplaj}
#'     \item{cls}{Vectorul claselor din tuplaj, de aceeași lungime cu 'prof';
#'         pentru fiecare tuplaj asocierea 'prof/cls' (în cadrul matricei-orar)
#'         respectă ordinea profesorilor și claselor din acel tuplaj}
#'     \item{ora}{Ora alocată în matricea-orar lecțiilor din tuplaj}
#'}
"TPL"

