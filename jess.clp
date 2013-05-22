(import com.auphelia.models.Contact)
(import net.sf.json.JSONObject)

(deftemplate Contact (declare (from-class Contact)));
(deftemplate JSONObject (declare (from-class JSONObject)));

(defrule normalisation-code-postal
	"Normalisation code postal"
	(declare (salience 3))
	?c <- (Contact (codePostal ?cp))
	?jo <- (JSONObject)
	(test (or (or (regexp ".* .*" ?cp) (regexp ".*-.*" ?cp)) (regexp ".*[a-z].*" ?cp)))
	=>
	;(printout t "Code postal irrégulier : " ?cp crlf)
    (modify ?c (codePostal (call (call (call (get ?c codePostal) toUpperCase) replace " " "") replace "-" "")))
    (call (get ?jo OBJECT) accumulate "code_postal" "Retrait automatique des tirets et espaces du code postal.")
    ;(printout t "Code postal corrigé : " (get ?c codePostal) crlf)
)

(defrule normalisation-ville
	"Normalisation ville"
	(declare (salience 3))
	?c <- (Contact (ville ?v))
	?jo <- (JSONObject)
	(test (or (regexp "^[sS]t-.*" ?v) (regexp "^[sS]te-.*" ?v)))
	=>
	;(printout t "Ville irrégulière : " ?v crlf)
    (modify ?c (ville (call (get ?c ville) replace "St" "Saint")))
    (call (get ?jo OBJECT) accumulate "ville" "Traduction de St(e) en Saint(e).")
    ;(printout t "Ville corrigée : " (get ?c ville) crlf)
)

(defrule validation-code-postal
    "Validation code postal"
    (declare (salience 1))
    ?c <- (Contact (codePostal ?cp))
    ?jo <- (JSONObject)
    (test (not (regexp "^[ABCEGHJKLMNPRSTVXY]{1}\\d{1}[A-Z]{1}\\d{1}[A-Z]{1}\\d{1}$" ?cp)))
    =>
    ;(printout t "Code postal invalide." crlf)
    (call (get ?jo OBJECT) accumulate "code_postal" "Code postal invalide.")
)

(defrule validation-email
	"Validation email"
	(declare (salience 1))
	?c <- (Contact (email ?e))
	?jo <- (JSONObject)
	(test (not (regexp "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])" ?e)))
	=>
	;(printout t "Email invalide." crlf)
	(call (get ?jo OBJECT) accumulate "email" "Email invalide.")
)

(defrule format-province-quebec
	"Format province Québec"
	(declare (salience 2))
	?c <- (Contact (province ?p))
	?jo <- (JSONObject)
	(test (and (regexp "[qQ][uU][eEéÉ][bB][eE][cC]" ?p) (not (eq ?p "Québec"))))
	=>
	;(printout t "Correction province (avant) : " crlf)
	(modify ?c (province "Québec"))
	(call (get ?jo OBJECT) accumulate "province" "Correction de l'ortographe de la province.")
	;(printout t "Correction province (après) : " (get ?c province) crlf)
)

(defrule format-province-ontario
	"Format province Ontario"
	(declare (salience 2))
	?c <- (Contact (province ?p))
	?jo <- (JSONObject)
	(test (and (regexp "[oO][nN][tT][aA][rR][iI][oO]" ?p) (not (eq ?p "Ontario"))))
	=>
	;(printout t "Correction province (avant) : " ?p crlf)
	(modify ?c (province "Ontario"))
	(call (get ?jo OBJECT) accumulate "province" "Correction de l'ortographe de la province.")
	;(printout t "Correction province (après) : " (get ?c province) crlf)
)

(defrule format-province-cb
	"Format province Colombie-Britannique"
	(declare (salience 2))
	?c <- (Contact (province ?p))
	?jo <- (JSONObject)
	(test (and (regexp "[cC][oO][lL][oO][mM][bB][iI][eE][ -][bB][rR][iI][tT][aA][nN][nN][iI][qQ][uU][eE]" ?p) (not (eq ?p "Colombie-Britannique"))))
	=>
	;(printout t "Correction province (avant) : " crlf)
	(modify ?c (province "Colombie-Britannique"))
	(call (get ?jo OBJECT) accumulate "province" "Correction de l'ortographe de la province.")
	;(printout t "Correction province (après) : " (get ?c province) crlf)
)

(defrule validation-province
	"Validation province"
	(declare (salience 1))
	?c <- (Contact (province ?p))
	?jo <- (JSONObject)
	(test (not (member$ ?p (create$ "Québec","Ontario","Colombie-Britannique"))))
	=>
	;(printout t "Province invalide." crlf)
	(call (get ?jo OBJECT) accumulate "province" "Province invalide.")
)

(defrule code-postal-obligatoire
	"Code postal obligatoire"
	(declare (salience 4))
	?c <- (Contact (codePostal ?cp))
	?jo <- (JSONObject)
	(test (eq ?cp ""))
	=>
	;(printout t "Le code postal est obligatoire." crlf)
	(call (get ?jo OBJECT) accumulate "code_postal" "Le code postal est obligatoire.")
	(halt)
)

(defrule email-obligatoire
	"Email obligatoire"
	(declare (salience 4))
	?c <- (Contact (email ?e))
	?jo <- (JSONObject)
	(test (eq ?e ""))
	=>
	;(printout t "Le email est obligatoire." crlf)
	(call (get ?jo OBJECT) accumulate "email" "Le email est obligatoire.")
	(halt)
)

; (deftemplate ...) permet de définir un template à partir de la classe Java.
; (modify object (field new-value) ...) permet de modifier directement l'objet Java.
; (call object method arg1 ...    ) permet d'appeler n'importe quelle méthode d'un object Java.
; (call (get ?jo OBJECT) accumulate "codePostal" "Invalid postal code") ;; appelle la méthode accumulate sur le JSON Object auquel on accède par (get ... OBJECT)
; (get object field) permet de récupérer la valeur d'un champ.
