(import com.auphelia.models.Contact)
(import net.sf.json.JSONObject)

(deftemplate Contact (declare (from-class Contact)));
(deftemplate JSONObject (declare (from-class JSONObject)));

(defrule normalisation-code-postal
    "Normalisation code postal"
    ?c <- (Contact (codePostal ?cp))
    ?jo <- (JSONObject)
    (test (not (regexp "^[ABCEGHJKLMNPRSTVXY]{1}\\d{1}[A-Z]{1}\\d{1}[A-Z]{1}\\d{1}$" ?cp)))
    =>
    (printout t "Code postal irrégulier : " ?cp crlf)
    (modify ?c (codePostal (call (call (call (get ?c codePostal) toUpperCase) replace " " "") replace "-" "")))
    (printout t "Code postal corrigé : " (get ?c codePostal) crlf)
    (call (get ?jo OBJECT) accumulate "code_postal" "Code postal invalide")
)

(defrule validation-email
    "Validation email"
	?c <- (Contact (email ?e))
	?jo <- (JSONObject)
	(test (not (regexp "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])" ?e)))
	=>
	(printout t "Email irrégulier : " ?e crlf)
	(call (get ?jo OBJECT) accumulate "email" "Email invalide")
)

; (deftemplate ...) permet de définir un template à partir de la classe Java.
; (modify object (field new-value) ...) permet de modifier directement l'objet Java.
; (call object method arg1 ...    ) permet d'appeler n'importe quelle méthode d'un object Java.
; (call (get ?jo OBJECT) accumulate "codePostal" "Invalid postal code") ;; appelle la méthode accumulate sur le JSON Object auquel on accède par (get ... OBJECT)
; (get object field) permet de récupérer la valeur d'un champ.
