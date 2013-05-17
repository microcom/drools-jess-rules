(import com.auphelia.models.Contact)

(deftemplate Contact (declare (from-class Contact)));

(defrule normalisation-code-postal
    "Normalisation code postal"
    ?c <- (Contact (codePostal ?cp))
    (test (not (regexp "^[ABCEGHJKLMNPRSTVXY]{1}\\d{1}[A-Z]{1}\\d{1}[A-Z]{1}\\d{1}$" ?cp)))
    =>
    (printout t "Code postal irrégulier : " ?cp crlf)
    (modify ?c (codePostal (call (call (call (get ?c codePostal) toUpperCase) replace " " "") replace "-" "")))
    (printout t "Code postal corrigé : " (get ?c codePostal) crlf)
)

; (deftemplate ...) permet de définir un template à partir de la classe Java.
; (modify object (field new-value) ...) permet de modifier directement l'objet Java.
; (call object method arg1 ...    ) permet d'appeler n'importe quelle méthode d'un object Java.
; (call (get ?jo OBJECT) accumulate "codePostal" "Invalid postal code") ;; appelle la méthode accumulate sur le JSON Object auquel on accède par (get ... OBJECT)
; (get object field) permet de récupérer la valeur d'un champ.