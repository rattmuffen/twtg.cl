;;+---------------------+;;
;;|TWTG - THE WAY TO GO |;;
;;|Build:      09-12-08 |;;
;;|By:Magnus and Jasmin |;;
;;+---------------------+;;
;;svenska-vägar.cl av Anders Haraldsson.

;;TODO:
;;-Testning?
;;-Avrunda restiden till en decimal?

;;--INITIERING--
;(load "/home/magpe097/Kurser/TDDC67/Projekt/svenska-vägar.cl") ;;Skolan!
(load "C:\\Dropbox\\My Dropbox\\Skola\\Kurser\\TDDC67\\Projekt\\svenska-vägar.cl") ;;Hos Magnus!

(defparameter long-dist 99999999)
(defparameter distance-to-source-db (make-hash-table))
(defparameter previous-node-db (make-hash-table))
(skapa-sveriges-vägar)

;;--TOPPNIVÅ--
(defun kortaste-väg (startstad slutstad &optional (choice 'nil))
  "city x city x choice -> "
  (cond ((not (or (equal choice 'nil)
                  (equal choice 'lång)
                  (equal choice 'kort)))
         (error "Felaktigt val: ~S" choice))
        
        ((and (not (city-exist? startstad *väg-db*))
              (not (city-exist? slutstad *väg-db*)))
         (error "Varken städerna ~S eller ~S finns i kartan!~%För att se tillgängliga städer skriv (städer)." startstad slutstad))
        
        ((not (city-exist? startstad *väg-db*))
         (error "Staden ~S finns inte i kartan.~%För att se tillgängliga städer skriv (städer)." startstad))
        
        ((not (city-exist? slutstad *väg-db*))
         (error "Staden ~S finns inte i kartan.~%För att se tillgängliga städer skriv (städer)." slutstad))
        
        ((equal choice 'lång)
         (print-long-guidance (dijk *väg-db* startstad slutstad)))
        
        (t
         (print-short-guidance (dijk *väg-db* startstad slutstad)))))


(defun avstånd-mellan (startstad slutstad)
  "city x city -> "
  (if (not (or (city-exist? startstad *väg-db*)
               (city-exist? slutstad *väg-db*)))
      (print-distance (path-distance (dijk *väg-db* startstad slutstad) *väg-db*)
                      startstad
                      slutstad)
    (error "Någon av städerna ~S eller ~S finns inte i kartan." startstad slutstad)))
    

(defun tid-mellan (startstad slutstad)
  "city x city -> "
  (if (not (or (city-exist? startstad *väg-db*)
               (city-exist? slutstad *väg-db*)))
      (print-travtime (path-travtime (dijk *väg-db* startstad slutstad) *väg-db*)
                      startstad
                      slutstad)
    (error "Någon av städerna ~S eller ~S finns inte i kartan." startstad slutstad)))

(defun städer ()
  " -> "
  (print-cities))

(defun finns-stad? (stad)
  "city -> "
  (if (city-exist? stad *väg-db*)
      (format t "Staden ~S finns i kartan." stad)
    (format t "Staden ~S finns inte i kartan." stad)))

(defun hjälp ()
  " -> "
  (format t "       Välkommen till The Way to Go!~%`Vägvisningsprogrammet med en personlighet!´~%~%")
  (format t "Tillgängliga funktioner:~%")
  (format t " - (kortaste-väg stad stad ['kort eller 'lång]) - skriver ut en lång eller kort vägbeskrivning mellan städerna.~%")
  (format t " - (avstånd-mellan stad stad) - skriver ut avståndet på den kortaste vägen mellan städerna.~%")
  (format t " - (tid-mellan stad stad) - skriver ut restiden av den kortaste vägen mellan städerna.~%")
  (format t " - (städer) - skriver ut alla städer i kartan.~%")
  (format t " - (finns-stad? stad) - undersöker om staden finns i kartan.~%")
  (format t " - (hjälp) - den här texten.~%~%")
  (format t "Och kom ihåg; kör försiktigt där ute!~%"))



;;--UTSKRIFTER--
(defun print-distance (distance startcity goalcity)
  "float x city x city -> " 
  (format t "Minsta avståndet mellan ~S och ~S är: ~a km" startcity goalcity distance))

(defun print-travtime (travtime startcity goalcity)
  "float x city x city -> "
  (format t "Minsta restid mellan ~S och ~S är: ~a timmar" startcity goalcity travtime))

;;Går ingeom en path och skriver ut en lång vägbeskrivning.
(defun print-long-guidance (path)
  "path ->  "
  (let ((i 0))
    (dotimes (i (- (path-length path) 1))
      (let ((city1 (nth-node i path))
            (city2 (nth-node (+ 1 i) path)))
        (print-guidance-neighbours city1 city2)))                  
    (format t "Du har nått ditt resmål ~S efter ~a långa km!~%" (last-node path) (path-distance path *väg-db*))
    (format t "Minimal (laglig!) restid: ~a timmar." (path-travtime path *väg-db*))))

;;Går ingeom en path och skriver ut en kort vägbeskrivning
(defun print-short-guidance (path)
  "path -> "
  (let ((i 0))
    (dotimes (i (- (path-length path) 1))
      (format t "~S -> " (nth-node i path)))
    (format t "Framme i ~S!~%" (last-node path))
    (format t "Restid: ~a h  |  Avstånd: ~a km" (path-travtime path *väg-db*) (path-distance path *väg-db*))))


(defun print-guidance-neighbours (city ncity)
  "city x city -> "
  (format t "Åk ~a km på vägen mellan ~S och ~S.~%" (get-distance city ncity *väg-db*) city ncity))

(defun print-cities ()
  " -> "
  (let ((cities (sort (get-cities *väg-db*) #'string<)))
    (dolist (city cities)
      (format t "~S~%" city))
    (format t "Totalt så finns det ~a städer i kartan." (length cities))))



;;--PRIMITIVER--

;;Primitiver för PATH
;;Representation: lista med noder (symboler)

;;Konstruerar en path med en previous-node-db, en målnod och en startnod.
;;Detta gör funktionen genom att gå igenom previous-node-db:n tills den nått en node
;;som har 'nil associerat till sig (dvs startnoden).
(defun construct-path (prev-db target-node source-node)
  "DB x targetnode x sourcenode -> path"
  (let ((returnpath '())
        (node target-node))

    ;;Så länge vi inte har kommit till start-noden...
   (while (not (equal 'nil (get-db-entry node prev-db)))
     
     ;;Lägg till den aktuella noden till returnpath
     (setq returnpath (add-node node returnpath))
     
     ;;Sätt node till föregående nod i previous-databasen.
     (setq node (get-db-entry node prev-db)))
   
   ;;Vi får inte glömma att lägga till startnoden ;)
   (return-from construct-path (add-node source-node returnpath))))

;;Hämtar den n:te noden ur path.
(defun nth-node (n path)
  "integer x path -> node"
  (nth n path))

;;Lägger till en nod i path.
(defun add-node (node path)
  "node x path -> path"
  (push node path))

;;Returnerar den första noden i path.
(defun first-node (path)
  "path -> node"
  (first path))

;;Returnerar alla noder utom den första i path.
(defun rest-nodes (path)
  "path -> path"
  (rest path))

;;Returnerar sista noden i path.
(defun last-node (path)
  "path -> node"
  (first (last path)))

;;Räknar antalet noder i path.
(defun path-length (path)
  "path -> integer"
  (length path))

;;Räknar ut den totala avståndet i en path genom att gå igenom path
;;och addera avståndet mellan varje nod och dess efterföljare.
(defun path-distance (path mp)
  "path x map -> float"
  (let ((res 0)
        (i 0))
    
    (dotimes (i (- (path-length path) 1))
      ;;Adderar avståndet mellan den i:te noden i listan och nästföljande nod till res.
      (setq res (+ res (get-distance (nth-node i path) 
                                     (nth-node (+ i 1) path) 
                                     mp))))
    (return-from path-distance res)))

;;Räknar ut den totala restiden i en path genom att gå igenom path
;;och addera restiden mellan varje nod och dess efterföljare.
(defun path-travtime (path mp)
  "path x map -> float"
  (let ((i 0)
        (res 0))
    
    (dotimes (i (- (path-length path) 1))
      (let ((city1 (nth-node i path))
            (city2 (nth-node (+ 1 i) path)))
        ;;Adderar restiden mellan den i:te noden i listan och nästföljande med res
        (setq res (+ res (calculate-time city1 city2 mp)))))
    (return-from path-travtime res)))


;;Primitiver för MAP och CITY
;;Representation: Hashtabell resp. symbol

;;Skapar en karta med namn *väg-db*
;;Funktionen (skapa-sveriges-vägar) från svenska-vägar.cl
(defun create-map ()
  " -> "
  (skapa-sveriges-vägar))

;;Undersöker om city1 och city2 är grannar.
(defun is-neighbour? (city1 city2 mp)
  "city x city x map -> boolean"
  (member city2 (get-neighbours city1 mp)))

;;Returnerar grannstäderna till city i en lista
(defun get-neighbours (city mp)
  "city x map -> list of citites"
  (mapcar #'first (get-db-entry city mp)))

;;Returnerar avståndet mellan två grannstäder.
(defun get-distance (city1 city2 mp)
  "city x city x map -> float"
  (if (is-neighbour? city1 city2 mp)
      (first (rest (assoc city2 (get-db-entry city1 mp))))
    long-dist))

;;Funktionen returnerar den hastighetsbegränsing som är på vägen mellan
;;city1 och city2 i kartan mp
(defun get-speedlimit (city1 city2 mp)
  "city x city x map -> integer"
  (if (is-neighbour? city1 city2 mp)
      (second (rest (assoc city2 (get-db-entry city1 mp))))
    0))

;;Funktionen kalkylerar den estimereade restiden mellan två städer, city1 och city2.
;;Detta sker mha den underbara t=s/v formeln!
(defun calculate-time (city1 city2 mp)
  "city x city x map -> float"
  (let* ((distance (get-distance city1 city2 mp))
         (velocity (get-speedlimit city1 city2 mp))
         (travtime (/ distance velocity)))
    (return-from calculate-time travtime)))

;;Returnerar en lista med städerna i kartan mp
(defun get-cities (mp)
  "map -> list of cities"
  (get-keys mp))

;;Undersöker om staden city existerar i kartan mp
(defun city-exist? (city mp)
  "city x map -> boolean"
  (key-exist? city mp))


;;Primitiver för DB
;;Representation: HASHTABELL

;;Skapar en db med namn distance-to-source-db
(defun create-distance-to-source-db ()
  " -> DB"
  (defparameter distance-to-source-db (make-hash-table)))

;;Skapar en db med namn previous-node-db
(defun create-previous-node-db ()
  " -> DB"
  (defparameter previous-node-db (make-hash-table)))

;;Hämtar det värde som key är bundet till i db.
(defun get-db-entry (key db)
  "key x DB -> value"
  (gethash key db))

;;Lägger till, eller uppdaterar, en nyckels värde i en db.
(defun set-db-entry (key val db)
  "Key x value x DB -> updated DB"
  (setf (gethash key db) val))

;;Kollar om nyckeln key finns i db
(defun key-exist? (key db)
  "Key x DB -> boolean"
  (if (get-db-entry key db)
      t
    'nil))

;;Returnerar alla nycklar i DB i en lista
(defun get-keys (db)
  "DB -> List of keys"
  (let ((returnlist '()))
    (maphash #'(lambda (key val) (push key returnlist)) db)
    returnlist))

;;Skriver ut alla bindningar i en DB
(defun print-db (db)
  "DB -> "
  (maphash #'print-db-entry db))

;;Skriver ut [key]=[value]
(defun print-db-entry (key value)
  "key x value -> "
  (format t "~S=~S~%" key value))



;;--HJÄLPFUNK. TILL ALGORITMEN--
;;Hämtar den nod i nodes som har lägst distance associerat till sig i dist-db.
(defun get-smallest-node (nodes dist-to-source-db)
  "list of nodes x DB -> node"
  (let ((return-node '())
        (smallest-val long-dist))
    
    ;;Gå igenom nodlistan...
    (dolist (node nodes)
      
      ;;Om det avstånd som noden är associerat till i databasen är mindre än det hitills minsta värdet så skall vi: 
      (when (< (get-db-entry node dist-to-source-db)
               smallest-val)
        
        ;;Sätta det hittills minsta värdet till det nya minsta värdet!
        (setq smallest-val (get-db-entry node dist-to-source-db))
        
        ;;Sätta returvärdet till den aktuella noden!
        (setq return-node node)))
    (return-from get-smallest-node return-node)))



;;--ALGORITM, DIJKSTRAS--
;;Inspirerad av: http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
(defun dijk (graph source target)
  "Graph x sourcenode x targetnode -> (the shortest) path!"
  (let  (smallest-node
        (unvisited-nodes (get-cities graph)))
    
    ;;Sätt alla distance-to-source till long-dist
    ;;och alla previous-node till nil
    (dolist (node unvisited-nodes)
      (set-db-entry node long-dist distance-to-source-db)
      (set-db-entry node  'nil  previous-node-db))
    
    ;;Distance-to-source från source är givetvis 0
    (set-db-entry source 0 distance-to-source-db)
       
    ;;Så länge unvisited-nodes inte är tom skall vi...
    (while (not (equal 'nil unvisited-nodes))
      
      ;;Sätta smallest-node till den nod med lägst distance från de tillgängliga noderna i unvisited-nodes
      (setq smallest-node (get-smallest-node unvisited-nodes distance-to-source-db))
      
      ;;Ta bort den nod som vi snart kommer att expandera från unvisited-nodes
      (setf unvisited-nodes (remove smallest-node unvisited-nodes))
      
      ;;Gå igenom alla grannar 'neighbour till smallest-node...
      (dolist (neighbour (get-neighbours smallest-node graph))
        
        ;;Sätt totaldist till distance-to-source[neighbour] + avståndet smallest-node och neighbour
        (let ((total-dist (+ (get-db-entry smallest-node distance-to-source-db)
                             (get-distance smallest-node neighbour graph))))
          
          ;;Om total-dist < distance-to-source[neighbour] så:
          (when (< total-dist 
                   (get-db-entry neighbour distance-to-source-db))
            
            ;;Sätt distance-to-source[neighbour] till total-dist
            (set-db-entry neighbour total-dist distance-to-source-db)
            
            ;;Sätt previous-node[neighbour] till smallest-node, dvs det var smallest-node vi kom ifrån. 
            (set-db-entry neighbour smallest-node previous-node-db)))))
    
  ;;När loopen är klar har vi gått igenom hela grafen efter möjliga vägar, och alla dist är satta, det är bara att konstrukta!
  (return-from dijk (construct-path previous-node-db target source))))
