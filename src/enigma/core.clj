(ns enigma.core
  (:gen-class))


(def rotor-specs {  ;; map keyed by string
   "I"      ["EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Q" ] ;; 1930 Enigma I
   "II"     ["AJDKSIRUXBLHWTMCQGZNPYFVOE" "E" ] ; ditto
   "III"    ["BDFHJLCPRTXVZNYEIWGAKMUSQO" "V" ] ; ditto
   "IV"     ["ESOVPZJAYQUIRHXLNFTGKDCMWB" "J" ] ;; December 1938 M3 Army
   "V"      ["VZBRGITYUPSDNHLXAWMJQOFECK" "Z" ] ; ditto
   "VI"     ["JPGVOUMFYQBENHZRDKASXLICTW" "ZM"] ;;1939 M3 & M4 Naval (FEB 1942
   "VII"    ["NZJHGRCXMYSWBOUFAIVLPEKQDT" "ZM"] ;ditto
   "VIII"   ["FKQHTLXOCBJSPDZRAMEWNIUYGV" "ZM"] ;ditto
   "Beta"   ["LEYJVCNIXWPBQMDRTAKZGFUHOS" ""  ] ;; Spring 1941 M4 R2
   "Gamma"  ["FSOKANUERHMBTIYCWLQPZXVGJD" ""  ] ;ditto
   "A"      ["EJMZALYXVBWFCRQUONTSPIKHGD" ""  ]
   "B"      ["YRUHQSLDPXNGOKMIEBFZCWVJAT" ""  ]
   "C"      ["FVPJIAOYEDRZXWGCTKUQSBNMHL" ""  ]
   "B Thin" ["ENKQAUYWJICOPBLMDXZVFTHRGS" ""  ] ;;1940 M4 R1 (M3 + Thin)
   "C Thin" ["RDOBJNTKVEHMLFCWZAXGYIPSUQ" ""  ] } ) ;ditto

(def AL 26)
(def mach (atom { :pbv []  :rv []  :ofsv [] :maxRtr 0}) )

(defn ord "A..Z -> 0..25" [ch] (- (int ch) (int \A)) )


(defn tbl "make table vec from string" [tstr] (vec (map ord tstr)))

(defn rcp "make recip" [tblv]
  (vec (map (fn [v](v 1)) (sort (map-indexed (fn [idx itm] [itm idx]) tblv) )) ))

(defn notchfn "" [ring ch] (mod(+ (- (ord ch) ring) 1 AL)AL) )

(defn Rotor "ala ctor"
  ([name]
     (Rotor name \A 1) )
  
  ([name ofsch ringn]
     (let [[tblstr nchstr]  (rotor-specs name)   ]
       {:table     (tbl tblstr)
        :recip     (rcp (tbl tblstr))
        :notches   (set (map (partial notchfn ringn) nchstr))
        :zofs      (+ (- (ord ofsch) ringn) 1)
        :name  name } )   ) 
  )

(defn knocks? "" [rx ]
  (let [this-rtr  ((:rv @mach) rx)    this-ofs  ((:ofsv @mach) rx)  ]
    (contains? (:notches this-rtr)  (mod (+ AL this-ofs) AL) ) )  )

(defn advance "update state of  ofs  for rotor at  rx" [rx]
  (swap! mach assoc :ofsv (assoc (:ofsv @mach) rx  (inc ((:ofsv @mach) rx)) ) ))

(defn enter "" [rx ch ofs]
  (let [this-ofs  ((:ofsv @mach) rx)
        this      ((:rv @mach ) rx)
        xtemp     (mod (- (+ ch this-ofs) ofs) AL)    ]
    ((:table this) (if (< xtemp 0)   (+ xtemp AL)   xtemp) )  )   )

(defn exit "" [rx ch ofs]
  (let [this-ofs  ((:ofsv @mach) rx)    this  ((:rv @mach ) rx)   ]
    ((:recip this) (mod (- (+ ch this-ofs AL) ofs) AL) ) )  )


(defn upd-pair [arr pair]
  (aset arr (first pair)(last pair))
  (aset arr (last pair)(first pair)) arr)

(defn vtst [aray pairs] (vec (last (map (partial upd-pair aray) pairs))) )

(defn init-pb "set up plugboard parings" [s]
  (if (= 0 (count s))
    (vec (range AL))  ;; i.e. map each char to itself
    (vtst (int-array (range AL)) (partition 2 (map ord s))) )    )


(defn Machine [pb-str rv]
  (swap! mach assoc
         :pbv    (init-pb pb-str)
         :rv     rv
         :ofsv   (vec (map (fn [rtr] (:zofs rtr)) rv) ) ;;anon. fn
         :maxRtr (dec (count rv))  )  )


(defn transcode "Machine 'mathod' " [ ch]
   (if (or (< (int ch) (int \A))  (> (int ch) (int \Z)))
    ch
    (let [maxRtr   (:maxRtr @mach)
          rg2lfi   (case maxRtr  3  [3 2 1 0]   4  [4 3 2 1 0])
          lf2rgi   (case maxRtr  3  [1 2 3]     4  [1 2 3 4 ]  )
          leftx    (- maxRtr 2)
          middlex  (- maxRtr 1)
          _        (cond (knocks? middlex) (do (advance leftx)(advance middlex))
                         (knocks? maxRtr ) (do (advance middlex))    )
          _        (do (advance maxRtr)) ;; right adv. for each input char 
          ofsv     (:ofsv @mach)  ;; i.e. 0..maxRtr
          pbv      (:pbv @mach)

          [ofs1d ch1d]  (reduce (fn [[ofs-in ch-in] rx]
                                  [ ((:ofsv @mach) rx)
                                    (enter rx ch-in ofs-in) ]  )
                                [0 (pbv (ord ch))]  rg2lfi )          
;;_ (printf "c%2d o%2d  " ch1d ofs1d)

          [ofs2c ch2c] (reduce (fn [[ofs-in ch-in] rx]
                                 [ ((:ofsv @mach) rx)
                                   (exit rx ch-in ofs-in) ]    )
                               [ofs1d ch1d] lf2rgi )

          chu    (mod (- ch2c ofs2c) AL)  ;;unmap ch
;;_ (printf "c%2d o%2d  " ch2c ofs2c)  ;;_ (printf "u c%2d o%2d  " chu ofs2c )
          chp    (pbv  (if (>= chu 0)  chu  (+ chu AL)) )      ]
;;(printf "c%2d %c\n" chp (char (+ chp (int \A))))
      (char (+ chp (int \A)))   )  )     )


(defn -main  " ..."  [& args]
  
   (Machine
    "APBRCMFZGJILNTOVQSWX";;"AP BR CM FZ GJ IL NT OV QS WX"  TODO
    [ (Rotor "B") (Rotor "I" \V 10) (Rotor "II" \Q 14) (Rotor "III" \Q 21) ])
   (println (apply str (map transcode (seq "HABHV HLYDF NADZY"))))
    ;;THATS ITWEL LDONE
   )
