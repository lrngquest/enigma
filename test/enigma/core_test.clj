(ns enigma.core-test
  (:require [clojure.test :refer :all]
            [enigma.core :refer :all]))

;; from http://wiki.franklinheath.co.uk/index.php/Enigma/Paper_Enigma
(deftest a-test-1
  (testing "1  paper enigma"
    (Machine "" [(Rotor "B")(Rotor "I" \A 1)(Rotor "II" \B 1)(Rotor "III" \C 1)]) 
    (is (= (apply str (map transcode (seq "AEFAE JXXBN XYJTY")))
           "CONGR ATULA TIONS" ) )
    )  )

(deftest a-test-2
  (testing "2  paper enigma"
    (Machine "" [ (Rotor "B")(Rotor "I" \A 1)(Rotor "II" \B 1)(Rotor "III" \R 1) ])
    (is (= (apply str (map transcode (seq "MABEK GZXSG")))
           "TURNM IDDLE" ) ))  )

(deftest a-test-3
  (testing "3  paper enigma"
    (Machine "" [  (Rotor "B") (Rotor "I" \A 1)(Rotor "II" \D 1) (Rotor "III" \S 1) ]) 
    (is (= (apply str (map transcode (seq "RZFOG FYHPL")))
           "TURNS THREE"  ) ))  )

(deftest a-test-4
  (testing "4  paper enigma"
    (Machine "" [ (Rotor "B")(Rotor "I" \X 10)(Rotor "II" \Y 14)(Rotor "III" \Z 21)] )
    (is (= (apply str (map transcode (seq "QKTPE BZIUK")))
           "GOODR ESULT" ) ))  )

(deftest a-test-5
  (testing "5 paper enigme"
    (Machine
     "APBRCMFZGJILNTOVQSWX";;"AP BR CM FZ GJ IL NT OV QS WX"
     [ (Rotor "B") (Rotor "I" \V 10) (Rotor "II" \Q 14) (Rotor "III" \Q 21) ])
    (is (= (apply str (map transcode (seq "HABHV HLYDF NADZY")))
           "THATS ITWEL LDONE" ) )   ) )


;; from http://wiki.franklinheath.co.uk/index.php/Enigma/Sample_Messages

(deftest a-test-6
  (testing "Enigma Instruction Manual, 1930"
    (Machine
     "AMFINVPSTUWZ";;"AM FI NV PS TU WZ"
     [ (Rotor "A") (Rotor "II" \A 24) (Rotor "I" \B 13) (Rotor "III" \L 22) ] )

    (is (=
         (apply str (map transcode (seq "GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC FHUHM UNZEF RDISI KBGPM YVXUZ")))
         "FEIND LIQEI NFANT ERIEK OLONN EBEOB AQTET XANFA NGSUE DAUSG ANGBA ERWAL DEXEN DEDRE IKMOS TWAER TSNEU STADT"
         ))
    ))


(deftest a-test-7a
  (testing "Operation Barbarossa 1941 1of2"
    (Machine
   "AVBSCGDLFUHZINKMOWRX"
   [ (Rotor "B") (Rotor "II" \B 2) (Rotor "IV" \L 21) (Rotor "V" \A 12) ])

    (is (= (apply str (map transcode
                 (seq "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK-")))
           "AUFKL XABTE ILUNG XVONX KURTI NOWAX KURTI NOWAX NORDW ESTLX SEBEZ XSEBE ZXUAF FLIEG ERSTR ASZER IQTUN GXDUB ROWKI XDUBR OWKIX OPOTS CHKAX OPOTS CHKAX UMXEI NSAQT DREIN ULLXU HRANG ETRET ENXAN GRIFF XINFX RGTX-"  ))    ))

(deftest a-test-7b
  (testing "Operation Barbarossa, 1941 2of2"
    (Machine
   "AVBSCGDLFUHZINKMOWRX"
   [ (Rotor "B")(Rotor "II" \L 2) (Rotor "IV" \S 21) (Rotor "V" \D 12) ])

    (is (= (apply str (map transcode (seq "SFBWD NJUSE GQOBH KRTAR EEZMW KPPRB XOHDR OEQGB BGTQV PGVKB VVGBI MHUSZ YDAJQ IROAX SSSNR EHYGG RPISE ZBOVM QIEMM ZCYSG QDGRE RVBIL EKXYQ IRGIR QNRDN VRXCY YTNJR")))
           "DREIG EHTLA NGSAM ABERS IQERV ORWAE RTSXE INSSI EBENN ULLSE QSXUH RXROE MXEIN SXINF RGTXD REIXA UFFLI EGERS TRASZ EMITA NFANG XEINS SEQSX KMXKM XOSTW XKAME NECXK"   ))    ))

(deftest a-test-8
  (testing "U-264 (Kapitanleutnant Hartwig Looks), 1942"
    (Machine "ATBLDFGJHMNWOPQYRZVX" [(Rotor "B Thin") (Rotor "Beta" \V 1) (Rotor "II" \J 1)(Rotor "IV" \N 1) (Rotor "I" \A 22) ] ) 

    (is (=
         (apply str (map transcode (seq "NCZW VUSX PNYM INHZ XMQX SFWX WLKJ AHSH NMCO CCAK UQPM KCSM HKSE INJU SBLK IOSX CKUB HMLL XCSJ USRR DVKO HULX WCCB GVLI YXEO AHXR HKKF VDRE WEZL XOBA FGYU JQUK GRTV UKAM EURB VEKS UHHV OYHA BCJW MAKL FKLM YFVN RIZR VVRT KOFD ANJM OLBG FFLE OPRG TFLV RHOW OPBE KVWM UQFM PWPA RMFH AGKX IIBG")))
         "VONV ONJL OOKS JHFF TTTE INSE INSD REIZ WOYY QNNS NEUN INHA LTXX BEIA NGRI FFUN TERW ASSE RGED RUEC KTYW ABOS XLET ZTER GEGN ERST ANDN ULAC HTDR EINU LUHR MARQ UANT ONJO TANE UNAC HTSE YHSD REIY ZWOZ WONU LGRA DYAC HTSM YSTO SSEN ACHX EKNS VIER MBFA ELLT YNNN NNNO OOVI ERYS ICHT EINS NULL"
         ))
    ))

(deftest a-test-9
  (testing "Scharnhorst (Konteradmiral Erich Bey), 1943"
    (Machine
     "ANEZHKIJLRMQOTPVSWUX";;"AN EZ HK IJ LR MQ OT PV SW UX"
     [ (Rotor "B") (Rotor "III" \U 1) (Rotor "VI" \Z 8) (Rotor "VIII" \V 13)] )

    (is (= (apply str (map transcode (seq "YKAE NZAP MSCH ZBFO CUVM RMDP YCOF HADZ IZME FXTH FLOL PZLF GGBO TGOX GRET DWTJ IQHL MXVJ WKZU ASTR")))
           "STEU EREJ TANA FJOR DJAN STAN DORT QUAA ACCC VIER NEUN NEUN ZWOF AHRT ZWON ULSM XXSC HARN HORS THCO"
           ))
    ) )


