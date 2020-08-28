open Core

let get_industry_list : Type.IndustryList.t =
  [ { category = "real estate"
    ; codes =
        [ "sh.600606"
        ; "sz.000540"
        ; "sz.000002"
        ; "sh.600048"
        ; "sh.600208"
        ; "sh.600515"
        ; "sz.000069"
        ; "sh.600791"
        ; "sh.600239"
        ; "sh.600503"
        ; "sh.600185"
        ; "sh.600077"
        ; "sh.600223"
        ; "sh.600383"
        ; "sz.000667"
        ; "sz.000006"
        ; "sz.000732"
        ; "sz.000560"
        ; "sz.000656"
        ; "sh.600895"
        ; "sh.600094"
        ; "sz.000691"
        ; "sh.600393"
        ; "sz.002208"
        ; "sh.600604"
        ; "sz.002305"
        ; "sh.600466"
        ; "sh.600565"
        ; "sh.600555"
        ; "sz.002146"
        ; "sz.000961"
        ; "sz.000886"
        ; "sz.000011"
        ; "sz.000620"
        ; "sz.000036"
        ; "sh.600890"
        ; "sh.600340"
        ; "sh.600622"
        ; "sz.000671"
        ; "sh.600325"
        ]
    }
  ; { category = "Capital Market Services"
    ; codes =
        [ "sz.300059"
        ; "sh.601099"
        ; "sz.002797"
        ; "sh.601108"
        ; "sh.600030"
        ; "sz.000166"
        ; "sz.000750"
        ; "sh.601788"
        ; "sh.601162"
        ; "sh.601878"
        ; "sh.601555"
        ; "sh.601377"
        ; "sh.600369"
        ; "sh.600837"
        ; "sh.600918"
        ; "sh.600109"
        ; "sh.601688"
        ; "sz.000783"
        ; "sh.601990"
        ; "sz.000776"
        ; "sz.000686"
        ; "sh.600909"
        ; "sh.601901"
        ; "sh.601066"
        ; "sh.601881"
        ; "sz.002500"
        ; "sh.600999"
        ; "sz.002673"
        ; "sh.601211"
        ; "sh.600958"
        ; "sz.002926"
        ; "sh.601375"
        ; "sh.600864"
        ; "sh.601696"
        ; "sz.000728"
        ; "sz.002939"
        ; "sh.600061"
        ; "sh.600621"
        ; "sh.601198"
        ; "sz.002670"
        ]
    }
  ; { category = "bank"
    ; codes =
        [ "sh.601398"
        ; "sh.601288"
        ; "sh.601818"
        ; "sz.000001"
        ; "sh.601328"
        ; "sz.002958"
        ; "sh.601988"
        ; "sh.600016"
        ; "sh.600036"
        ; "sh.601939"
        ; "sh.601166"
        ; "sh.601916"
        ; "sh.601658"
        ; "sh.601860"
        ; "sh.600139"
        ; "sz.002839"
        ; "sh.601169"
        ; "sh.600000"
        ; "sh.601009"
        ; "sh.601077"
        ; "sh.600919"
        ; "sz.002142"
        ; "sh.600908"
        ; "sh.601128"
        ; "sh.601998"
        ; "sh.600926"
        ; "sz.002807"
        ; "sh.600015"
        ; "sh.600901"
        ; "sz.002936"
        ; "sh.601229"
        ; "sh.601838"
        ; "sh.603323"
        ; "sh.601997"
        ; "sz.002966"
        ; "sh.600928"
        ; "sh.601577"
        ; "sz.002948"
        ]
    }
  ; { category = "internet"
    ; codes =
        [ "sz.002425"
        ; "sz.300315"
        ; "sz.002555"
        ; "sh.603444"
        ; "sz.002624"
        ; "sz.300418"
        ; "sh.600633"
        ; "sz.002235"
        ; "sz.002131"
        ; "sz.002174"
        ; "sh.601360"
        ; "sh.603881"
        ; "sz.002439"
        ; "sz.002517"
        ; "sh.600986"
        ; "sz.300031"
        ; "sh.600804"
        ; "sz.002464"
        ; "sz.300242"
        ; "sz.300773"
        ; "sh.600070"
        ; "sh.603613"
        ; "sh.603258"
        ; "sh.603888"
        ; "sz.002467"
        ; "sz.300459"
        ; "sz.300148"
        ; "sh.603000"
        ; "sz.300113"
        ; "sz.002803"
        ; "sz.000503"
        ; "sz.002530"
        ; "sz.002605"
        ; "sh.603825"
        ; "sz.002315"
        ; "sz.002558"
        ; "sz.000676"
        ; "sz.002168"
        ; "sz.300494"
        ; "sh.600640"
        ]
    }
  ; { category = "Food industry"
    ; codes =
        [ "sh.600873"
        ; "sh.600887"
        ; "sz.002570"
        ; "sz.002481"
        ; "sz.000716"
        ; "sh.600073"
        ; "sz.300401"
        ; "sz.002495"
        ; "sh.603027"
        ; "sz.002770"
        ; "sz.002626"
        ; "sh.600597"
        ; "sh.600866"
        ; "sh.600305"
        ; "sz.300146"
        ; "sz.002650"
        ; "sh.600186"
        ; "sz.002329"
        ; "sz.002507"
        ; "sz.002216"
        ; "sh.600429"
        ; "sh.603697"
        ; "sz.002661"
        ; "sh.603079"
        ; "sh.603299"
        ; "sz.002910"
        ; "sh.600298"
        ; "sz.002946"
        ; "sh.600381"
        ; "sh.600872"
        ; "sh.603696"
        ; "sh.600929"
        ; "sz.300858"
        ; "sh.603020"
        ; "sh.603043"
        ; "sh.603317"
        ; "sh.600419"
        ; "sh.603866"
        ; "sh.603886"
        ; "sz.002053"
        ]
    }
  ; { category = "insurance"
    ; codes =
        [ "sh.600291"
        ; "sh.601318"
        ; "sh.601319"
        ; "sh.601336"
        ; "sh.601601"
        ; "sh.601628"
        ; "sz.000627"
        ]
    }
  ; { category = "beverage"
    ; codes =
        [ "sh.600519"
        ; "sz.000858"
        ; "sz.000568"
        ; "sz.000799"
        ; "sz.002304"
        ; "sh.600809"
        ; "sz.000860"
        ; "sh.600600"
        ; "sh.600300"
        ; "sz.000596"
        ; "sz.002568"
        ; "sh.600779"
        ; "sz.000848"
        ; "sh.600559"
        ; "sh.603589"
        ; "sh.603369"
        ; "sh.600702"
        ; "sh.600197"
        ; "sh.600132"
        ; "sz.002461"
        ; "sh.603198"
        ; "sh.600059"
        ; "sz.000729"
        ; "sz.000869"
        ; "sh.600199"
        ; "sh.603156"
        ; "sh.603919"
        ; "sh.600189"
        ; "sh.603711"
        ; "sh.600365"
        ; "sh.600962"
        ; "sh.600616"
        ; "sh.600084"
        ; "sz.000929"
        ; "sh.600573"
        ; "sz.002646"
        ; "sh.603779"
        ; (* "sz.200596"; *)
          "sh.601579"
        ; "sh.600543"
        ]
    }
  ; { category = "pharmaceutical manufacturing"
    ; codes =
        [ "sh.600196"
        ; "sz.300122"
        ; "sz.002030"
        ; "sz.300142"
        ; "sz.002007"
        ; "sh.600211"
        ; "sh.600276"
        ; "sz.300601"
        ; "sz.000661"
        ; "sh.600085"
        ; "sh.600161"
        ; "sh.600812"
        ; "sz.300841"
        ; "sz.002019"
        ; "sh.603976"
        ; "sh.603718"
        ; "sz.002581"
        ; "sz.000513"
        ; "sz.000518"
        ; "sh.600267"
        ; "sh.688180"
        ; "sh.600079"
        ; "sz.002001"
        ; "sz.002287"
        ; "sh.600056"
        ; "sh.603392"
        ; "sz.002252"
        ; "sh.600201"
        ; "sz.002603"
        ; "sh.688399"
        ; "sz.300009"
        ; "sz.000538"
        ; "sz.002022"
        ; "sh.600332"
        ; "sh.600380"
        ; "sz.300558"
        ; "sz.000908"
        ; "sz.300497"
        ; "sh.600521"
        ; "sz.002793"
        ]
    }
  ; { category = "agriculture"
    ; codes =
        [ "sz.002385"
        ; "sh.600127"
        ; "sh.600438"
        ; "sz.000505"
        ; "sz.002157"
        ; "sh.601952"
        ; "sz.002124"
        ; "sh.600811"
        ; "sz.002702"
        ; "sh.600737"
        ; "sz.002286"
        ; "sz.002548"
        ; "sh.600095"
        ; "sh.600251"
        ; "sz.002515"
        ; "sz.002567"
        ; "sz.002582"
        ; "sz.002726"
        ; "sz.002100"
        ; "sz.000876"
        ; "sz.000639"
        ; "sz.300175"
        ; "sh.603609"
        ; "sh.600191"
        ; "sh.600275"
        ; "sz.000702"
        ; "sz.000895"
        ; "sh.603668"
        ; "sz.002991"
        ; "sz.000529"
        ; "sh.603363"
        ; "sz.002330"
        ; "sz.002311"
        ; "sz.002695"
        ; "sh.603336"
        ; "sz.300138"
        ; "sz.002852"
        ; "sz.002557"
        ; "sz.002840"
        ; "sh.603517"
        ]
    }
  ; { category = "航空运输业"
    ; codes =
        [ "sh.600004"
        ; "sh.600009"
        ; "sh.600029"
        ; "sh.600115"
        ; "sh.600221"
        ; "sh.600897"
        ; "sh.601021"
        ; "sh.601111"
        ; "sh.603885"
        ; "sz.000089"
        ; "sz.000099"
        ; "sz.002928"
        ]
    }
  ]

let all_codes =
  List.map get_industry_list ~f:(fun e -> e.codes)
  |> List.concat |> List.stable_dedup
