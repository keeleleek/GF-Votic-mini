concrete MiniGrammarVot of MiniGrammar = open MiniResVot, Prelude in {

  lincat
    Utt = {s : Str} ;
    Adv = Adverb ;
    Pol = {s : Str ; b : Bool} ;
    
    S  = {s : Str} ;
    Cl = {s : Bool => Str} ;
    VP = {verb : Verb ; compl : Number => Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP   = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det  = {s : Case => Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = {s : Str ; hasRect : Case} ;
    V  = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = ProperName ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! partitive} ;

    UsePresCl pol cl = {
      s = pol.s ++ cl.s ! pol.b
      } ;

    PredVP np vp = {
      s = \\pol =>
           np.s ! nominative 
	++ case <pol, np.a, vp.verb.isAux> of {
	    <True,  Agr num per, _> => vp.verb.s  ! Presn num per ;
	    <False, Agr num per, _> => neg_Verb.s ! Presn num per ++ vp.verb.s ! Imp
	}
	++ vp.compl ! getNum np.a
      } ;

    
    UseV v = {
      verb = v ;
      compl = table {Sg => [] ; Pl => []}
      } ;
    ComplV2 v2 np = {
      verb = v2 ;
      compl = table { _ => np.s ! v2.hasRect }
      } ;
    UseAP ap = {
      verb = be_Verb ;
      compl = 
	table {
	  Sg => ap.s ! NF Sg nominative ;
	  Pl => ap.s ! NF Pl nominative
	} 
      } ;
    AdvVP vp adv =
      vp ** { compl = table { num => vp.compl ! num ++ adv.s } } ;
      
    DetCN det cn = {
      s = table {c => det.s ! c ++ cn.s ! NF det.n c} ;
      a = Agr det.n Per3
      } ;
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
    UsePron p =
      p ;
    MassNP cn = {
      s = \\c => cn.s ! NF Sg c ;
      a = Agr Sg Per3
      } ;

    a_Det = {s = table { _ => "" } ;
	     n = Sg} ;

    aPl_Det = {s = table {
		 nominative => "" ;
		 genitive => "" ;
		 partitive => "" ;
		 illative => "" ;
		 inessive => "" ;
		 elative => "" ;
		 allative => "" ;
		 adessive => "" ;
		 ablative => "" ;
		 terminative => "" ;
		 translative => "" ;
		 comitative => ""
		 } ; n = Pl} ;

    the_Det = {s = table {
		 nominative => "see" ;
		 genitive => "sene" ;
		 partitive => "sitä" ;
		 illative => "sihe" ;
		 inessive => "senez" ;
		 elative => "seness" ;
		 allative => "selle" ;
		 adessive => "sell" ;
		 ablative => "selt" ;
		 terminative => "senessaa" ;
		 translative => "senessi" ;
		 comitative => "seneka"
		 } ; n = Sg} ;

    thePl_Det = {s = table {
		 nominative => "nee" | "need" ;
		 genitive => "nejje" ;
		 partitive => "neit" ;
		 illative => "neise" ;
		 inessive => "neiz" ;
		 elative => "neiss" ;
		 allative => "neille" ;
		 adessive => "neill" ;
		 ablative => "neilt" ;
		 terminative => "nejjessaa" ;
		 translative => "neissi" ;
		 comitative => "nejjeka"
		 } ; n = Pl} ;

    UseN n = n ;

    AdjCN ap cn = {
      s = table {n => ap.s ! n ++ cn.s ! n}
      } ;
    
    PositA a = a ;

    PrepNP prep np = {s = np.s ! prep.hasRect ++ prep.s} ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = [] ; b = True} ;
    PNeg  = {s = [] ; b = False} ;

    and_Conj = {s = "i"} ;
    or_Conj = {s = "vai"} ;

    every_Det = {s = \\c => (mkKõikk "kõikk").s ! NF Sg c ; n = Pl} ;

    in_Prep = {s = "süämez" ; hasRect = genitive } ; -- TODO change s to table with NForms
    on_Prep = {s = "pääll" ; hasRect = genitive } ;
    with_Prep = {s = "kaasa" ; hasRect = comitative } ;

    i_Pron = {
      s = table {
        nominative => "miä" ;
        genitive => "minu" ;
        partitive => "minnua" ;
        illative => "minnu" | "minusõ" ;
        inessive => "minuz" ;
        elative => "minuss" ;
        allative => "millõ" ;
        adessive => "mill" ;
        ablative => "milt" ;
        translative => "minussi" ;
        terminative => "minussaa" ;
        comitative => "minuka"
	} ;
      a = Agr Sg Per1
      } ;

    youSg_Pron = {
      s = table {
        nominative => "siä" ;
        genitive => "sinu" ;
        partitive => "sinnua" ;
        illative => "sinnu" | "sinusõ" ;
        inessive => "sinuz" ;
        elative => "sinuss" ;
        allative => "sillõ" ;
        adessive => "sill" ;
        ablative => "silt" ;
        translative => "sinussi" ;
        terminative => "sinussaa" ;
        comitative => "sinuka"
	} ;
      a = Agr Sg Per2
      } ;
    
    he_Pron = {
      s = table {
        nominative => "tämä" ;
        genitive => "tämä" ;
        partitive => "tätä" ;
        illative => "tämäse" ;
        inessive => "tämäz" ;
        elative => "tämäss" ;
        allative => "tämälle" ;
        adessive => "tämäll" ;
        ablative => "tämält" ;
        translative => "tämässi" ;
        terminative => "tämässaa" ;
        comitative => "tämäka"
	} ;
      a = Agr Sg Per3
      } ;
    
    she_Pron = he_Pron ;
    
    we_Pron = {
      s = table {
        nominative => "müü" ;
        genitive => "mejje" ;
        partitive => "meit" | "meite" ;
        illative => "meise" ;
        inessive => "meiz" ;
        elative => "meiss" ;
        allative => "meille" ;
        adessive => "meill" ;
        ablative => "meilt" ;
        translative => "meissi" ;
        terminative => "mejjessaa" ;
        comitative => "mejjeka"
	} ;
      a = Agr Pl Per1
      } ;
    
    youPl_Pron = {
      s = table {
        nominative => "tüü" ;
        genitive => "tejje" ;
        partitive => "teit" | "teite" ;
        illative => "teise" ;
        inessive => "teiz" ;
        elative => "teiss" ;
        allative => "teille" ;
        adessive => "teill" ;
        ablative => "teilt" ;
        translative => "teissi" ;
        terminative => "tejjessaa" ;
        comitative => "tejjeka"
	} ;
      a = Agr Pl Per2
      } ;
    
    they_Pron = {
      s = table {
        nominative => "nämä" | "nämäd" ;
        genitive => "näjje" ;
        partitive => "näit" | "näite" ;
        illative => "näise" ;
        inessive => "näiz" ;
        elative => "näiss" ;
        allative => "näille" ;
        adessive => "näill" ;
        ablative => "näilt" ;
        translative => "näissi" ;
        terminative => "näjjessaa" ;
        comitative => "näjjeka"
	} ;
      a = Agr Pl Per3
      } ;

}
