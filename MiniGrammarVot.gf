concrete MiniGrammarVot of MiniGrammar = open MiniResVot, Prelude in {


  lincat
    Utt = {s : Str} ;
    Adv = Adverb ;
    Pol = {s : Str ; b : Bool} ;
    
    S  = {s : Str} ;
    Cl = {s : Bool => Str} ;
    VP = {verb : GVerb ; compl : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
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
      s = \\b =>
           np.s ! nominative 
	++ case <b, np.a, vp.verb.isAux> of {
	    <True, Agr Sg Per1,_> => vp.verb.s ! PresSg1 ;
	    <True, Agr Sg Per3,_> => vp.verb.s ! VF PresSg3 ;
	    <True, _          ,_> => vp.verb.s ! PresPl ;
	    <False, Agr Sg Per1,True>  => vp.verb.s ! PresSg1 ++ "not" ;
	    <False, Agr Sg Per3,True>  => vp.verb.s ! VF PresSg3 ++ "not" ;
	    <False, _          ,True>  => vp.verb.s ! PresPl ++ "not" ;
	    <False, Agr Sg Per3,False> => "does not" ++ vp.verb.s ! VF Inf ;
	    <False, _          ,False> => "do not" ++ vp.verb.s ! VF Inf
	    }
        ++ vp.compl ;
      } ;
      
    UseV v = {
      verb = verb2gverb v ;
      compl = []
      } ;
    ComplV2 v2 np = {
      verb = verb2gverb v2 ;
      compl = v2.c ++ np.s ! partitive
      } ;
    UseAP ap = {
      verb = be_GVerb ;
      compl = ap.s
      } ;
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = table {c => det.s ++ cn.s ! det.n} ;
      a = Agr det.n Per3
      } ;
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
    UsePron p =
      p ;
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = Agr Sg Per3
      } ;
    a_Det = {s = "" ; n = Sg} ;
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "tämä" ; n = Sg} ;
    thePl_Det = {s = "nämäd" ; n = Pl} ;
    UseN n =
      n ;
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! partitive} ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = [] ; b = True} ;
    PNeg  = {s = [] ; b = False} ;

    and_Conj = {s = "i"} ;
    or_Conj = {s = "vai"} ;

    every_Det = {s = "every" ; n = Sg} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

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
      a = Agr Pl Per2
      } ;

}
