resource MiniResVot = MorphoVot ** open Prelude in {

param
  Person = Per1 | Per2 | Per3 ;

  Agreement = Agr Number Person ;

  VForm = Presn Number Person | Imp ;

oper
  ProperName : Type = {s : Str} ;

  mkPN : Str -> ProperName = \s -> {s = s} ;

  Adjective : Type = {s : NForm => Str};

  --mkA : Str -> Adjective = \s -> {s = s} ;

  Verb : Type = {s : VForm => Str ; isAux : Bool} ;

  getNum : Agreement -> Number = \a ->
    case a of { Agr n p => n } ;
  
  mkRegularVerb : (imp : Str) -> Verb = \imp -> {
    s = table {
      Presn Sg Per1 => imp + "n" ;
      Presn Sg Per2 => imp + "d" ;
      Presn Sg Per3 => imp + "b" ;
      Presn Pl Per1 => imp + "mme" ;
      Presn Pl Per2 => imp + "ttõ" ;
      Presn Pl Per3 => imp + "ta" ;
      Imp => imp 
      } ;
    isAux = False ;
    } ;
  
  --smartVerb : Str -> Verb = \inf ->
  --   mkVerb inf inf ;

  mkV = overload {
    mkV : Str -> Verb = mkRegularVerb ;
    --mkV : (inf,pres : Str) -> Verb = mkVerb ;
    } ;
  
  Verb2 : Type = Verb ** {hasRect : Case} ;

  mkV2 = overload {
    --mkV2 : Str         -> Verb2 = \s   -> mkV s ** {c = []} ;
    --mkV2 : Str  -> Str -> Verb2 = \s,p -> mkV s ** {c = p} ;
    mkV2 : Verb         -> Verb2 = \v   -> v ** {hasRect = partitive} ; -- default rection is partitive
    mkV2 : Verb -> Case -> Verb2 = \v,c -> v ** {hasRect = c} ;
    } ;

  Adverb : Type = {s : Str} ;

  mkAdv : Str -> Adverb = \s -> {s = s} ;

  be_Verb : Verb = {
    s = table {
      Presn Sg Per1 => "õõn" ;
      Presn Sg Per2 => "õõd" ;
      Presn Sg Per3 => "on" ;
      Presn Pl Per1 => "õõmme" ;
      Presn Pl Per2 => "õõttõ" ;
      Presn Pl Per3 => "õlla" ;
      Imp => "õõ" 
      } ;
    isAux = True ;
    } ;

    neg_Verb : Verb = {
    s = table {
      Presn Sg Per1 => "en" ;
      Presn Sg Per2 => "ed" ;
      Presn Sg Per3 => "eb" ;
      Presn Pl Per1 => "emme" ;
      Presn Pl Per2 => "ette" ;
      Presn Pl Per3 => "eväd" ;
      Imp => []
      } ;
    isAux = False ;
    } ;
  
}