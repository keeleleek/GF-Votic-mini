resource MiniResVot = MorphoVot ** open Prelude in {

param
  Person = Per1 | Per2 | Per3 ;

  Agreement = Agr Number Person ;

  VForm = Presn Number Person | Imp ;

oper
  ProperName : Type = {s : Str} ;

  mkPN : Str -> ProperName = \s -> {s = s} ;

  Adjective : Type = {s : NForm => Str};

  -- mkA : Str -> Adjective = \s -> {s = s} ;

  Verb : Type = {s : VForm => Str ; isAux : Bool} ;

  -- mkVerb : (inf,pres : Str) -> Verb = \inf,pres -> {
  --   s = table {
  --     Inf => inf ;
  --     PresSg3 => pres
  --     }
  --   } ;
  -- 
  -- smartVerb : Str -> Verb = \inf ->
  --    mkVerb inf inf ;
  -- 
  -- mkV = overload {
  --   mkV : Str -> Verb = smartVerb ;
  --   mkV : (inf,pres : Str) -> Verb = mkVerb ;
  --   } ;
  --
  --mkV : { s = VForm => Str } -> Verb = \v -> {s = v.s ; isAux = False } ;
  
  Verb2 : Type = Verb ** {hasRect : Case} ;
  -- 
  mkV2 = overload {
  --   mkV2 : Str         -> Verb2 = \s   -> mkV s ** {c = []} ;
  --   mkV2 : Str  -> Str -> Verb2 = \s,p -> mkV s ** {c = p} ;
    mkV2 : Verb         -> Verb2 = \v   -> v ** {hasRect = partitive} ; -- default rection is partitive
    mkV2 : Verb -> Case -> Verb2 = \v,c -> v ** {hasRect = c} ;
  --   mkV2 : Verb -> Str -> Verb2 = \v,p -> v ** {c = p} ;
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