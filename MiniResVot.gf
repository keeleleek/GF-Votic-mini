resource MiniResVot = open Prelude, MorphoVot in {

param
  Number = Sg | Pl ;
  Case = nominative | genitive | partitive | illative | inessive | elative | allative | adessive | ablative | translative | terminative | comitative ;
  Person = Per1 | Per2 | Per3 ;

  Agreement = Agr Number Person ;

  VForm = Inf | PresSg3 ;

--  NForm = NF Number Case ; -- @TODO remove this later
  
oper
  --  Noun : Type = {s : Number => Case => Str} ;
--  Noun : Type = {s : NForm => Str} ;

--   mkNoun : Str -> Str -> Noun = \sg,pl -> {
--     s = table {Sg => sg ; Pl => pl}
--     } ;
-- 
--   regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;
-- 
--   -- smart paradigm
--   smartNoun : Str -> Noun = \sg -> case sg of {
--     _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
--     x + "y" => mkNoun sg (x + "ies") ;
--     _ + ("ch"|"sh"|"s"|"o") => mkNoun sg (sg + "es") ;
--     _       => regNoun sg
--     } ;
-- 
--   mkN = overload {
--    mkN : Str -> Noun = smartNoun ;
--    mkN : Str -> Str -> Noun = mkNoun ;
--    } ;

--   mkOmõn : Str -> Noun = \omõn -> 
--     case omõn of {
--       omõn => mkOmõnConcrete omõn ;
--       _ => Predef.error "Unsuitable lemma for mkOmõn"
--     } ;
-- 
--   mkOmõnConcrete : Str -> Noun = \omõn -> 
--     { s = 
--       table {
--         NCase Sg nominative => omõn ;
--         NCase Pl nominative => omõn + "ad" ;
--         NCase Sg genitive => omõn + "a" ;
--         NCase Pl genitive => omõn + "oi" ;
--         NCase Pl genitive => omõn + "ojõ" ;
--         NCase Sg partitive => omõn + "a" ;
--         NCase Sg partitive => omõn + "aa" ;
--         NCase Pl partitive => omõn + "oi" ;
--         NCase Pl partitive => omõn + "oitõ" ;
--         NCase Sg illative => omõn + "aa" ;
--         NCase Sg illative => omõn + "asõ" ;
--         NCase Pl illative => omõn + "oisõ" ;
--         NCase Sg inessive => omõn + "õz" ;
--         NCase Pl inessive => omõn + "oiz" ;
--         NCase Sg elative => omõn + "õssõ" ;
--         NCase Pl elative => omõn + "oissõ" ;
--         NCase Sg allative => omõn + "õllõ" ;
--         NCase Pl allative => omõn + "oillõ" ;
--         NCase Sg adessive => omõn + "õl" ;
--         NCase Pl adessive => omõn + "oil" ;
--         NCase Sg ablative => omõn + "õltõ" ;
--         NCase Pl ablative => omõn + "oiltõ" ;
--         NCase Sg translative => omõn + "õssi" ;
--         NCase Pl translative => omõn + "oissi" ;
--         NCase Sg terminative => omõn + "assaa" ;
--         NCase Pl terminative => omõn + "oissaa" ;
--         NCase Sg comitative => omõn + "aka" ;
--         NCase Pl comitative => omõn + "oika"
--       }
--     } ;

  
  ProperName : Type = {s : Str} ;

  mkPN : Str -> ProperName = \s -> {s = s} ;

  Adjective : Type = {s : Str} ;

  mkA : Str -> Adjective = \s -> {s = s} ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres : Str) -> Verb = \inf,pres -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres
      }
    } ;

  smartVerb : Str -> Verb = \inf ->
     mkVerb inf inf ;
 
  mkV = overload {
    mkV : Str -> Verb = smartVerb ;
    mkV : (inf,pres : Str) -> Verb = mkVerb ;
    } ;

  Verb2 : Type = Verb ** {c : Str} ;

  mkV2 = overload {
    mkV2 : Str         -> Verb2 = \s   -> mkV s ** {c = []} ;
    mkV2 : Str  -> Str -> Verb2 = \s,p -> mkV s ** {c = p} ;
    mkV2 : Verb        -> Verb2 = \v   -> v ** {c = []} ;
    mkV2 : Verb -> Str -> Verb2 = \v,p -> v ** {c = p} ;
    } ;

  Adverb : Type = {s : Str} ;

  mkAdv : Str -> Adverb = \s -> {s = s} ;

  be_GVerb : GVerb = {
     s = table {
       PresSg1 => "am" ;
       PresPl  => "are" ;
       VF vf   => (mkVerb "be" "is").s ! vf
       } ;
     isAux = True
     } ;

  GVerb : Type = {
     s : GVForm => Str ;
     isAux : Bool
     } ;

 param
   GVForm = VF VForm | PresSg1 | PresPl ;

 oper
   verb2gverb : Verb -> GVerb = \v -> {s =
     table {
       PresSg1 => v.s ! Inf ;
       PresPl  => v.s ! Inf ;
       VF vf   => v.s ! vf
       } ;
     isAux = False
     } ;

}