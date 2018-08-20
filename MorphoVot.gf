resource MorphoVot = {

param
  Number = Sg | Pl ;
  Case = nominative | genitive | partitive | illative | inessive | elative | allative | adessive | ablative | translative | terminative | comitative ;
  NForm = NF Number Case ;

oper
  Noun : Type = {s : NForm => Str} ;

------------------------------------------------
-- Start of Noun section
------------------------------------------------

  mkAapõ : Str -> Noun = \aapõ -> 
    case aapõ of {
      aa + "põ" => mkAapõConcrete aa ;
      _ => Predef.error "Unsuitable lemma for mkAapõ"
    } ;

  mkAapõConcrete : Str -> Noun = \aa -> 
    { s =
      table {
        NF Sg nominative => aa + "põ" ;
        NF Pl nominative => aa + "võd" ;
        NF Sg genitive => aa + "va" ;
        NF Pl genitive => aa + "poi" ;
        NF Pl genitive => aa + "pojõ" ;
        NF Sg partitive => aa + "pa" ;
        NF Pl partitive => aa + "poi" ;
        NF Pl partitive => aa + "poitõ" ;
        NF Sg illative => aa + "paa" ;
        NF Sg illative => aa + "pasõ" ;
        NF Pl illative => aa + "poisõ" ;
        NF Sg inessive => aa + "vaz" ;
        NF Pl inessive => aa + "voiz" ;
        NF Sg elative => aa + "võssõ" ;
        NF Pl elative => aa + "poissõ" ;
        NF Sg allative => aa + "võllõ" ;
        NF Pl allative => aa + "poillõ" ;
        NF Sg adessive => aa + "võl" ;
        NF Pl adessive => aa + "poil" ;
        NF Sg ablative => aa + "võssi" ;
        NF Pl ablative => aa + "poissi" ;
        NF Sg translative => aa + "passi" ;
        NF Pl translative => aa + "poissi" ;
        NF Sg terminative => aa + "passaa" ;
        NF Pl terminative => aa + "poissaa" ;
        NF Sg comitative => aa + "vaka" ;
        NF Pl comitative => aa + "poika"
      }
    } ;


  mkSinin : Str -> Noun = \sinin -> 
    case sinin of {
      sini + "n" => mkSininConcrete sini ;
      _ => Predef.error "Unsuitable lemma for mkSinin"
    } ;

  mkSininConcrete : Str -> Noun = \sini -> 
    { s =
      table {
        NF Sg nominative => sini + "n" ;
        NF Pl nominative => sini + "zed" ;
        NF Sg genitive => sini + "ze" ;
        NF Pl genitive => sini + "zije" ;
        NF Sg partitive => sini + "sse" ;
        NF Pl partitive => sini + "zii" ;
        NF Pl partitive => sini + "ziit" ;
        NF Sg illative => sini + "ze" ;
        NF Sg illative => sini + "zese" ;
        NF Pl illative => sini + "zije" ;
        NF Pl illative => sini + "zise" ;
        NF Sg inessive => sini + "zez" ;
        NF Pl inessive => sini + "ziz" ;
        NF Sg elative => sini + "zess" ;
        NF Pl elative => sini + "ziss" ;
        NF Sg allative => sini + "zelle" ;
        NF Pl allative => sini + "zille" ;
        NF Sg adessive => sini + "zell" ;
        NF Pl adessive => sini + "zill" ;
        NF Sg ablative => sini + "zelt" ;
        NF Pl ablative => sini + "zilt" ;
        NF Sg translative => sini + "zessi" ;
        NF Pl translative => sini + "zissi" ;
        NF Sg terminative => sini + "zeessaa" ;
        NF Pl terminative => sini + "ziissaa" ;
        NF Sg comitative => sini + "zeka" ;
        NF Pl comitative => sini + "zika"
      }
    } ;


  mkAikõ : Str -> Noun = \aikõ -> 
    case aikõ of {
      ai + "kõ" => mkAikõConcrete ai ;
      _ => Predef.error "Unsuitable lemma for mkAikõ"
    } ;

  mkAikõConcrete : Str -> Noun = \ai -> 
    { s =
      table {
        NF Sg nominative => ai + "kõ" ;
        NF Pl nominative => ai + "gõd" ;
        NF Sg genitive => ai + "ga" ;
        NF Pl genitive => ai + "koi" ;
        NF Pl genitive => ai + "kojõ" ;
        NF Sg partitive => ai + "ka" ;
        NF Sg partitive => ai + "kaa" ;
        NF Pl partitive => ai + "koi" ;
        NF Pl partitive => ai + "koitõ" ;
        NF Sg illative => ai + "ka" ;
        NF Sg illative => ai + "kasõ" ;
        NF Pl illative => ai + "koisõ" ;
        NF Sg inessive => ai + "gõz" ;
        NF Pl inessive => ai + "koiz" ;
        NF Sg elative => ai + "gõssõ" ;
        NF Pl elative => ai + "koissõ" ;
        NF Sg allative => ai + "gõllõ" ;
        NF Pl allative => ai + "koillõ" ;
        NF Sg adessive => ai + "gõl" ;
        NF Pl adessive => ai + "koil" ;
        NF Sg ablative => ai + "gõltõ" ;
        NF Pl ablative => ai + "koiltõ" ;
        NF Sg translative => ai + "gõssi" ;
        NF Pl translative => ai + "koissi" ;
        NF Sg terminative => ai + "kassaa" ;
        NF Pl terminative => ai + "koissaa" ;
        NF Sg comitative => ai + "gaka" ;
        NF Pl comitative => ai + "koika"
      }
    } ;


  mkPoikõ : Str -> Noun = \poikõ -> 
    case poikõ of {
      poi + "kõ" => mkPoikõConcrete poi ;
      _ => Predef.error "Unsuitable lemma for mkPoikõ"
    } ;

  mkPoikõConcrete : Str -> Noun = \poi -> 
    { s =
      table {
        NF Sg nominative => poi + "kõ" ;
        NF Pl nominative => poi + "gõd" ;
        NF Sg genitive => poi + "ga" ;
        NF Pl genitive => poi + "ki" ;
        NF Pl genitive => poi + "kije" ;
        NF Sg partitive => poi + "ka" ;
        NF Sg partitive => poi + "kaa" ;
        NF Pl partitive => poi + "ki" ;
        NF Pl partitive => poi + "kitõ" ;
        NF Sg illative => poi + "kaa" ;
        NF Sg illative => poi + "kasõ" ;
        NF Pl illative => poi + "ki" ;
        NF Pl illative => poi + "kisõ" ;
        NF Sg inessive => poi + "gõz" ;
        NF Pl inessive => poi + "kiz" ;
        NF Sg elative => poi + "gõssõ" ;
        NF Pl elative => poi + "kissõ" ;
        NF Sg allative => poi + "gõllõ" ;
        NF Pl allative => poi + "killõ" ;
        NF Sg adessive => poi + "gõl" ;
        NF Pl adessive => poi + "kil" ;
        NF Sg ablative => poi + "gõltõ" ;
        NF Pl ablative => poi + "kiltõ" ;
        NF Sg translative => poi + "gõssi" ;
        NF Pl translative => poi + "kissi" ;
        NF Sg terminative => poi + "kassaa" ;
        NF Pl terminative => poi + "kissaa" ;
        NF Sg comitative => poi + "gaka" ;
        NF Pl comitative => poi + "kika"
      }
    } ;


  mkAmmõz : Str -> Noun = \ammõz -> 
    case ammõz of {
      am + "mõz" => mkAmmõzConcrete am ;
      _ => Predef.error "Unsuitable lemma for mkAmmõz"
    } ;

  mkAmmõzConcrete : Str -> Noun = \am -> 
    { s =
      table {
        NF Sg nominative => am + "mõz" ;
        NF Pl nominative => am + "pad" ;
        NF Sg genitive => am + "pa" ;
        NF Pl genitive => am + "paijõ" ;
        NF Sg partitive => am + "massõ" ;
        NF Pl partitive => am + "paitõ" ;
        NF Sg illative => am + "pasõ" ;
        NF Pl illative => am + "paisõ" ;
        NF Sg inessive => am + "paz" ;
        NF Pl inessive => am + "paiz" ;
        NF Sg elative => am + "passõ" ;
        NF Pl elative => am + "paissõ" ;
        NF Sg allative => am + "pallõ" ;
        NF Pl allative => am + "paillõ" ;
        NF Sg adessive => am + "pal" ;
        NF Pl adessive => am + "pail" ;
        NF Sg ablative => am + "paltõ" ;
        NF Pl ablative => am + "pailtõ" ;
        NF Sg translative => am + "passi" ;
        NF Pl translative => am + "paissi" ;
        NF Sg terminative => am + "passaa" ;
        NF Pl terminative => am + "paissaa" ;
        NF Sg comitative => am + "paka" ;
        NF Pl comitative => am + "paika"
      }
    } ;


  mkAikõ : Str -> Noun = \aikõ -> 
    case aikõ of {
      ai + "kõ" => mkAikõConcrete ai ;
      _ => Predef.error "Unsuitable lemma for mkAikõ"
    } ;

  mkAikõConcrete : Str -> Noun = \ai -> 
    { s =
      table {
        NF Sg nominative => ai + "kõ" ;
        NF Pl nominative => ai + "gõd" ;
        NF Sg genitive => ai + "ga" ;
        NF Pl genitive => ai + "koi" ;
        NF Pl genitive => ai + "kojõ" ;
        NF Sg partitive => ai + "ka" ;
        NF Sg partitive => ai + "kaa" ;
        NF Pl partitive => ai + "koi" ;
        NF Pl partitive => ai + "koitõ" ;
        NF Sg illative => ai + "ka" ;
        NF Sg illative => ai + "kasõ" ;
        NF Pl illative => ai + "koisõ" ;
        NF Sg inessive => ai + "gõz" ;
        NF Pl inessive => ai + "koiz" ;
        NF Sg elative => ai + "gõssõ" ;
        NF Pl elative => ai + "koissõ" ;
        NF Sg allative => ai + "gõllõ" ;
        NF Pl allative => ai + "koillõ" ;
        NF Sg adessive => ai + "gõl" ;
        NF Pl adessive => ai + "koil" ;
        NF Sg ablative => ai + "gõltõ" ;
        NF Pl ablative => ai + "koiltõ" ;
        NF Sg translative => ai + "gõssi" ;
        NF Pl translative => ai + "koissi" ;
        NF Sg terminative => ai + "kassaa" ;
        NF Pl terminative => ai + "koissaa" ;
        NF Sg comitative => ai + "gaka" ;
        NF Pl comitative => ai + "koika"
      }
    } ;


  mkKoirõ : Str -> Noun = \koirõ -> 
    case koirõ of {
      koir + "õ" => mkKoirõConcrete koir ;
      _ => Predef.error "Unsuitable lemma for mkKoirõ"
    } ;

  mkKoirõConcrete : Str -> Noun = \koir -> 
    { s =
      table {
        NF Sg nominative => koir + "õ" ;
        NF Pl nominative => koir + "õd" ;
        NF Sg genitive => koir + "a" ;
        NF Pl genitive => koir + "i" ;
        NF Pl genitive => koir + "ije" ;
        NF Sg partitive => koir + "a" ;
        NF Sg partitive => koir + "aa" ;
        NF Pl partitive => koir + "i" ;
        NF Pl partitive => koir + "itõ" ;
        NF Sg illative => koir + "aa" ;
        NF Sg illative => koir + "asõ" ;
        NF Pl illative => koir + "i" ;
        NF Pl illative => koir + "isõ" ;
        NF Sg inessive => koir + "õz" ;
        NF Pl inessive => koir + "iz" ;
        NF Sg elative => koir + "õssõ" ;
        NF Pl elative => koir + "issõ" ;
        NF Sg allative => koir + "õllõ" ;
        NF Pl allative => koir + "illõ" ;
        NF Sg adessive => koir + "õl" ;
        NF Pl adessive => koir + "il" ;
        NF Sg ablative => koir + "õltõ" ;
        NF Pl ablative => koir + "iltõ" ;
        NF Sg translative => koir + "õssi" ;
        NF Pl translative => koir + "issi" ;
        NF Sg terminative => koir + "assaa" ;
        NF Pl terminative => koir + "issaa" ;
        NF Sg comitative => koir + "aka" ;
        NF Pl comitative => koir + "ika"
      }
    } ;


  mkLentüz : Str -> Noun = \lentüz -> 
    case lentüz of {
      lentü + "z" => mkLentüzConcrete lentü ;
      _ => Predef.error "Unsuitable lemma for mkLentüz"
    } ;

  mkLentüzConcrete : Str -> Noun = \lentü -> 
    { s =
      table {
        NF Sg nominative => lentü + "z" ;
        NF Pl nominative => lentü + "sed" ;
        NF Sg genitive => lentü + "se" ;
        NF Pl genitive => lentü + "si" ;
        NF Sg partitive => lentü + "sse" ;
        NF Pl partitive => lentü + "ssi" ;
        NF Sg illative => lentü + "sesse" ;
        NF Pl illative => lentü + "sisse" ;
        NF Sg inessive => lentü + "sez" ;
        NF Pl inessive => lentü + "siz" ;
        NF Sg elative => lentü + "sse" ;
        NF Pl elative => lentü + "sissõ" ;
        NF Sg allative => lentü + " gõllõ" ;
        NF Pl allative => lentü + " koillõ" ;
        NF Sg adessive => lentü + " gõl" ;
        NF Pl adessive => lentü + " koil" ;
        NF Sg ablative => lentü + " gõltõ" ;
        NF Pl ablative => lentü + " koiltõ" ;
        NF Sg translative => lentü + " gõssi" ;
        NF Pl translative => lentü + " koissi" ;
        NF Sg terminative => lentü + " kassaa" ;
        NF Pl terminative => lentü + " koissaa" ;
        NF Sg comitative => lentü + " gaka" ;
        NF Pl comitative => lentü + " koika"
      }
    } ;


  mkLuzikkõ : Str -> Noun = \luzikkõ -> 
    case luzikkõ of {
      luzik + "kõ" => mkLuzikkõConcrete luzik ;
      _ => Predef.error "Unsuitable lemma for mkLuzikkõ"
    } ;

  mkLuzikkõConcrete : Str -> Noun = \luzik -> 
    { s =
      table {
        NF Sg nominative => luzik + "kõ" ;
        NF Pl nominative => luzik + "õd" ;
        NF Sg genitive => luzik + "a" ;
        NF Pl genitive => luzik + "koi" ;
        NF Pl genitive => luzik + "kojõ" ;
        NF Sg partitive => luzik + "ka" ;
        NF Sg partitive => luzik + "kaa" ;
        NF Pl partitive => luzik + "koi" ;
        NF Pl partitive => luzik + "koitõ" ;
        NF Sg illative => luzik + "ka" ;
        NF Sg illative => luzik + "kasõ" ;
        NF Pl illative => luzik + "koisõ" ;
        NF Sg inessive => luzik + "õz" ;
        NF Pl inessive => luzik + "koiz" ;
        NF Sg elative => luzik + "õssõ" ;
        NF Pl elative => luzik + "koissõ" ;
        NF Sg allative => luzik + "gõllõ" ;
        NF Pl allative => luzik + "koillõ" ;
        NF Sg adessive => luzik + "õl" ;
        NF Pl adessive => luzik + "koil" ;
        NF Sg ablative => luzik + "õltõ" ;
        NF Pl ablative => luzik + "koiltõ" ;
        NF Sg translative => luzik + "õssi" ;
        NF Pl translative => luzik + "koissi" ;
        NF Sg terminative => luzik + "kassaa" ;
        NF Pl terminative => luzik + "koissaa" ;
        NF Sg comitative => luzik + "aka" ;
        NF Pl comitative => luzik + "koika"
      }
    } ;


  mkLuikko : Str -> Noun = \luikko -> 
    case luikko of {
      luik + "ko" => mkLuikkoConcrete luik ;
      _ => Predef.error "Unsuitable lemma for mkLuikko"
    } ;

  mkLuikkoConcrete : Str -> Noun = \luik -> 
    { s =
      table {
        NF Sg nominative => luik + "ko" ;
        NF Pl nominative => luik + "od" ;
        NF Sg genitive => luik + "o" ;
        NF Pl genitive => luik + "koi" ;
        NF Pl genitive => luik + "kojõ" ;
        NF Sg partitive => luik + "koa" ;
        NF Pl partitive => luik + "koitõ" ;
        NF Sg illative => luik + "kosõ" ;
        NF Pl illative => luik + "koisõ" ;
        NF Sg inessive => luik + "oz" ;
        NF Pl inessive => luik + "koiz" ;
        NF Sg elative => luik + "ossõ" ;
        NF Pl elative => luik + "koissõ" ;
        NF Sg allative => luik + "ollõ" ;
        NF Pl allative => luik + "koillõ" ;
        NF Sg adessive => luik + "ol" ;
        NF Pl adessive => luik + "koil" ;
        NF Sg ablative => luik + "oltõ" ;
        NF Pl ablative => luik + "koiltõ" ;
        NF Sg translative => luik + "ossi" ;
        NF Pl translative => luik + "koissi" ;
        NF Sg terminative => luik + "kassaa" ;
        NF Pl terminative => luik + "koissaa" ;
        NF Sg comitative => luik + "oka" ;
        NF Pl comitative => luik + "koika"
      }
    } ;


  mkAitõ : Str -> Noun = \aitõ -> 
    case aitõ of {
      ai + "tõ" => mkAitõConcrete ai ;
      _ => Predef.error "Unsuitable lemma for mkAitõ"
    } ;

  mkAitõConcrete : Str -> Noun = \ai -> 
    { s =
      table {
        NF Sg nominative => ai + "tõ" ;
        NF Pl nominative => ai + "jõd" ;
        NF Sg genitive => ai + "ja" ;
        NF Pl genitive => ai + "toi" ;
        NF Pl genitive => ai + "tojõ" ;
        NF Sg partitive => ai + "ta" ;
        NF Pl partitive => ai + "toi" ;
        NF Pl partitive => ai + "toitõ" ;
        NF Sg illative => ai + "taa" ;
        NF Sg illative => ai + "tasõ" ;
        NF Pl illative => ai + "toisõ" ;
        NF Sg inessive => ai + "jaz" ;
        NF Pl inessive => ai + "joiz" ;
        NF Sg elative => ai + "jõssõ" ;
        NF Pl elative => ai + "toissõ" ;
        NF Sg allative => ai + "jõllõ" ;
        NF Pl allative => ai + "toillõ" ;
        NF Sg adessive => ai + "jõl" ;
        NF Pl adessive => ai + "toil" ;
        NF Sg ablative => ai + "jõssi" ;
        NF Pl ablative => ai + "toissi" ;
        NF Sg translative => ai + "tassi" ;
        NF Pl translative => ai + "toissi" ;
        NF Sg terminative => ai + "tassaa" ;
        NF Pl terminative => ai + "toissaa" ;
        NF Sg comitative => ai + "jaka" ;
        NF Pl comitative => ai + "toika"
      }
    } ;


  mkAhkõrõ : Str -> Noun = \ahkõrõ -> 
    case ahkõrõ of {
      ahkõr + "õ" => mkAhkõrõConcrete ahkõr ;
      _ => Predef.error "Unsuitable lemma for mkAhkõrõ"
    } ;

  mkAhkõrõConcrete : Str -> Noun = \ahkõr -> 
    { s =
      table {
        NF Sg nominative => ahkõr + "õ" ;
        NF Pl nominative => ahkõr + "õd" ;
        NF Sg genitive => ahkõr + "a" ;
        NF Pl genitive => ahkõr + "oi" ;
        NF Sg partitive => ahkõr + "a" ;
        NF Pl partitive => ahkõr + "oi" ;
        NF Pl partitive => ahkõr + "oitõ" ;
        NF Sg illative => ahkõr + "aa" ;
        NF Sg illative => ahkõr + "asõ" ;
        NF Pl illative => ahkõr + "oisõ" ;
        NF Sg inessive => ahkõr + "az" ;
        NF Pl inessive => ahkõr + "oiz" ;
        NF Sg elative => ahkõr + "õssõ" ;
        NF Pl elative => ahkõr + "oissõ" ;
        NF Sg allative => ahkõr + "õllõ" ;
        NF Pl allative => ahkõr + "oillõ" ;
        NF Sg adessive => ahkõr + "õl" ;
        NF Pl adessive => ahkõr + "oil" ;
        NF Sg ablative => ahkõr + "õssi" ;
        NF Pl ablative => ahkõr + "oissi" ;
        NF Sg translative => ahkõr + "assi" ;
        NF Pl translative => ahkõr + "oissi" ;
        NF Sg terminative => ahkõr + "assaa" ;
        NF Pl terminative => ahkõr + "oissaa" ;
        NF Sg comitative => ahkõr + "aka" ;
        NF Pl comitative => ahkõr + "oika"
      }
    } ;


  mkOmõn : Str -> Noun = \omõn -> 
    case omõn of {
      omõn => mkOmõnConcrete omõn ;
      _ => Predef.error "Unsuitable lemma for mkOmõn"
    } ;

  mkOmõnConcrete : Str -> Noun = \omõn -> 
    { s =
      table {
        NF Sg nominative => omõn ;
        NF Pl nominative => omõn + "ad" ;
        NF Sg genitive => omõn + "a" ;
        NF Pl genitive => omõn + "oi" ;
        NF Pl genitive => omõn + "ojõ" ;
        NF Sg partitive => omõn + "a" ;
        NF Sg partitive => omõn + "aa" ;
        NF Pl partitive => omõn + "oi" ;
        NF Pl partitive => omõn + "oitõ" ;
        NF Sg illative => omõn + "aa" ;
        NF Sg illative => omõn + "asõ" ;
        NF Pl illative => omõn + "oisõ" ;
        NF Sg inessive => omõn + "õz" ;
        NF Pl inessive => omõn + "oiz" ;
        NF Sg elative => omõn + "õssõ" ;
        NF Pl elative => omõn + "oissõ" ;
        NF Sg allative => omõn + "õllõ" ;
        NF Pl allative => omõn + "oillõ" ;
        NF Sg adessive => omõn + "õl" ;
        NF Pl adessive => omõn + "oil" ;
        NF Sg ablative => omõn + "õltõ" ;
        NF Pl ablative => omõn + "oiltõ" ;
        NF Sg translative => omõn + "õssi" ;
        NF Pl translative => omõn + "oissi" ;
        NF Sg terminative => omõn + "assaa" ;
        NF Pl terminative => omõn + "oissaa" ;
        NF Sg comitative => omõn + "aka" ;
        NF Pl comitative => omõn + "oika"
      }
    } ;


  mkPliittõ : Str -> Noun = \pliittõ -> 
    case pliittõ of {
      pliit + "tõ" => mkPliittõConcrete pliit ;
      _ => Predef.error "Unsuitable lemma for mkPliittõ"
    } ;

  mkPliittõConcrete : Str -> Noun = \pliit -> 
    { s =
      table {
        NF Sg nominative => pliit + "tõ" ;
        NF Pl nominative => pliit + "õd" ;
        NF Sg genitive => pliit + "a" ;
        NF Pl genitive => pliit + "toi" ;
        NF Pl genitive => pliit + "tojõ" ;
        NF Sg partitive => pliit + "ta" ;
        NF Pl partitive => pliit + "toi" ;
        NF Pl partitive => pliit + "toitõ" ;
        NF Sg illative => pliit + "tasõ" ;
        NF Pl illative => pliit + "toisõ" ;
        NF Sg inessive => pliit + "tõz" ;
        NF Pl inessive => pliit + "toiz" ;
        NF Sg elative => pliit + "õssõ" ;
        NF Pl elative => pliit + "toissõ" ;
        NF Sg allative => pliit + "õllõ" ;
        NF Pl allative => pliit + "toillõ" ;
        NF Sg adessive => pliit + "õl" ;
        NF Pl adessive => pliit + "toil" ;
        NF Sg ablative => pliit + "õltõ" ;
        NF Pl ablative => pliit + "toiltõ" ;
        NF Sg translative => pliit + "õssi" ;
        NF Pl translative => pliit + "toissi" ;
        NF Sg terminative => pliit + "tassaa" ;
        NF Pl terminative => pliit + "toissaa" ;
        NF Sg comitative => pliit + "aka" ;
        NF Pl comitative => pliit + "toika" ;
        NF Sg partitive => pliit + "taa" ;
        NF Sg illative => pliit + "ta"
      }
    } ;


  mkMansikõz : Str -> Noun = \mansikõz -> 
    case mansikõz of {
      mansik + "õz" => mkMansikõzConcrete mansik ;
      _ => Predef.error "Unsuitable lemma for mkMansikõz"
    } ;

  mkMansikõzConcrete : Str -> Noun = \mansik -> 
    { s =
      table {
        NF Sg nominative => mansik + "õz" ;
        NF Pl nominative => mansik + "kad" ;
        NF Sg genitive => mansik + "ka" ;
        NF Pl genitive => mansik + "kaijõ" ;
        NF Sg partitive => mansik + "assõ" ;
        NF Pl partitive => mansik + "kaitõ" ;
        NF Sg illative => mansik + "kasõ" ;
        NF Pl illative => mansik + "kaisõ" ;
        NF Sg inessive => mansik + "kaz" ;
        NF Pl inessive => mansik + "kaiz" ;
        NF Sg elative => mansik + "kassõ" ;
        NF Pl elative => mansik + "kaissõ" ;
        NF Sg allative => mansik + "kallõ" ;
        NF Pl allative => mansik + "kaillõ" ;
        NF Sg adessive => mansik + "kal" ;
        NF Pl adessive => mansik + "kail" ;
        NF Sg ablative => mansik + "kaltõ" ;
        NF Pl ablative => mansik + "kailtõ" ;
        NF Sg translative => mansik + "kassi" ;
        NF Pl translative => mansik + "kaissi" ;
        NF Sg terminative => mansik + "kassaa" ;
        NF Pl terminative => mansik + "kaissaa" ;
        NF Sg comitative => mansik + "kaka" ;
        NF Pl comitative => mansik + "kaika"
      }
    } ;


  mkTüttö : Str -> Noun = \tüttö -> 
    case tüttö of {
      tüt + "t" + ö => mkTüttöConcrete tüt ö ;
      _ => Predef.error "Unsuitable lemma for mkTüttö"
    } ;

  mkTüttöConcrete : Str -> Str -> Noun = \tüt,ö -> 
    { s =
      table {
        NF Sg nominative => tüt + "t" + ö ;
        NF Pl nominative => tüt + ö + "d" ;
        NF Sg genitive => tüt + ö ;
        NF Pl genitive => tüt + "t" + ö + "i" ;
        NF Pl genitive => tüt + "t" + ö + "je" ;
        NF Sg partitive => tüt + "t" + ö + "ä" ;
        NF Pl partitive => tüt + "t" + ö + "i" ;
        NF Pl partitive => tüt + "t" + ö + "ite" ;
        NF Sg illative => tüt + "t" + ö + "se" ;
        NF Pl illative => tüt + "t" + ö + "ise" ;
        NF Sg inessive => tüt + "t" + ö + "z" ;
        NF Pl inessive => tüt + "t" + ö + "iz" ;
        NF Sg elative => tüt + ö + "sse" ;
        NF Pl elative => tüt + "t" + ö + "isse" ;
        NF Sg allative => tüt + ö + "lle" ;
        NF Pl allative => tüt + "t" + ö + "ille" ;
        NF Sg adessive => tüt + ö + "l" ;
        NF Pl adessive => tüt + "t" + ö + "il" ;
        NF Sg ablative => tüt + ö + "lte" ;
        NF Pl ablative => tüt + "t" + ö + "ilte" ;
        NF Sg translative => tüt + ö + "ssi" ;
        NF Pl translative => tüt + "t" + ö + "issi" ;
        NF Sg terminative => tüt + "t" + ö + "ssaa" ;
        NF Pl terminative => tüt + "t" + ö + "issaa" ;
        NF Sg comitative => tüt + ö + "ka" ;
        NF Pl comitative => tüt + "t" + ö + "ika"
      }
    } ;


  mkTüttö : Str -> Noun = \tüttö -> 
    case tüttö of {
      tüt + "t" + ö => mkTüttöConcrete tüt ö ;
      _ => Predef.error "Unsuitable lemma for mkTüttö"
    } ;

  mkTüttöConcrete : Str -> Str -> Noun = \tüt,ö -> 
    { s =
      table {
        NF Sg nominative => tüt + "t" + ö ;
        NF Pl nominative => tüt + ö + "d" ;
        NF Sg genitive => tüt + ö ;
        NF Pl genitive => tüt + "t" + ö + "i" ;
        NF Pl genitive => tüt + "t" + ö + "je" ;
        NF Sg partitive => tüt + "t" + ö + "ä" ;
        NF Pl partitive => tüt + "t" + ö + "i" ;
        NF Pl partitive => tüt + "t" + ö + "ite" ;
        NF Sg illative => tüt + "t" + ö + "se" ;
        NF Pl illative => tüt + "t" + ö + "ise" ;
        NF Sg inessive => tüt + "t" + ö + "z" ;
        NF Pl inessive => tüt + "t" + ö + "iz" ;
        NF Sg elative => tüt + ö + "sse" ;
        NF Pl elative => tüt + "t" + ö + "isse" ;
        NF Sg allative => tüt + ö + "lle" ;
        NF Pl allative => tüt + "t" + ö + "ille" ;
        NF Sg adessive => tüt + ö + "l" ;
        NF Pl adessive => tüt + "t" + ö + "il" ;
        NF Sg ablative => tüt + ö + "lte" ;
        NF Pl ablative => tüt + "t" + ö + "ilte" ;
        NF Sg translative => tüt + ö + "ssi" ;
        NF Pl translative => tüt + "t" + ö + "issi" ;
        NF Sg terminative => tüt + "t" + ö + "ssaa" ;
        NF Pl terminative => tüt + "t" + ö + "issaa" ;
        NF Sg comitative => tüt + ö + "ka" ;
        NF Pl comitative => tüt + "t" + ö + "ika"
      }
    } ;


  mkKukkõ : Str -> Noun = \kukkõ -> 
    case kukkõ of {
      kuk + "kõ" => mkKukkõConcrete kuk ;
      _ => Predef.error "Unsuitable lemma for mkKukkõ"
    } ;

  mkKukkõConcrete : Str -> Noun = \kuk -> 
    { s =
      table {
        NF Sg nominative => kuk + "kõ" ;
        NF Pl nominative => kuk + "õd" ;
        NF Sg genitive => kuk + "a" ;
        NF Pl genitive => kuk + "ki" ;
        NF Pl genitive => kuk + "kije" ;
        NF Sg partitive => kuk + "ka" ;
        NF Sg partitive => kuk + "kaa" ;
        NF Pl partitive => kuk + "ki" ;
        NF Pl partitive => kuk + "kitõ" ;
        NF Sg illative => kuk + "kaa" ;
        NF Sg illative => kuk + "kasõ" ;
        NF Pl illative => kuk + "ki" ;
        NF Pl illative => kuk + "kisõ" ;
        NF Sg inessive => kuk + "õz" ;
        NF Pl inessive => kuk + "kiz" ;
        NF Sg elative => kuk + "õssõ" ;
        NF Pl elative => kuk + "kissõ" ;
        NF Sg allative => kuk + "õllõ" ;
        NF Pl allative => kuk + "killõ" ;
        NF Sg adessive => kuk + "õl" ;
        NF Pl adessive => kuk + "kil" ;
        NF Sg ablative => kuk + "õltõ" ;
        NF Pl ablative => kuk + "kiltõ" ;
        NF Sg translative => kuk + "õssi" ;
        NF Pl translative => kuk + "kissi" ;
        NF Sg terminative => kuk + "kassaa" ;
        NF Pl terminative => kuk + "kissaa" ;
        NF Sg comitative => kuk + "aka" ;
        NF Pl comitative => kuk + "kika"
      }
    } ;


  mkPäive : Str -> Noun = \päive -> 
    case päive of {
      päiv + "e" => mkPäiveConcrete päiv ;
      _ => Predef.error "Unsuitable lemma for mkPäive"
    } ;

  mkPäiveConcrete : Str -> Noun = \päiv -> 
    { s =
      table {
        NF Sg nominative => päiv + "e" ;
        NF Pl nominative => päiv + "äd" ;
        NF Sg genitive => päiv + "ä" ;
        NF Pl genitive => päiv + "i" ;
        NF Pl genitive => päiv + "ije" ;
        NF Sg partitive => päiv + "ä" ;
        NF Sg partitive => päiv + "ää" ;
        NF Pl partitive => päiv + "i" ;
        NF Pl partitive => päiv + "ii" ;
        NF Sg illative => päiv + "ää" ;
        NF Sg illative => päiv + "äse" ;
        NF Pl illative => päiv + "i" ;
        NF Pl illative => päiv + "ise" ;
        NF Sg inessive => päiv + "äz" ;
        NF Pl inessive => päiv + "iz" ;
        NF Sg elative => päiv + "ässä" ;
        NF Pl elative => päiv + "issä" ;
        NF Sg allative => päiv + "ällä" ;
        NF Pl allative => päiv + "ille" ;
        NF Sg adessive => päiv + "äl" ;
        NF Pl adessive => päiv + "il" ;
        NF Sg ablative => päiv + "älte" ;
        NF Pl ablative => päiv + "ilte" ;
        NF Sg translative => päiv + "ässi" ;
        NF Pl translative => päiv + "issi" ;
        NF Sg terminative => päiv + "ässaa" ;
        NF Pl terminative => päiv + "issaa" ;
        NF Sg comitative => päiv + "äka" ;
        NF Pl comitative => päiv + "ika"
      }
    } ;


  mkPartõ : Str -> Noun = \partõ -> 
    case partõ of {
      par + "tõ" => mkPartõConcrete par ;
      _ => Predef.error "Unsuitable lemma for mkPartõ"
    } ;

  mkPartõConcrete : Str -> Noun = \par -> 
    { s =
      table {
        NF Sg nominative => par + "tõ" ;
        NF Pl nominative => par + "rõd" ;
        NF Sg genitive => par + "ra" ;
        NF Pl genitive => par + "toi" ;
        NF Pl genitive => par + "tojõ" ;
        NF Sg partitive => par + "ta" ;
        NF Pl partitive => par + "toi" ;
        NF Pl partitive => par + "toitõ" ;
        NF Sg illative => par + "tasõ" ;
        NF Pl illative => par + "toisõ" ;
        NF Sg inessive => par + "rõz" ;
        NF Pl inessive => par + "toiz" ;
        NF Sg elative => par + "rõssõ" ;
        NF Pl elative => par + "toissõ" ;
        NF Sg allative => par + "rõllõ" ;
        NF Pl allative => par + "toillõ" ;
        NF Sg adessive => par + "rõl" ;
        NF Pl adessive => par + "toil" ;
        NF Sg ablative => par + "rõltõ" ;
        NF Pl ablative => par + "toiltõ" ;
        NF Sg translative => par + "rõssi" ;
        NF Pl translative => par + "toissi" ;
        NF Sg terminative => par + "tassaa" ;
        NF Pl terminative => par + "toissaa" ;
        NF Sg comitative => par + "raka" ;
        NF Pl comitative => par + "toika" ;
        NF Sg partitive => par + "taa" ;
        NF Sg illative => par + "ta"
      }
    } ;


}