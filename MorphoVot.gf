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
        NF singular nominative => aa + "põ" ;
        NF plural nominative => aa + "võd" ;
        NF singular genitive => aa + "va" ;
        NF plural genitive => aa + "poi" ;
        NF plural genitive => aa + "pojõ" ;
        NF singular partitive => aa + "pa" ;
        NF plural partitive => aa + "poi" ;
        NF plural partitive => aa + "poitõ" ;
        NF singular illative => aa + "paa" ;
        NF singular illative => aa + "pasõ" ;
        NF plural illative => aa + "poisõ" ;
        NF singular inessive => aa + "vaz" ;
        NF plural inessive => aa + "voiz" ;
        NF singular elative => aa + "võssõ" ;
        NF plural elative => aa + "poissõ" ;
        NF singular allative => aa + "võllõ" ;
        NF plural allative => aa + "poillõ" ;
        NF singular adessive => aa + "võl" ;
        NF plural adessive => aa + "poil" ;
        NF singular ablative => aa + "võssi" ;
        NF plural ablative => aa + "poissi" ;
        NF singular translative => aa + "passi" ;
        NF plural translative => aa + "poissi" ;
        NF singular terminative => aa + "passaa" ;
        NF plural terminative => aa + "poissaa" ;
        NF singular comitative => aa + "vaka" ;
        NF plural comitative => aa + "poika"
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
        NF singular nominative => sini + "n" ;
        NF plural nominative => sini + "zed" ;
        NF singular genitive => sini + "ze" ;
        NF plural genitive => sini + "zije" ;
        NF singular partitive => sini + "sse" ;
        NF plural partitive => sini + "zii" ;
        NF plural partitive => sini + "ziit" ;
        NF singular illative => sini + "ze" ;
        NF singular illative => sini + "zese" ;
        NF plural illative => sini + "zije" ;
        NF plural illative => sini + "zise" ;
        NF singular inessive => sini + "zez" ;
        NF plural inessive => sini + "ziz" ;
        NF singular elative => sini + "zess" ;
        NF plural elative => sini + "ziss" ;
        NF singular allative => sini + "zelle" ;
        NF plural allative => sini + "zille" ;
        NF singular adessive => sini + "zell" ;
        NF plural adessive => sini + "zill" ;
        NF singular ablative => sini + "zelt" ;
        NF plural ablative => sini + "zilt" ;
        NF singular translative => sini + "zessi" ;
        NF plural translative => sini + "zissi" ;
        NF singular terminative => sini + "zeessaa" ;
        NF plural terminative => sini + "ziissaa" ;
        NF singular comitative => sini + "zeka" ;
        NF plural comitative => sini + "zika"
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
        NF singular nominative => ai + "kõ" ;
        NF plural nominative => ai + "gõd" ;
        NF singular genitive => ai + "ga" ;
        NF plural genitive => ai + "koi" ;
        NF plural genitive => ai + "kojõ" ;
        NF singular partitive => ai + "ka" ;
        NF singular partitive => ai + "kaa" ;
        NF plural partitive => ai + "koi" ;
        NF plural partitive => ai + "koitõ" ;
        NF singular illative => ai + "ka" ;
        NF singular illative => ai + "kasõ" ;
        NF plural illative => ai + "koisõ" ;
        NF singular inessive => ai + "gõz" ;
        NF plural inessive => ai + "koiz" ;
        NF singular elative => ai + "gõssõ" ;
        NF plural elative => ai + "koissõ" ;
        NF singular allative => ai + "gõllõ" ;
        NF plural allative => ai + "koillõ" ;
        NF singular adessive => ai + "gõl" ;
        NF plural adessive => ai + "koil" ;
        NF singular ablative => ai + "gõltõ" ;
        NF plural ablative => ai + "koiltõ" ;
        NF singular translative => ai + "gõssi" ;
        NF plural translative => ai + "koissi" ;
        NF singular terminative => ai + "kassaa" ;
        NF plural terminative => ai + "koissaa" ;
        NF singular comitative => ai + "gaka" ;
        NF plural comitative => ai + "koika"
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
        NF singular nominative => poi + "kõ" ;
        NF plural nominative => poi + "gõd" ;
        NF singular genitive => poi + "ga" ;
        NF plural genitive => poi + "ki" ;
        NF plural genitive => poi + "kije" ;
        NF singular partitive => poi + "ka" ;
        NF singular partitive => poi + "kaa" ;
        NF plural partitive => poi + "ki" ;
        NF plural partitive => poi + "kitõ" ;
        NF singular illative => poi + "kaa" ;
        NF singular illative => poi + "kasõ" ;
        NF plural illative => poi + "ki" ;
        NF plural illative => poi + "kisõ" ;
        NF singular inessive => poi + "gõz" ;
        NF plural inessive => poi + "kiz" ;
        NF singular elative => poi + "gõssõ" ;
        NF plural elative => poi + "kissõ" ;
        NF singular allative => poi + "gõllõ" ;
        NF plural allative => poi + "killõ" ;
        NF singular adessive => poi + "gõl" ;
        NF plural adessive => poi + "kil" ;
        NF singular ablative => poi + "gõltõ" ;
        NF plural ablative => poi + "kiltõ" ;
        NF singular translative => poi + "gõssi" ;
        NF plural translative => poi + "kissi" ;
        NF singular terminative => poi + "kassaa" ;
        NF plural terminative => poi + "kissaa" ;
        NF singular comitative => poi + "gaka" ;
        NF plural comitative => poi + "kika"
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
        NF singular nominative => am + "mõz" ;
        NF plural nominative => am + "pad" ;
        NF singular genitive => am + "pa" ;
        NF plural genitive => am + "paijõ" ;
        NF singular partitive => am + "massõ" ;
        NF plural partitive => am + "paitõ" ;
        NF singular illative => am + "pasõ" ;
        NF plural illative => am + "paisõ" ;
        NF singular inessive => am + "paz" ;
        NF plural inessive => am + "paiz" ;
        NF singular elative => am + "passõ" ;
        NF plural elative => am + "paissõ" ;
        NF singular allative => am + "pallõ" ;
        NF plural allative => am + "paillõ" ;
        NF singular adessive => am + "pal" ;
        NF plural adessive => am + "pail" ;
        NF singular ablative => am + "paltõ" ;
        NF plural ablative => am + "pailtõ" ;
        NF singular translative => am + "passi" ;
        NF plural translative => am + "paissi" ;
        NF singular terminative => am + "passaa" ;
        NF plural terminative => am + "paissaa" ;
        NF singular comitative => am + "paka" ;
        NF plural comitative => am + "paika"
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
        NF singular nominative => ai + "kõ" ;
        NF plural nominative => ai + "gõd" ;
        NF singular genitive => ai + "ga" ;
        NF plural genitive => ai + "koi" ;
        NF plural genitive => ai + "kojõ" ;
        NF singular partitive => ai + "ka" ;
        NF singular partitive => ai + "kaa" ;
        NF plural partitive => ai + "koi" ;
        NF plural partitive => ai + "koitõ" ;
        NF singular illative => ai + "ka" ;
        NF singular illative => ai + "kasõ" ;
        NF plural illative => ai + "koisõ" ;
        NF singular inessive => ai + "gõz" ;
        NF plural inessive => ai + "koiz" ;
        NF singular elative => ai + "gõssõ" ;
        NF plural elative => ai + "koissõ" ;
        NF singular allative => ai + "gõllõ" ;
        NF plural allative => ai + "koillõ" ;
        NF singular adessive => ai + "gõl" ;
        NF plural adessive => ai + "koil" ;
        NF singular ablative => ai + "gõltõ" ;
        NF plural ablative => ai + "koiltõ" ;
        NF singular translative => ai + "gõssi" ;
        NF plural translative => ai + "koissi" ;
        NF singular terminative => ai + "kassaa" ;
        NF plural terminative => ai + "koissaa" ;
        NF singular comitative => ai + "gaka" ;
        NF plural comitative => ai + "koika"
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
        NF singular nominative => koir + "õ" ;
        NF plural nominative => koir + "õd" ;
        NF singular genitive => koir + "a" ;
        NF plural genitive => koir + "i" ;
        NF plural genitive => koir + "ije" ;
        NF singular partitive => koir + "a" ;
        NF singular partitive => koir + "aa" ;
        NF plural partitive => koir + "i" ;
        NF plural partitive => koir + "itõ" ;
        NF singular illative => koir + "aa" ;
        NF singular illative => koir + "asõ" ;
        NF plural illative => koir + "i" ;
        NF plural illative => koir + "isõ" ;
        NF singular inessive => koir + "õz" ;
        NF plural inessive => koir + "iz" ;
        NF singular elative => koir + "õssõ" ;
        NF plural elative => koir + "issõ" ;
        NF singular allative => koir + "õllõ" ;
        NF plural allative => koir + "illõ" ;
        NF singular adessive => koir + "õl" ;
        NF plural adessive => koir + "il" ;
        NF singular ablative => koir + "õltõ" ;
        NF plural ablative => koir + "iltõ" ;
        NF singular translative => koir + "õssi" ;
        NF plural translative => koir + "issi" ;
        NF singular terminative => koir + "assaa" ;
        NF plural terminative => koir + "issaa" ;
        NF singular comitative => koir + "aka" ;
        NF plural comitative => koir + "ika"
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
        NF singular nominative => lentü + "z" ;
        NF plural nominative => lentü + "sed" ;
        NF singular genitive => lentü + "se" ;
        NF plural genitive => lentü + "si" ;
        NF singular partitive => lentü + "sse" ;
        NF plural partitive => lentü + "ssi" ;
        NF singular illative => lentü + "sesse" ;
        NF plural illative => lentü + "sisse" ;
        NF singular inessive => lentü + "sez" ;
        NF plural inessive => lentü + "siz" ;
        NF singular elative => lentü + "sse" ;
        NF plural elative => lentü + "sissõ" ;
        NF singular allative => lentü + " gõllõ" ;
        NF plural allative => lentü + " koillõ" ;
        NF singular adessive => lentü + " gõl" ;
        NF plural adessive => lentü + " koil" ;
        NF singular ablative => lentü + " gõltõ" ;
        NF plural ablative => lentü + " koiltõ" ;
        NF singular translative => lentü + " gõssi" ;
        NF plural translative => lentü + " koissi" ;
        NF singular terminative => lentü + " kassaa" ;
        NF plural terminative => lentü + " koissaa" ;
        NF singular comitative => lentü + " gaka" ;
        NF plural comitative => lentü + " koika"
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
        NF singular nominative => luzik + "kõ" ;
        NF plural nominative => luzik + "õd" ;
        NF singular genitive => luzik + "a" ;
        NF plural genitive => luzik + "koi" ;
        NF plural genitive => luzik + "kojõ" ;
        NF singular partitive => luzik + "ka" ;
        NF singular partitive => luzik + "kaa" ;
        NF plural partitive => luzik + "koi" ;
        NF plural partitive => luzik + "koitõ" ;
        NF singular illative => luzik + "ka" ;
        NF singular illative => luzik + "kasõ" ;
        NF plural illative => luzik + "koisõ" ;
        NF singular inessive => luzik + "õz" ;
        NF plural inessive => luzik + "koiz" ;
        NF singular elative => luzik + "õssõ" ;
        NF plural elative => luzik + "koissõ" ;
        NF singular allative => luzik + "gõllõ" ;
        NF plural allative => luzik + "koillõ" ;
        NF singular adessive => luzik + "õl" ;
        NF plural adessive => luzik + "koil" ;
        NF singular ablative => luzik + "õltõ" ;
        NF plural ablative => luzik + "koiltõ" ;
        NF singular translative => luzik + "õssi" ;
        NF plural translative => luzik + "koissi" ;
        NF singular terminative => luzik + "kassaa" ;
        NF plural terminative => luzik + "koissaa" ;
        NF singular comitative => luzik + "aka" ;
        NF plural comitative => luzik + "koika"
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
        NF singular nominative => luik + "ko" ;
        NF plural nominative => luik + "od" ;
        NF singular genitive => luik + "o" ;
        NF plural genitive => luik + "koi" ;
        NF plural genitive => luik + "kojõ" ;
        NF singular partitive => luik + "koa" ;
        NF plural partitive => luik + "koitõ" ;
        NF singular illative => luik + "kosõ" ;
        NF plural illative => luik + "koisõ" ;
        NF singular inessive => luik + "oz" ;
        NF plural inessive => luik + "koiz" ;
        NF singular elative => luik + "ossõ" ;
        NF plural elative => luik + "koissõ" ;
        NF singular allative => luik + "ollõ" ;
        NF plural allative => luik + "koillõ" ;
        NF singular adessive => luik + "ol" ;
        NF plural adessive => luik + "koil" ;
        NF singular ablative => luik + "oltõ" ;
        NF plural ablative => luik + "koiltõ" ;
        NF singular translative => luik + "ossi" ;
        NF plural translative => luik + "koissi" ;
        NF singular terminative => luik + "kassaa" ;
        NF plural terminative => luik + "koissaa" ;
        NF singular comitative => luik + "oka" ;
        NF plural comitative => luik + "koika"
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
        NF singular nominative => ai + "tõ" ;
        NF plural nominative => ai + "jõd" ;
        NF singular genitive => ai + "ja" ;
        NF plural genitive => ai + "toi" ;
        NF plural genitive => ai + "tojõ" ;
        NF singular partitive => ai + "ta" ;
        NF plural partitive => ai + "toi" ;
        NF plural partitive => ai + "toitõ" ;
        NF singular illative => ai + "taa" ;
        NF singular illative => ai + "tasõ" ;
        NF plural illative => ai + "toisõ" ;
        NF singular inessive => ai + "jaz" ;
        NF plural inessive => ai + "joiz" ;
        NF singular elative => ai + "jõssõ" ;
        NF plural elative => ai + "toissõ" ;
        NF singular allative => ai + "jõllõ" ;
        NF plural allative => ai + "toillõ" ;
        NF singular adessive => ai + "jõl" ;
        NF plural adessive => ai + "toil" ;
        NF singular ablative => ai + "jõssi" ;
        NF plural ablative => ai + "toissi" ;
        NF singular translative => ai + "tassi" ;
        NF plural translative => ai + "toissi" ;
        NF singular terminative => ai + "tassaa" ;
        NF plural terminative => ai + "toissaa" ;
        NF singular comitative => ai + "jaka" ;
        NF plural comitative => ai + "toika"
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
        NF singular nominative => ahkõr + "õ" ;
        NF plural nominative => ahkõr + "õd" ;
        NF singular genitive => ahkõr + "a" ;
        NF plural genitive => ahkõr + "oi" ;
        NF singular partitive => ahkõr + "a" ;
        NF plural partitive => ahkõr + "oi" ;
        NF plural partitive => ahkõr + "oitõ" ;
        NF singular illative => ahkõr + "aa" ;
        NF singular illative => ahkõr + "asõ" ;
        NF plural illative => ahkõr + "oisõ" ;
        NF singular inessive => ahkõr + "az" ;
        NF plural inessive => ahkõr + "oiz" ;
        NF singular elative => ahkõr + "õssõ" ;
        NF plural elative => ahkõr + "oissõ" ;
        NF singular allative => ahkõr + "õllõ" ;
        NF plural allative => ahkõr + "oillõ" ;
        NF singular adessive => ahkõr + "õl" ;
        NF plural adessive => ahkõr + "oil" ;
        NF singular ablative => ahkõr + "õssi" ;
        NF plural ablative => ahkõr + "oissi" ;
        NF singular translative => ahkõr + "assi" ;
        NF plural translative => ahkõr + "oissi" ;
        NF singular terminative => ahkõr + "assaa" ;
        NF plural terminative => ahkõr + "oissaa" ;
        NF singular comitative => ahkõr + "aka" ;
        NF plural comitative => ahkõr + "oika"
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
        NF singular nominative => omõn ;
        NF plural nominative => omõn + "ad" ;
        NF singular genitive => omõn + "a" ;
        NF plural genitive => omõn + "oi" ;
        NF plural genitive => omõn + "ojõ" ;
        NF singular partitive => omõn + "a" ;
        NF singular partitive => omõn + "aa" ;
        NF plural partitive => omõn + "oi" ;
        NF plural partitive => omõn + "oitõ" ;
        NF singular illative => omõn + "aa" ;
        NF singular illative => omõn + "asõ" ;
        NF plural illative => omõn + "oisõ" ;
        NF singular inessive => omõn + "õz" ;
        NF plural inessive => omõn + "oiz" ;
        NF singular elative => omõn + "õssõ" ;
        NF plural elative => omõn + "oissõ" ;
        NF singular allative => omõn + "õllõ" ;
        NF plural allative => omõn + "oillõ" ;
        NF singular adessive => omõn + "õl" ;
        NF plural adessive => omõn + "oil" ;
        NF singular ablative => omõn + "õltõ" ;
        NF plural ablative => omõn + "oiltõ" ;
        NF singular translative => omõn + "õssi" ;
        NF plural translative => omõn + "oissi" ;
        NF singular terminative => omõn + "assaa" ;
        NF plural terminative => omõn + "oissaa" ;
        NF singular comitative => omõn + "aka" ;
        NF plural comitative => omõn + "oika"
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
        NF singular nominative => pliit + "tõ" ;
        NF plural nominative => pliit + "õd" ;
        NF singular genitive => pliit + "a" ;
        NF plural genitive => pliit + "toi" ;
        NF plural genitive => pliit + "tojõ" ;
        NF singular partitive => pliit + "ta" ;
        NF plural partitive => pliit + "toi" ;
        NF plural partitive => pliit + "toitõ" ;
        NF singular illative => pliit + "tasõ" ;
        NF plural illative => pliit + "toisõ" ;
        NF singular inessive => pliit + "tõz" ;
        NF plural inessive => pliit + "toiz" ;
        NF singular elative => pliit + "õssõ" ;
        NF plural elative => pliit + "toissõ" ;
        NF singular allative => pliit + "õllõ" ;
        NF plural allative => pliit + "toillõ" ;
        NF singular adessive => pliit + "õl" ;
        NF plural adessive => pliit + "toil" ;
        NF singular ablative => pliit + "õltõ" ;
        NF plural ablative => pliit + "toiltõ" ;
        NF singular translative => pliit + "õssi" ;
        NF plural translative => pliit + "toissi" ;
        NF singular terminative => pliit + "tassaa" ;
        NF plural terminative => pliit + "toissaa" ;
        NF singular comitative => pliit + "aka" ;
        NF plural comitative => pliit + "toika" ;
        NF singular partitive => pliit + "taa" ;
        NF singular illative => pliit + "ta"
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
        NF singular nominative => mansik + "õz" ;
        NF plural nominative => mansik + "kad" ;
        NF singular genitive => mansik + "ka" ;
        NF plural genitive => mansik + "kaijõ" ;
        NF singular partitive => mansik + "assõ" ;
        NF plural partitive => mansik + "kaitõ" ;
        NF singular illative => mansik + "kasõ" ;
        NF plural illative => mansik + "kaisõ" ;
        NF singular inessive => mansik + "kaz" ;
        NF plural inessive => mansik + "kaiz" ;
        NF singular elative => mansik + "kassõ" ;
        NF plural elative => mansik + "kaissõ" ;
        NF singular allative => mansik + "kallõ" ;
        NF plural allative => mansik + "kaillõ" ;
        NF singular adessive => mansik + "kal" ;
        NF plural adessive => mansik + "kail" ;
        NF singular ablative => mansik + "kaltõ" ;
        NF plural ablative => mansik + "kailtõ" ;
        NF singular translative => mansik + "kassi" ;
        NF plural translative => mansik + "kaissi" ;
        NF singular terminative => mansik + "kassaa" ;
        NF plural terminative => mansik + "kaissaa" ;
        NF singular comitative => mansik + "kaka" ;
        NF plural comitative => mansik + "kaika"
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
        NF singular nominative => tüt + "t" + ö ;
        NF plural nominative => tüt + ö + "d" ;
        NF singular genitive => tüt + ö ;
        NF plural genitive => tüt + "t" + ö + "i" ;
        NF plural genitive => tüt + "t" + ö + "je" ;
        NF singular partitive => tüt + "t" + ö + "ä" ;
        NF plural partitive => tüt + "t" + ö + "i" ;
        NF plural partitive => tüt + "t" + ö + "ite" ;
        NF singular illative => tüt + "t" + ö + "se" ;
        NF plural illative => tüt + "t" + ö + "ise" ;
        NF singular inessive => tüt + "t" + ö + "z" ;
        NF plural inessive => tüt + "t" + ö + "iz" ;
        NF singular elative => tüt + ö + "sse" ;
        NF plural elative => tüt + "t" + ö + "isse" ;
        NF singular allative => tüt + ö + "lle" ;
        NF plural allative => tüt + "t" + ö + "ille" ;
        NF singular adessive => tüt + ö + "l" ;
        NF plural adessive => tüt + "t" + ö + "il" ;
        NF singular ablative => tüt + ö + "lte" ;
        NF plural ablative => tüt + "t" + ö + "ilte" ;
        NF singular translative => tüt + ö + "ssi" ;
        NF plural translative => tüt + "t" + ö + "issi" ;
        NF singular terminative => tüt + "t" + ö + "ssaa" ;
        NF plural terminative => tüt + "t" + ö + "issaa" ;
        NF singular comitative => tüt + ö + "ka" ;
        NF plural comitative => tüt + "t" + ö + "ika"
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
        NF singular nominative => tüt + "t" + ö ;
        NF plural nominative => tüt + ö + "d" ;
        NF singular genitive => tüt + ö ;
        NF plural genitive => tüt + "t" + ö + "i" ;
        NF plural genitive => tüt + "t" + ö + "je" ;
        NF singular partitive => tüt + "t" + ö + "ä" ;
        NF plural partitive => tüt + "t" + ö + "i" ;
        NF plural partitive => tüt + "t" + ö + "ite" ;
        NF singular illative => tüt + "t" + ö + "se" ;
        NF plural illative => tüt + "t" + ö + "ise" ;
        NF singular inessive => tüt + "t" + ö + "z" ;
        NF plural inessive => tüt + "t" + ö + "iz" ;
        NF singular elative => tüt + ö + "sse" ;
        NF plural elative => tüt + "t" + ö + "isse" ;
        NF singular allative => tüt + ö + "lle" ;
        NF plural allative => tüt + "t" + ö + "ille" ;
        NF singular adessive => tüt + ö + "l" ;
        NF plural adessive => tüt + "t" + ö + "il" ;
        NF singular ablative => tüt + ö + "lte" ;
        NF plural ablative => tüt + "t" + ö + "ilte" ;
        NF singular translative => tüt + ö + "ssi" ;
        NF plural translative => tüt + "t" + ö + "issi" ;
        NF singular terminative => tüt + "t" + ö + "ssaa" ;
        NF plural terminative => tüt + "t" + ö + "issaa" ;
        NF singular comitative => tüt + ö + "ka" ;
        NF plural comitative => tüt + "t" + ö + "ika"
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
        NF singular nominative => kuk + "kõ" ;
        NF plural nominative => kuk + "õd" ;
        NF singular genitive => kuk + "a" ;
        NF plural genitive => kuk + "ki" ;
        NF plural genitive => kuk + "kije" ;
        NF singular partitive => kuk + "ka" ;
        NF singular partitive => kuk + "kaa" ;
        NF plural partitive => kuk + "ki" ;
        NF plural partitive => kuk + "kitõ" ;
        NF singular illative => kuk + "kaa" ;
        NF singular illative => kuk + "kasõ" ;
        NF plural illative => kuk + "ki" ;
        NF plural illative => kuk + "kisõ" ;
        NF singular inessive => kuk + "õz" ;
        NF plural inessive => kuk + "kiz" ;
        NF singular elative => kuk + "õssõ" ;
        NF plural elative => kuk + "kissõ" ;
        NF singular allative => kuk + "õllõ" ;
        NF plural allative => kuk + "killõ" ;
        NF singular adessive => kuk + "õl" ;
        NF plural adessive => kuk + "kil" ;
        NF singular ablative => kuk + "õltõ" ;
        NF plural ablative => kuk + "kiltõ" ;
        NF singular translative => kuk + "õssi" ;
        NF plural translative => kuk + "kissi" ;
        NF singular terminative => kuk + "kassaa" ;
        NF plural terminative => kuk + "kissaa" ;
        NF singular comitative => kuk + "aka" ;
        NF plural comitative => kuk + "kika"
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
        NF singular nominative => päiv + "e" ;
        NF plural nominative => päiv + "äd" ;
        NF singular genitive => päiv + "ä" ;
        NF plural genitive => päiv + "i" ;
        NF plural genitive => päiv + "ije" ;
        NF singular partitive => päiv + "ä" ;
        NF singular partitive => päiv + "ää" ;
        NF plural partitive => päiv + "i" ;
        NF plural partitive => päiv + "ii" ;
        NF singular illative => päiv + "ää" ;
        NF singular illative => päiv + "äse" ;
        NF plural illative => päiv + "i" ;
        NF plural illative => päiv + "ise" ;
        NF singular inessive => päiv + "äz" ;
        NF plural inessive => päiv + "iz" ;
        NF singular elative => päiv + "ässä" ;
        NF plural elative => päiv + "issä" ;
        NF singular allative => päiv + "ällä" ;
        NF plural allative => päiv + "ille" ;
        NF singular adessive => päiv + "äl" ;
        NF plural adessive => päiv + "il" ;
        NF singular ablative => päiv + "älte" ;
        NF plural ablative => päiv + "ilte" ;
        NF singular translative => päiv + "ässi" ;
        NF plural translative => päiv + "issi" ;
        NF singular terminative => päiv + "ässaa" ;
        NF plural terminative => päiv + "issaa" ;
        NF singular comitative => päiv + "äka" ;
        NF plural comitative => päiv + "ika"
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
        NF singular nominative => par + "tõ" ;
        NF plural nominative => par + "rõd" ;
        NF singular genitive => par + "ra" ;
        NF plural genitive => par + "toi" ;
        NF plural genitive => par + "tojõ" ;
        NF singular partitive => par + "ta" ;
        NF plural partitive => par + "toi" ;
        NF plural partitive => par + "toitõ" ;
        NF singular illative => par + "tasõ" ;
        NF plural illative => par + "toisõ" ;
        NF singular inessive => par + "rõz" ;
        NF plural inessive => par + "toiz" ;
        NF singular elative => par + "rõssõ" ;
        NF plural elative => par + "toissõ" ;
        NF singular allative => par + "rõllõ" ;
        NF plural allative => par + "toillõ" ;
        NF singular adessive => par + "rõl" ;
        NF plural adessive => par + "toil" ;
        NF singular ablative => par + "rõltõ" ;
        NF plural ablative => par + "toiltõ" ;
        NF singular translative => par + "rõssi" ;
        NF plural translative => par + "toissi" ;
        NF singular terminative => par + "tassaa" ;
        NF plural terminative => par + "toissaa" ;
        NF singular comitative => par + "raka" ;
        NF plural comitative => par + "toika" ;
        NF singular partitive => par + "taa" ;
        NF singular illative => par + "ta"
      }
    } ;


}