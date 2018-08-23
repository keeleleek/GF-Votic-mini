concrete MiniLexiconVot of MiniLexicon = MiniGrammarVot ** open MiniResVot,Prelude in {
-- lin already_Adv = mkAdv "already" ;
-- lin animal_N = mkN "animal" ;
lin apple_N = mkOmõn "omõn" ;
-- lin baby_N = mkN "baby" ;
-- lin bad_A = mkA "bad" ; -- märännü
-- lin beer_N = mkN "beer" ; -- õlut
-- lin big_A = mkA "big" ; -- suuri
-- lin bike_N = mkN "bike" ; -- samakatka, velossipeeda
-- lin bird_N = mkN "bird" ; -- lintu
-- lin black_A = mkA "black" ; -- mussa
-- lin blood_N = mkN "blood" ; -- veri
lin blue_A = mkSinin "sinin" ; -- TODO this is not an adjective
-- lin boat_N = mkN "boat" ; -- vene
-- lin book_N = mkN "book" ; -- čirja
lin boy_N = mkPoikõ "poikõ" ;
-- lin bread_N = mkN "bread" ; -- leipe
-- lin break_V2 = mkV2 "break" ; 
-- lin buy_V2 = mkV2 "buy" ;
-- lin car_N = mkN "car" ; -- mašinõ
lin cat_N = mkKatti "katti" ;
-- lin child_N = mkN "child" "children" ; -- lahsi
-- lin city_N = mkN "city" ; -- lidna
-- lin clean_A = mkA "clean" ; -- puhaz
-- lin clever_A = mkA "clever" ; -- tarkka
-- lin cloud_N = mkN "cloud" ; -- pilvi
-- lin cold_A = mkA "cold" ; -- čülmä
  lin come_V = {
	s = table {
	  Presn Sg Per1 => "tuõn" ;
	  Presn Sg Per2 => "tuõd" ;
	  Presn Sg Per3 => "tuõb" ;
	  Presn Pl Per1 => "tuõmmõ" ;
	  Presn Pl Per2 => "tuõttõ" ;
	  Presn Pl Per3 => "tulla" ;
	  Imp => "tuõ"
	  } ;
	isAux = False ;
	} ;
-- lin computer_N = mkN "computer" ; -- kampuuttera
lin cow_N = mkPäive "lehme" ;
-- lin dirty_A = mkA "dirty" ; 
lin dog_N = mkKoirõ "koirõ" ;
-- lin drink_V2 = mkV2 "drink" ;
-- lin eat_V2 = mkV2 "eat" ;
-- lin find_V2 = mkV2 "find" ;
-- lin fire_N = mkN "fire" ; -- tuli
-- lin fish_N = mkN "fish" ; -- kala
-- lin flower_N = mkN "flower" ; -- kukka
-- lin friend_N = mkN "friend" ; -- tavarissa
lin girl_N = mkTüttö "tüttö" ; -- tüttö
-- lin good_A = mkA "good" ; -- üvä
  lin go_V = {
	s = table {
	  Presn Sg Per1 => "meen" ;
	  Presn Sg Per2 => "meed" ;
	  Presn Sg Per3 => "meeb" ;
	  Presn Pl Per1 => "meemme" ;
	  Presn Pl Per2 => "meette" ;
	  Presn Pl Per3 => "mennä" ;
	  Imp => "mee"
	  } ;
	isAux = False ;
	} ;
-- lin grammar_N = mkN "grammar" ;
-- lin green_A = mkA "green" ; -- rohoin
-- lin heavy_A = mkA "heavy" ; -- raskaz
-- lin horse_N = mkN "horse" ; -- õpõn
-- lin hot_A = mkA "hot" ;
-- lin house_N = mkN "house" ; -- koto
-- lin john_PN = mkPN "John" ;
-- lin jump_V = mkV "jump" ;
-- lin kill_V2 = mkV2 "kill" ;
-- lin language_N = mkN "language" ; -- čeeli
  lin live_V = {
	s = table {
	  Presn Sg Per1 => "elän" ;
	  Presn Sg Per2 => "eläd" ;
	  Presn Sg Per3 => "eläb" ;
	  Presn Pl Per1 => "elämme" ;
	  Presn Pl Per2 => "elätte" ;
	  Presn Pl Per3 => "eletä" ;
	  Imp => "elä"
	  } ;
	isAux = False ;
	} ;
-- lin love_V2 = mkV2 "love" ;
-- lin man_N = mkN "man" "men" ; -- meez
lin milk_N = mkPäive "piime" ;
-- lin music_N = mkN "music" ; -- muzükka
-- lin new_A = mkA "new" ; -- uusi 
-- lin now_Adv = mkAdv "now" ;
-- lin old_A = mkA "old" ;
-- lin paris_PN = mkPN "Paris" ;
  lin play_V = {
	s = table {
	  Presn Sg Per1 => "pelan" ;
	  Presn Sg Per2 => "pelad" ;
	  Presn Sg Per3 => "pelab" ;
	  Presn Pl Per1 => "pelammõ" ;
	  Presn Pl Per2 => "pelattõ" ;
	  Presn Pl Per3 => "pelata" ;
	  Imp => "pela"
	  } ;
	isAux = False ;
	} ;
  lin read_V2 = mkV2 {
	s = table {
	  Presn Sg Per1 => "lugõn" ;
	  Presn Sg Per2 => "lugõd" ;
	  Presn Sg Per3 => "lugõb" ;
	  Presn Pl Per1 => "lugõmmõ" ;
	  Presn Pl Per2 => "lugõttõ" ;
	  Presn Pl Per3 => "lugõta" ;
	  Imp => "lugõ"
	  } ;
	isAux = False ;
	} ;
-- lin ready_A = mkA "ready" ;
-- lin red_A = mkA "red" ;
-- lin river_N = mkN "river" ;
-- lin run_V = mkV "run" ;
-- lin sea_N = mkN "sea" ;
-- lin see_V2 = mkV2 "see" ;
-- lin ship_N = mkN "ship" ;
-- lin sleep_V = mkV "sleep" ;
-- lin small_A = mkA "small" ;
-- lin star_N = mkN "star" ;
-- lin swim_V = mkV "swim" ;
-- lin teach_V2 = mkV2 "teach" ;
-- lin train_N = mkN "train" ;
-- lin travel_V = mkV "travel" ;
-- lin tree_N = mkN "tree" ;
-- lin understand_V2 = mkV2 "understand" ;
-- lin wait_V2 = mkV2 "wait" "for" ;
-- lin walk_V = mkV "walk" ;
-- lin warm_A = mkA "warm" ;
-- lin water_N = mkN "water" ;
-- lin white_A = mkA "white" ;
-- lin wine_N = mkN "wine" ;
-- lin woman_N = mkN "woman" "women" ;
-- lin yellow_A = mkA "yellow" ;
-- lin young_A = mkA "young" ;

}
