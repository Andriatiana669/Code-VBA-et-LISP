// =================================================================
//
//  RBLOC.DCL V2.20
//
//  Copyright (C) Patrick_35
//
// =================================================================

rbloc : dialog {
  key = "titre";
  is_cancel = true;
  : boxed_column {
    label = " Bloc(s) d'origine(s) ";
    : row {
      : popup_list {key = "listeo"; width = 30; label = "Nom";}
      : button     {key = "sel"; width = 15; label = "Sélection...";}
    }
    spacer;
    : toggle {key = "attr"; label = "Conserver les attributs";}
    : toggle {key = "dyna"; label = "Conserver les propriétés dynamiques";}
    : text {key = "texte1";}
  }
  : boxed_column {
    label = " Bloc remplaçant ";
    : popup_list {key = "lister"; width = 30; label = "Nom";}
    spacer;
    : row {
      : button     {key = "pick"; width = 15; label = "Sélection...";}
      : button     {key = "rech"; width = 15; label = "Parcourir...";}
    }
    : text {key = "texte2";}
  }
  : boxed_column {
    label = " Echelle ";
    : toggle {key = "echori"; label = "Conserver l'échelle d'origine";}
    : toggle {key = "uniforme"; label = "Echelle uniforme";}
    : row {
      : edit_box {key = "fact_x"; width = 5; label = "X:";}
      : edit_box {key = "fact_y"; width = 5; label = "Y:";}
      : edit_box {key = "fact_z"; width = 5; label = "Z:";}
    }
    : text {key = "texte3";}
  }
  spacer;
  ok_cancel;
}