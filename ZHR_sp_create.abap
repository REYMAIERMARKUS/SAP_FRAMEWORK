*&---------------------------------------------------------------------*
*& Report ZHR_SP_CREATE
*&---------------------------------------------------------------------*
*& Anlegen Debitor aus Personalstamm:
*&
*& Markus Reymaier       14.02.2019      "NEW"
*&---------------------------------------------------------------------*
REPORT ZHR_sp_create.

INCLUDE ZHR_sp_create_top.    "TopInclude
INCLUDE ZHR_sp_create_lcl.    "Lokale Klasse
INCLUDE ZHR_sp_create_scr.    "Screen

START-OF-SELECTION.
* Lokale Klasse Erzeugen, in dem alle Funktionen des Programmes enthalten sind
  ao_sp = NEW #( iv_test = p_test ).

* Schränke die Standard PNPCE Selektion über die Parameter auf der Startmaske ein:
  pnpindex[] = ao_sp->get_pnp( EXPORTING iv_sel   = COND i( WHEN p_sel1 = 'X' THEN 1
                                                            WHEN p_sel2 = 'X' THEN 2
                                                            WHEN p_sel3 = 'X' THEN 3 )
                                         iv_begda = pn-begps
                                         iv_endda = pn-endps ).

* Get-schleife m. personen aus log. db
  GET peras.

* Personen nur zur Liste hinzufügen wenn ein Eintrag im IT0017 vorhanden (Reiseprivilegien)
  IF NOT p0017[] IS INITIAL.
    ao_sp->ao_list->add_via_logdb( ).
  ENDIF.

END-OF-SELECTION.
* Hauptprogramm starten
  ao_sp->do_main( EXPORTING iv_due_date = pn-endda ).

* gesammelte Daten darstellen
  ao_sp->do_display( ).