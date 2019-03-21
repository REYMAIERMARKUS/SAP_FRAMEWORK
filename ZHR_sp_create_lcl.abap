*&---------------------------------------------------------------------*
*&  Include           ZHR_SP_CREATE_LCL
*&---------------------------------------------------------------------*

*========================================================
* Deklaration der lokalen Klasse
CLASS lcl_create_sp DEFINITION.
*-------------------------------------------------------
* Öffentlicher Bereich
*-------------------------------------------------------
  PUBLIC SECTION.
*-------------------------------------------------------
**  TYPE - Deklaration

*-------------------------------------------------------
**  Datendeklaration
    DATA: ao_list TYPE REF TO /porr/cl_hr_pa_list.

*-------------------------------------------------------
**  Konstantendeklaration

*-------------------------------------------------------
**  Methodendeklaration
    METHODS constructor IMPORTING iv_test TYPE boolean DEFAULT abap_true.

    METHODS do_main IMPORTING iv_due_date TYPE datum.

    METHODS do_display.

    METHODS get_pnp IMPORTING iv_sel        TYPE i
                              iv_begda      TYPE begda
                              iv_endda      TYPE endda
                    RETURNING VALUE(rt_pnp) TYPE hrat_range_pernr_tab.

*-------------------------------------------------------
* Privater Bereich
*-------------------------------------------------------
  PRIVATE SECTION.
*-------------------------------------------------------
**  TYPE - Deklaration
    TYPES: BEGIN OF ty_out,
             pernr TYPE pernr_d,                    " PerNr
             cname TYPE p0002-cname,                " Name des Mitarbeiters
             lifnr TYPE lfa1-lifnr,                 " Kreditor Nummer
             stat  TYPE char4,                      " Status
             txt   TYPE char100,                    " Bemerkung
           END OF ty_out.
    TYPES: ty_out_tab TYPE TABLE OF ty_out.

*-------------------------------------------------------
**  Datendeklaration
    DATA: at_out      TYPE ty_out_tab.
    DATA: av_test     TYPE boolean  VALUE abap_true.

*-------------------------------------------------------
**  Konstantendeklaration
    CONSTANTS: ac_exis TYPE icon VALUE '@0V@'.
    CONSTANTS: ac_okay TYPE icon VALUE '@04@'.
    CONSTANTS: ac_fail TYPE icon VALUE '@05@'.

*-------------------------------------------------------
**  Methodendeklaration
    METHODS get_lifnr IMPORTING iv_pernr        TYPE pernr_d
                      RETURNING VALUE(rv_lifnr) TYPE lifnr.

    METHODS get_new_lifnr IMPORTING iv_range        TYPE inri-nrrangenr
                          RETURNING VALUE(rv_lifnr) TYPE lifnr.

ENDCLASS.

*========================================================
* Deklaration Referenz zur lokalen Klasse
DATA: ao_sp   TYPE REF TO lcl_create_sp.

*========================================================
* Implementierung des Inhaltes der Klasse
CLASS lcl_create_sp IMPLEMENTATION.

*-----------------------------------------------------------------------------
*-     Setze Status Testlauf
*-----------------------------------------------------------------------------
* [--->] Importing: iv_test TYPE boolean default 'X'
*-----------------------------------------------------------------------------
  METHOD constructor.
    ao_list = NEW #( ).
    av_test = iv_test.
  ENDMETHOD.

*-----------------------------------------------------------------------------
*-     Hauproutine
*-----------------------------------------------------------------------------
* [--->] Importing: iv_due_date TYPE endda
*-----------------------------------------------------------------------------
  METHOD do_main.

    LOOP AT ao_list->mt_person ASSIGNING FIELD-SYMBOL(<ls_person>).
      TRY .
*         --- ALV Zeile erzeugen ----------------------------------------
          APPEND INITIAL LINE TO at_out ASSIGNING FIELD-SYMBOL(<ls_out>).
          <ls_out>-pernr = <ls_person>-pernr.
          DATA(ls_p0001) = <ls_person>-person->get_s_p0001( iv_due_date = iv_due_date ).
          <ls_out>-cname = ls_p0001-ename.

*         --- Prüfe ob der Lieferant schon angelegt wurde ---------------
          <ls_out>-lifnr = get_lifnr( iv_pernr = <ls_person>-pernr ).

*         --- Falls ein Kreditor schon angelegt wurde -------------------
          IF <ls_out>-lifnr IS NOT INITIAL.
            <ls_out>-stat = ac_exis.
            <ls_out>-txt  = 'Kreditor bereits vorhanden'(t03).

*         --- Kreditor muss angelegt werden -----------------------------
          ELSE.

            <ls_out>-lifnr = me->get_new_lifnr( 'S0' ).

            IF av_test = abap_false.
              /porr/cl_sma_vendor=>do_create_vendor( EXPORTING iv_lifnr    = <ls_out>-lifnr
                                                               iv_bukrs    = 'GKP'
                                                               io_person   = <ls_person>-person
                                                               iv_due_date = iv_due_date
                                                               iv_big      = abap_true
                                                     IMPORTING et_error    = DATA(lt_error) ).
              IF lt_error IS INITIAL.
                /porr/cl_sma_vendor=>do_create_vendor( EXPORTING iv_lifnr    = <ls_out>-lifnr
                                                                 iv_bukrs    = ls_p0001-bukrs
                                                                 io_person   = <ls_person>-person
                                                                 iv_due_date = iv_due_date
                                                                 iv_big      = abap_false
                                                       IMPORTING et_error    = lt_error ).
              ENDIF.
            ENDIF.

            IF lt_error IS NOT INITIAL.
              <ls_out>-stat  = ac_fail.
              <ls_out>-txt   = 'Fehler bei der Anlage des Kreditor'(t01).
            ELSE.
              <ls_out>-stat  = ac_okay.
              <ls_out>-txt   = 'Kreditor wurde angelegt'(t02).
            ENDIF.
          ENDIF.
        CATCH /porr/cx_hr INTO DATA(lx_hr_empl).
*         Fülle die Restlichen Daten der ALV:
          <ls_out>-cname = ls_p0001-ename.
          <ls_out>-lifnr = get_lifnr( iv_pernr = <ls_person>-pernr ).
          <ls_out>-stat  = ac_fail.
          <ls_out>-txt   = 'Fehler bei der Anlage des Kreditor'(t01).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

*-----------------------------------------------------------------------------
*-     Gebe die Ausgabe (AT_OUT) in ALV aus
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
  METHOD do_display.

    DATA: lr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lr_columns   TYPE REF TO cl_salv_columns_table.

    DATA: lr_layout TYPE REF TO cl_salv_layout,
          ls_key    TYPE salv_s_layout_key.

    DATA: lr_alv TYPE REF TO cl_salv_table.

*... Instanz erzeugen
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lr_alv
          CHANGING
            t_table      = at_out.
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

* Funktionen
* ALV generische Funktionen aktivieren
    lr_functions = lr_alv->get_functions( ).
    lr_functions->set_default( abap_true ).
    lr_functions->set_all( abap_true ).

* Spalten setzen
    lr_columns = lr_alv->get_columns( ).
    lr_columns->set_optimize( abap_true ).

* Layout setzen
    lr_layout = lr_alv->get_layout( ).
    ls_key-report = sy-repid.
    lr_layout->set_key( ls_key ).
    lr_layout->set_default( abap_true ).
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).

* Überschrift ändern:
      lr_alv->get_display_settings( )->set_list_header( cond #( WHEN av_test = 'X' then 'Anlegen Kreditor - Testlauf'(T05)
                                                                when av_test = ' ' then 'Anlegen Kreditor'(T06) ) ).
    lr_alv->get_display_settings( )->set_list_header_size( cl_salv_display_settings=>c_header_size_large ).

* Tabelle anzeigen
    lr_alv->display( ).

  ENDMETHOD.

*-----------------------------------------------------------------------------
*- Erhalte die PerNr für die PNP
*-----------------------------------------------------------------------------
* [--->] Importing: iv_sel   TYPE p
* [--->]            iv_begda TYPE begda
* [--->]            iv_endda TYPE endda
* [<-()] Returning: rt_pnpid type
*-----------------------------------------------------------------------------
  METHOD get_pnp.
*   Über den Paramter iv_sel kann die Einschränkung der PNP gesteuert werden
*     1 = PNPCE Standard
*     2 = Einschränkung über Eintritt
*     3 = Einschränkung über Existenz Kreditor

    TYPES: BEGIN OF ty_pernr_wa,
             pernr TYPE pernr_d,
             pnalt TYPE p0032-pnalt,
             lifnr TYPE lfb1-lifnr,
           END OF ty_pernr_wa.
    TYPES: ty_pernr_tt TYPE TABLE OF ty_pernr_wa.

    DATA: lt_pernr TYPE ty_pernr_tt.

    CASE iv_sel.
      WHEN 1.
        " Da über SAP Standard, nichts zu tun.
      WHEN 2.
        " Suche alle Mitarbeiter, in diesem Zeitraum eingetreten sind
        SELECT DISTINCT pernr
          FROM pa0000
          INTO CORRESPONDING FIELDS OF TABLE lt_pernr
         WHERE massn = '01'
           AND begda BETWEEN iv_begda AND iv_endda
           AND sprps = abap_false.

        IF sy-subrc <> 0.
          lt_pernr = VALUE #( ( pernr = '0' ) ).
        ENDIF.
      WHEN 3.
        " Suche alle Mitarbeiter, wo keine Kreditoren angelegt wurden
        SELECT DISTINCT t1~pernr,
                        t1~pnalt,
                        t2~lifnr
          FROM pa0032 AS t1
          LEFT JOIN lfb1 AS t2 ON t1~pernr = t2~pernr
          INTO CORRESPONDING FIELDS OF TABLE @lt_pernr
         WHERE t1~pnalt IS NOT NULL.

        IF sy-subrc <> 0.
          lt_pernr = VALUE #( ( pernr = '0' pnalt = '0' ) ).
        ENDIF.

        DELETE lt_pernr WHERE pnalt IS INITIAL.
        DELETE lt_pernr WHERE lifnr IS NOT INITIAL.
      WHEN OTHERS.
    ENDCASE.

    LOOP AT lt_pernr ASSIGNING FIELD-SYMBOL(<ls_pernr>).
      APPEND INITIAL LINE TO rt_pnp ASSIGNING FIELD-SYMBOL(<ls_pnp>).
      <ls_pnp> = VALUE #( sign    = 'I'
                          option  = 'EQ'
                          low     = <ls_pernr>-pernr ).
    ENDLOOP.
  ENDMETHOD.

*-----------------------------------------------------------------------------
*- Erhalte die Lieferantennummer von der PerNr
*-----------------------------------------------------------------------------
* [--->] Importing: iv_pernr  type pernr_d
* [<-()] Returning: rv_lifnr  type lifnr
*-----------------------------------------------------------------------------
  METHOD get_lifnr.
    SELECT SINGLE lifnr FROM lfb1 INTO rv_lifnr WHERE pernr = iv_pernr.
  ENDMETHOD.

*-----------------------------------------------------------------------------
*- Erhalte die nächste Spesenkreditornummer
*-----------------------------------------------------------------------------
* [--->] Importing: iv_range     type INRI-NRRANGENR
* [<---] Exporting: ev_lifnr     type lifnr
*-----------------------------------------------------------------------------
  METHOD get_new_lifnr.
    DATA: lv_lif_num TYPE n LENGTH 5 VALUE '00000'.
    DATA: lv_nr_intern TYPE boolean,
          ls_interval  TYPE nriv.

* -------------------------------------------------------------------------
* Ermittle ob der Nummernkreis extern oder intern ist:
    CALL FUNCTION 'NUMBER_GET_INFO'
      EXPORTING
        nr_range_nr        = iv_range
        object             = 'KREDITOR'
      IMPORTING
        interval           = ls_interval
      EXCEPTIONS
        interval_not_found = 1
        object_not_found   = 2
        OTHERS             = 3.

    CHECK sy-subrc  = 0.
    lv_nr_intern = COND #( WHEN ls_interval-externind = 'X' THEN ' '
                           WHEN ls_interval-externind = ' ' THEN 'X' ).

* Besorge die nächste freie Nummer:
    CASE lv_nr_intern.
      WHEN abap_true.   "Interne Nummernvergabe
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = iv_range
            object                  = 'KREDITOR'
          IMPORTING
            number                  = rv_lifnr
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

      WHEN abap_false.  "Externe Nummernvergabe
        SELECT lifnr FROM lfa1 INTO TABLE @DATA(lt_lfa1).
        DELETE lt_lfa1 WHERE lifnr+0(2) <> 'SP'.

        SORT lt_lfa1 BY lifnr DESCENDING.
        READ TABLE lt_lfa1 INTO DATA(ls_lfa1) INDEX 1.

        DATA(lv_lif_typ) = ls_lfa1-lifnr+0(2).
        DATA(lv_length) = strlen( ls_lfa1-lifnr+2(5) ).
        CASE lv_length.
          WHEN 1.
            lv_lif_num = ls_lfa1-lifnr+2(1) && '0000'.
          WHEN 2.
            lv_lif_num = ls_lfa1-lifnr+2(2) && '000'.
          WHEN 3.
            lv_lif_num = ls_lfa1-lifnr+2(3) && '00'.
          WHEN 4.
            lv_lif_num = ls_lfa1-lifnr+2(4) && '0'.
          WHEN 5.
            lv_lif_num = ls_lfa1-lifnr+2(5).
        ENDCASE.
        ADD 1 TO lv_lif_num.

        rv_lifnr = lv_lif_typ && lv_lif_num.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.