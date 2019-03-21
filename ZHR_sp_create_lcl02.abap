class /PORR/CL_SMA_VENDOR definition
  public
  create public .

public section.

  data MV_LIFNR type LIFNR read-only .

  class-methods CREATE_VENDOR
    importing
      !IV_TAXNO type STRING
      !IV_PERNR type PERNR_D
    returning
      value(RO_VENDOR) type ref to /PORR/CL_SMA_VENDOR
    raising
      /PORR/CX_HR .
  methods GET_COMMON_DATA
    returning
      value(RS_LFA1) type LFA1 .
  methods FILL_ACCORDING_TAX_NO
    exporting
      !EV_VAT type STCEG
      !EV_TAXNO1 type STCD1 .
  methods GET_CC_DATA
    importing
      !IV_BUKRS type BUKRS
    returning
      value(RS_LFB1) type LFB1 .
  class-methods DO_CREATE_VENDOR
    importing
      !IV_AKONT type AKONT default '316350040'
      !IV_ZUAWA type DZUAWA default '012'
      !IV_BUKRS type BUKRS default 'GKP'
      !IO_PERSON type ref to /PORR/CL_HR_PP_PERSON
      !IV_REPRF type REPRF default ABAP_TRUE
      !IV_KTOKK type KTOKK default 'SOND'
      !IV_DUE_DATE type DATUM default SY-DATUM
      !IV_LIFNR type LIFNR
      !IV_BIG type BOOLEAN default ABAP_TRUE
    exporting
      !ET_ERROR type CFB_T_BDCMSGCOLL .
protected section.
private section.

  class-methods DO_CREATE_VENDOR_BUKRS
    importing
      !IS_BUKRS_ADR type BAPI0002_3 optional
      !IS_P0032 type P0032 optional
      !IS_P0002 type P0002 optional
      !IS_P0009 type P0009 optional
      !IV_AKONT type AKONT default '316350040'
      !IV_ZUAWA type DZUAWA default '012'
      !IV_BUKRS type BUKRS default 'GKP'
      !IV_REPRF type REPRF default ABAP_TRUE
      !IV_KTOKK type KTOKK default 'SOND'
      !IV_LIFNR type LIFNR
    returning
      value(RT_BDCDATA) type BDCDATA_TAB .
  class-methods DO_CREATE_VENDOR_GKP
    importing
      !IS_BUKRS_ADR type BAPI0002_3
      !IS_P0032 type P0032
      !IS_P0002 type P0002
      !IS_P0009 type P0009
      !IV_AKONT type AKONT
      !IV_ZUAWA type DZUAWA
      !IV_BUKRS type BUKRS
      !IV_REPRF type REPRF
      !IV_KTOKK type KTOKK
      !IV_LIFNR type LIFNR
    returning
      value(RT_BDCDATA) type BDCDATA_TAB .
ENDCLASS.



CLASS /PORR/CL_SMA_VENDOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /PORR/CL_SMA_VENDOR=>CREATE_VENDOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TAXNO                       TYPE        STRING
* | [--->] IV_PERNR                       TYPE        PERNR_D
* | [<-()] RO_VENDOR                      TYPE REF TO /PORR/CL_SMA_VENDOR
* | [!CX!] /PORR/CX_HR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_vendor.
    DATA lv_stcd1 TYPE lfa1-stcd1.
    DATA lt_lfa1  TYPE TABLE OF lfa1.
    DATA lt_lfas  TYPE TABLE OF lfas.
    DATA lv_stceg TYPE lfas-stceg.

    ro_vendor = NEW #( ).

    lv_stceg = iv_taxno.

    DATA(lo_person) = /porr/cl_hr_pp_person=>create_person(
                        EXPORTING
                          iv_pernr = CONV pernr_d( iv_pernr ) ).

    DATA(ls_p0001) = lo_person->get_s_p0001( ).

    "zuerst über VAT Tabelle nach Kreditor suchen
    IF NOT lv_stceg IS INITIAL.
      /porr/cl_hr_ddif=>get_instance( 'LFAS' )->get_table_by_names(
        EXPORTING
          iv_field1                    = 'STCEG'
          iv_par1                      = lv_stceg
        IMPORTING
          et_data                      = lt_lfas ).

      LOOP AT lt_lfas ASSIGNING FIELD-SYMBOL(<ls_lfas>).
        ro_vendor->mv_lifnr = <ls_lfas>-lifnr.

        DATA(ls_lfa1) = ro_vendor->get_common_data( ).
        DATA(ls_lfb1) = ro_vendor->get_cc_data( iv_bukrs = ls_p0001-bukrs ). "hier muss noch der Bukrs übergeben werden

        IF  ls_lfa1-sperr IS INITIAL
        AND ls_lfb1-sperr IS INITIAL.
          EXIT.
        ELSE.
          CLEAR ro_vendor->mv_lifnr.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ro_vendor->mv_lifnr IS INITIAL.
      "Lieferant zu Kreditor konnte nicht gefunden werden
      "Suche als NIP
      lv_stcd1 = iv_taxno.

      IF NOT lv_stcd1 IS INITIAL.
        /porr/cl_hr_ddif=>get_instance( 'LFA1' )->get_table_by_names(
          EXPORTING
            iv_field1                    = 'STCD1'
            iv_par1                      = lv_stcd1
          IMPORTING
            et_data                      = lt_lfa1 ).
      ENDIF.

      LOOP AT lt_lfa1 ASSIGNING FIELD-SYMBOL(<ls_lfa1>).
        ro_vendor->mv_lifnr = <ls_lfa1>-lifnr.

        ls_lfb1 = ro_vendor->get_cc_data( iv_bukrs = ls_p0001-bukrs ). "hier muss noch der Bukrs übergeben werden

        IF  <ls_lfa1>-sperr IS INITIAL
        AND ls_lfb1-sperr   IS INITIAL.
          EXIT.
        ELSE.
          CLEAR ro_vendor->mv_lifnr.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ro_vendor->mv_lifnr IS INITIAL.
      "Lieferant zur Steuernummer &1 nicht gefunden - bitte Daten eingeben.
      /porr/cx_hr=>create_simple(
        iv_msgid    = '/PORR/SMA_EXP'
        iv_msgno    = '023'
        iv_msgv1    = iv_taxno ).

      MESSAGE e023(/porr/sma_exp) INTO DATA(lv_msgv1).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /PORR/CL_SMA_VENDOR=>DO_CREATE_VENDOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_AKONT                       TYPE        AKONT (default ='316350040')
* | [--->] IV_ZUAWA                       TYPE        DZUAWA (default ='012')
* | [--->] IV_BUKRS                       TYPE        BUKRS (default ='GKP')
* | [--->] IO_PERSON                      TYPE REF TO /PORR/CL_HR_PP_PERSON
* | [--->] IV_REPRF                       TYPE        REPRF (default =ABAP_TRUE)
* | [--->] IV_KTOKK                       TYPE        KTOKK (default ='SOND')
* | [--->] IV_DUE_DATE                    TYPE        DATUM (default =SY-DATUM)
* | [--->] IV_LIFNR                       TYPE        LIFNR
* | [--->] IV_BIG                         TYPE        BOOLEAN (default =ABAP_TRUE)
* | [<---] ET_ERROR                       TYPE        CFB_T_BDCMSGCOLL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD do_create_vendor.

    DATA: ls_bukrs_adr TYPE bapi0002_3,
          lt_bdcdata   TYPE TABLE OF bdcdata,
          ls_bdcopt    TYPE ctu_params.
*          lt_bdcerr    TYPE

*   Besorge Daten des Mitarbeiters:
    DATA(ls_p0001) = io_person->get_s_p0001( iv_due_date = iv_due_date ).
    DATA(ls_p0002) = io_person->get_s_p0002( iv_due_date = iv_due_date ).
    DATA(ls_p0009) = io_person->get_s_p0009( iv_due_date = iv_due_date ).
    DATA(ls_p0032) = io_person->get_s_p0032( iv_due_date = iv_due_date ).

*   Besorge Daten aus dem Buchungskreis:
    CALL FUNCTION 'BAPI_COMPANYCODE_GETDETAIL'
      EXPORTING
        companycodeid       = ls_p0001-bukrs
      IMPORTING
        companycode_address = ls_bukrs_adr.

* Prüfe, ob alle Werte gefunden wurden:
    IF ls_p0001     IS INITIAL. et_error = VALUE #( ( msgtyp = 'E' ) ). ENDIF.
    IF ls_p0002     IS INITIAL. et_error = VALUE #( ( msgtyp = 'E' ) ). ENDIF.
    IF ls_p0009     IS INITIAL. et_error = VALUE #( ( msgtyp = 'E' ) ). ENDIF.
    IF ls_p0032     IS INITIAL. et_error = VALUE #( ( msgtyp = 'E' ) ). ENDIF.
    IF ls_bukrs_adr IS INITIAL. et_error = VALUE #( ( msgtyp = 'E' ) ). ENDIF.

    CHECK et_error IS INITIAL.

*   Überprüfe, ob ein Kreditor vorhanden ist:
    SELECT SINGLE * FROM lfb1 INTO @DATA(ls_lfb1) WHERE lifnr = @iv_lifnr
                                                    AND bukrs = @iv_bukrs.

    IF sy-subrc = 0.
      CHECK 1 = 0.
    ENDIF.

    IF iv_big = abap_true.
      lt_bdcdata = do_create_vendor_gkp( is_bukrs_adr = ls_bukrs_adr     " Includestruktur mit Namensattributen der ADRC-Adresse
                                         is_p0032     = ls_p0032         " Personal-Stammsatz Infotyp 0032 (Betriebsint.Vereinb.)
                                         is_p0002     = ls_p0002         " Personal-Stammsatz Infotyp 0002 (Daten zur Person)
                                         is_p0009     = ls_p0009         " Personal-Stammsatz Infotyp 0009 (Bankverbindung)
                                         iv_akont     = iv_akont
                                         iv_zuawa     = iv_zuawa
                                         iv_bukrs     = iv_bukrs
                                         iv_reprf     = iv_reprf
                                         iv_ktokk     = iv_ktokk
                                         iv_lifnr     = iv_lifnr ).
    ELSE.
      lt_bdcdata = do_create_vendor_bukrs( is_bukrs_adr = ls_bukrs_adr     " Includestruktur mit Namensattributen der ADRC-Adresse
                                           is_p0032     = ls_p0032         " Personal-Stammsatz Infotyp 0032 (Betriebsint.Vereinb.)
                                           is_p0002     = ls_p0002         " Personal-Stammsatz Infotyp 0002 (Daten zur Person)
                                           is_p0009     = ls_p0009         " Personal-Stammsatz Infotyp 0009 (Bankverbindung)
                                           iv_akont     = iv_akont
                                           iv_zuawa     = iv_zuawa
                                           iv_bukrs     = iv_bukrs
                                           iv_reprf     = iv_reprf
                                           iv_ktokk     = iv_ktokk
                                           iv_lifnr     = iv_lifnr ).
    ENDIF.

    ls_bdcopt = VALUE #(  dismode = 'N'
                          defsize = 'X' ).

    CALL TRANSACTION 'FK01' USING lt_bdcdata OPTIONS FROM ls_bdcopt MESSAGES INTO et_error.

    DELETE et_error WHERE msgtyp = ' '
                       OR msgtyp = 'S'
                       OR msgtyp = 'I'
                       OR msgtyp = 'W'.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method /PORR/CL_SMA_VENDOR=>DO_CREATE_VENDOR_BUKRS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BUKRS_ADR                   TYPE        BAPI0002_3(optional)
* | [--->] IS_P0032                       TYPE        P0032(optional)
* | [--->] IS_P0002                       TYPE        P0002(optional)
* | [--->] IS_P0009                       TYPE        P0009(optional)
* | [--->] IV_AKONT                       TYPE        AKONT (default ='316350040')
* | [--->] IV_ZUAWA                       TYPE        DZUAWA (default ='012')
* | [--->] IV_BUKRS                       TYPE        BUKRS (default ='GKP')
* | [--->] IV_REPRF                       TYPE        REPRF (default =ABAP_TRUE)
* | [--->] IV_KTOKK                       TYPE        KTOKK (default ='SOND')
* | [--->] IV_LIFNR                       TYPE        LIFNR
* | [<-()] RT_BDCDATA                     TYPE        BDCDATA_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DO_CREATE_VENDOR_BUKRS.

*   Stelle die Daten in den jeweiligen Variablen
    DATA(lv_pernr)    = is_p0032-pernr.

    rt_bdcdata = VALUE #( ( program = 'SAPMF02K'          dynpro  = '0105'   dynbegin = abap_true )
                          ( fnam    = 'RF02K-LIFNR'       fval    = CONV #( iv_lifnr )            )
                          ( fnam    = 'RF02K-BUKRS'       fval    = CONV #( iv_bukrs )            )
                          ( fnam    = 'RF02K-KTOKK'       fval    = CONV #( iv_ktokk )            )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=ENTR'                       )

                          ( program = 'SAPMF02K'          dynpro  = '0210'   dynbegin = abap_true )
                          ( fnam    = 'LFB1-AKONT'        fval    = CONV #( iv_akont )            )
                          ( fnam    = 'LFB1-ZUAWA'        fval    = CONV #( iv_zuawa )            )
                          ( fnam    = 'LFB1-PERNR'        fval    = CONV #( lv_pernr )            )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0215'   dynbegin = abap_true )
                          ( fnam    = 'LFB1-REPRF'        fval    = CONV #( iv_reprf )            )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0220'   dynbegin = abap_true )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0610'   dynbegin = abap_true )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( fnam    = 'BDC_OKCODE'        fval    = '=UPDA'                       )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=ENTR'                       ) ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method /PORR/CL_SMA_VENDOR=>DO_CREATE_VENDOR_GKP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BUKRS_ADR                   TYPE        BAPI0002_3
* | [--->] IS_P0032                       TYPE        P0032
* | [--->] IS_P0002                       TYPE        P0002
* | [--->] IS_P0009                       TYPE        P0009
* | [--->] IV_AKONT                       TYPE        AKONT
* | [--->] IV_ZUAWA                       TYPE        DZUAWA
* | [--->] IV_BUKRS                       TYPE        BUKRS
* | [--->] IV_REPRF                       TYPE        REPRF
* | [--->] IV_KTOKK                       TYPE        KTOKK
* | [--->] IV_LIFNR                       TYPE        LIFNR
* | [<-()] RT_BDCDATA                     TYPE        BDCDATA_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DO_CREATE_VENDOR_GKP.

*   Stelle die Daten in den jeweiligen Variablen
    DATA(lv_pernr)    = is_p0032-pernr.
    data(lv_pnalt)    = is_p0032-pnalt.
    DATA(lv_name)     = is_p0002-vorna && | | && is_p0002-nachn &&  |/SPESEN|.
    DATA(lv_street)   = is_bukrs_adr-street.
    DATA(lv_postc)    = is_bukrs_adr-postl_cod1.
    DATA(lv_city)     = is_bukrs_adr-city.
    DATA(lv_country)  = is_bukrs_adr-country.
    data(lv_iban01)   = is_p0009-iban+0(4).
    data(lv_iban02)   = is_p0009-iban+4(4).
    data(lv_iban03)   = is_p0009-iban+8(4).
    data(lv_iban04)   = is_p0009-iban+12(4).
    data(lv_iban05)   = is_p0009-iban+16(4).
    data(lv_iban06)   = is_p0009-iban+20(4).
    data(lv_iban07)   = is_p0009-iban+24(4).
    data(lv_iban08)   = is_p0009-iban+28(4).

    rt_bdcdata = VALUE #( ( program = 'SAPMF02K'          dynpro  = '0105'   dynbegin = abap_true )
                          ( fnam    = 'RF02K-LIFNR'       fval    = CONV #( iv_lifnr )            )
                          ( fnam    = 'RF02K-BUKRS'       fval    = CONV #( iv_bukrs )            )
                          ( fnam    = 'RF02K-KTOKK'       fval    = CONV #( iv_ktokk )            )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=ENTR'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0110'   dynbegin = abap_true )
                          ( fnam    = 'LFA1-NAME1'        fval    = CONV #( lv_name )             )
                          ( fnam    = 'LFA1-NAME2'        fval    = CONV #( lv_pnalt )            )
*                          ( fnam    = 'LFA1-SORTL'        fval    = CONV #( lv_name )             )
                          ( fnam    = 'LFA1-STRAS'        fval    = CONV #( lv_street )           )
                          ( fnam    = 'LFA1-ORT01'        fval    = CONV #( lv_city )             )
                          ( fnam    = 'LFA1-PSTLZ'        fval    = CONV #( lv_postc )            )
                          ( fnam    = 'LFA1-LAND1'        fval    = CONV #( lv_country )          )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0120'   dynbegin = abap_true )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0130'   dynbegin = abap_true )
                          ( fnam    = 'BDC_CURSOR'        fval    = 'IBAN(01)'                    )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=IBAN'                       )

                          ( program = 'SAPLIBMA'          dynpro  = '0200'   dynbegin = abap_true )
                          ( fnam    = 'IBAN01'            fval    = conv #( lv_iban01 )           )
                          ( fnam    = 'IBAN02'            fval    = conv #( lv_iban02 )           )
                          ( fnam    = 'IBAN03'            fval    = conv #( lv_iban03 )           )
                          ( fnam    = 'IBAN04'            fval    = conv #( lv_iban04 )           )
                          ( fnam    = 'IBAN05'            fval    = conv #( lv_iban05 )           )
                          ( fnam    = 'IBAN06'            fval    = conv #( lv_iban06 )           )
                          ( fnam    = 'IBAN07'            fval    = conv #( lv_iban07 )           )
                          ( fnam    = 'IBAN08'            fval    = conv #( lv_iban08 )           )
                          ( fnam    = 'G_ACCNO_UNKNOWN'   fval    = 'X'                           )
                          ( fnam    = 'BDC_CURSOR'        fval    = 'IBAN01'                      )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=ENTR'                       )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=ENTR'                       )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0130'   dynbegin = abap_true )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0380'   dynbegin = abap_true )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0210'   dynbegin = abap_true )
                          ( fnam    = 'LFB1-AKONT'        fval    = CONV #( iv_akont )            )
                          ( fnam    = 'LFB1-ZUAWA'        fval    = CONV #( iv_zuawa )            )
                          ( fnam    = 'LFB1-PERNR'        fval    = CONV #( lv_pernr )            )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0215'   dynbegin = abap_true )
                          ( fnam    = 'LFB1-REPRF'        fval    = CONV #( iv_reprf )            )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( program = 'SAPMF02K'          dynpro  = '0220'   dynbegin = abap_true )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=VW'                         )

                          ( fnam    = 'BDC_OKCODE'        fval    = '=ENTR'                       )
                          ( fnam    = 'BDC_OKCODE'        fval    = '=UPDA'                       ) ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /PORR/CL_SMA_VENDOR->FILL_ACCORDING_TAX_NO
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_VAT                         TYPE        STCEG
* | [<---] EV_TAXNO1                      TYPE        STCD1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_according_tax_no.
    DATA ls_lfas TYPE lfas.

    CLEAR ev_vat.
    CLEAR ev_taxno1.

    "beide nummern prüfen ob gefüllt
    DATA(ls_common) = me->get_common_data( ).

    IF ls_common-stcd1 IS NOT INITIAL.
      ev_taxno1 = ls_common-stcd1.
    ELSE.
      /porr/cl_hr_ddif=>get_instance( 'LFAS' )->get_one_eq(
        EXPORTING
          iv_par1                      = me->mv_lifnr
        IMPORTING
          es_data                      = ls_lfas ).

      IF ls_lfas-stceg IS NOT INITIAL.
        ev_vat = ls_lfas-stceg.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /PORR/CL_SMA_VENDOR->GET_CC_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BUKRS                       TYPE        BUKRS
* | [<-()] RS_LFB1                        TYPE        LFB1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cc_data.
    /porr/cl_hr_ddif=>get_instance( 'LFB1' )->get_one_eq(
  EXPORTING
    iv_par1                      = me->mv_lifnr
    iv_par2                      = iv_bukrs
  IMPORTING
    es_data                      = rs_lfb1 ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /PORR/CL_SMA_VENDOR->GET_COMMON_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_LFA1                        TYPE        LFA1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_common_data.
    /porr/cl_hr_ddif=>get_instance( 'LFA1' )->get_one_eq(
      EXPORTING
        iv_par1                      = me->mv_lifnr
      IMPORTING
        es_data                      = rs_lfa1
    ).
  ENDMETHOD.
ENDCLASS.