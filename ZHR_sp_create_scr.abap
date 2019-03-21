*&---------------------------------------------------------------------*
*&  Include           ZHR_SP_CREATE_SCR
*&---------------------------------------------------------------------*
selection-SCREEN BEGIN OF block zsel WITH FRAME TITLE text-003.
  PARAMETERS p_sel1 RADIOBUTTON GROUP sel TYPE boolean DEFAULT 'X'.   " Standard Selektion -> PNPCE
  PARAMETERS p_sel2 RADIOBUTTON GROUP sel TYPE boolean.               " Selektionseinschränkung über Eintrittsdatum
  PARAMETERS p_sel3 RADIOBUTTON GROUP sel TYPE boolean.               " Selektionseinschränkung über existanz Kreditor
SELECTION-SCREEN END OF block zsel.

selection-SCREEN BEGIN OF block zfun WITH FRAME TITLE text-002.
  PARAMETERS p_test as CHECKBOX TYPE boolean DEFAULT abap_true.   " Starte Report in Testlauf
SELECTION-SCREEN END OF block zfun.