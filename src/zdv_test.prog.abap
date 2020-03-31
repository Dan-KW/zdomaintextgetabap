*&---------------------------------------------------------------------*
*& Report ZDV_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdv_test.
DATA: lv_t1          TYPE i,
      lv_t2          TYPE i,
      lv_domain1     TYPE bukrs VALUE '2000',
      lv_domain2     TYPE zemdv_el_table VALUE '',
      lv_domain      TYPE  zemdv_el_ranged     VALUE 'BB',
      lv_description TYPE ddtext.

DATA : lt_txt              TYPE crmt_uif_domain_texts_tab,
       lt_dd07             TYPE STANDARD TABLE OF dd07v,
       lt_dd07x            TYPE STANDARD TABLE OF dd07v.

DATA: it_dd07t TYPE STANDARD TABLE OF dd07t,
      wa_dd07t LIKE LINE OF it_dd07t.

START-OF-SELECTION.
  GET RUN TIME FIELD lv_t1.
  lv_description = zcl_dv_domain_desc=>get_domain_descr( lv_domain1 ) .
*  lv_description = zcl_dv_domain_desc=>get_domain_descr( EXPORTING im_field = lv_domain1 im_langu = 'E' EXCEPTIONS internal ).
  WRITE : / 'Domain variable : ' , lv_description, 'For Value Table' .
  GET RUN TIME FIELD lv_t2.
  lv_t2 = ( lv_t2 - lv_t1 ).
  WRITE :/ 'Time', lv_t2.

  lv_description = zcl_dv_domain_desc=>get_domain_descr( lv_domain2 ).
  WRITE : / 'Domain variable : ' , lv_description, 'For Value Table' .
  GET RUN TIME FIELD lv_t2.
  lv_t2 = ( lv_t2 - lv_t1 ).
  WRITE :/ 'Time', lv_t2.

  lv_description = zcl_dv_domain_desc=>get_domain_descr( lv_domain ).
  WRITE : / 'Domain variable : ' , lv_description, 'Value range' .
  GET RUN TIME FIELD lv_t2.
  lv_t2 = ( lv_t2 - lv_t1 ).
  WRITE :/ 'Time', lv_t2.
