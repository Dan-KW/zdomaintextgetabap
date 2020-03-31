class ZCL_DV_DOMAIN_DESC definition
  public
  final
  create public .

public section.

  class-methods GET_DOMAIN_DESCR
    importing
      !IM_FIELD type ANY
      !IM_LANGU type SY-LANGU default SY-LANGU
      !IM_FNAME type FIELDNAME optional
    returning
      value(RV_DESC_TXT) type DDTEXT
    exceptions
      NO_DOMAIN_VALUES
      NO_TEXT_FOR_VALUE
      NO_TEXT_IN_TABLE
      INTERNAL_ERROR .
  class-methods CHECK_IF_TEXTTABLE_FOR_TABLE
    importing
      !TABNAME type TABNAME
    exporting
      !TEXTTABLE_EXISTS type FLAG_X
      !TEXTTABLE type TABNAME
    exceptions
      INTERNAL_ERROR .
  class-methods GET_DDIC_INFO
    importing
      !TABNAME type TABNAME
    exporting
      !DDIC_INFO type RSTI_T_DFI
    exceptions
      INTERNAL_ERROR
      NOT_FOUND .
  class-methods CHECK_IF_VALUETABLE_FOR_DOMAIN
    importing
      !DOMAINNAME type DOMNAME
    exporting
      !VALUETABLE_EXISTS type FLAG_X
      !VALUETABLE type TABNAME
    exceptions
      INTERNAL_ERROR .
protected section.

  types:
    BEGIN OF t_s_texttable_buffer,
           tabname          TYPE tabname,
           texttable        TYPE tabname,
           texttable_exists TYPE flag,
         END OF  t_s_texttable_buffer .
  types:
    t_t_texttable_buffer TYPE HASHED TABLE OF t_s_texttable_buffer
                                   WITH UNIQUE KEY tabname .
    types:
    begin of t_s_ddic_info,
             tabname type tabname,
             dfies   type dfies_table,
    end of t_s_ddic_info .
private section.

    class-data g_buff_texttables TYPE t_t_texttable_buffer .
*  types:
*    class-data g_buff_texttables TYPE t_t_texttable_buffer .

  types:
*  TYPES:
*    class-data gt_buff_texttables TYPE t_t_texttable_buffer .
    BEGIN OF ty_buf,
      key  TYPE key,
      text TYPE string,
    END OF ty_buf .

  class-data T_BUFF_TEXTTABLES type T_T_TEXTTABLE_BUFFER .
  class-data:
    t_buf_ddic_tables TYPE HASHED TABLE OF tabname
                      WITH UNIQUE KEY table_line .
  class-data:
    t_buf_ddic_info TYPE SORTED TABLE OF t_s_ddic_info
                    WITH UNIQUE KEY tabname .
  class-data:
    t_buf_desctxt TYPE HASHED TABLE OF ty_buf WITH UNIQUE KEY key .
ENDCLASS.



CLASS ZCL_DV_DOMAIN_DESC IMPLEMENTATION.


  method check_if_texttable_for_table.
    data: ls_buf_texttable type t_s_texttable_buffer.


    clear texttable_exists.
    clear texttable.

    "<<< note 1666312
    read table t_buff_texttables into ls_buf_texttable
      with table key tabname = tabname.

    if sy-subrc = 0.
      texttable = ls_buf_texttable-texttable.
      texttable_exists = ls_buf_texttable-texttable_exists.
      return.
    endif.
    ">>> note 1666312

    call function 'DDUT_TEXTTABLE_GET'
      exporting
        tabname   = tabname
      importing
        texttable = texttable
      exceptions
        others    = 1.

    if sy-subrc <> 0.
      raise internal_error.
    endif.

    if not texttable is initial.
      texttable_exists = 'X'.
    endif.

    "<<< note 1666312
    ls_buf_texttable-tabname = tabname.
    ls_buf_texttable-texttable = texttable.
    ls_buf_texttable-texttable_exists = texttable_exists.
    insert ls_buf_texttable into table t_buff_texttables.
    ">>> note 1666312
  endmethod.


  method check_if_valuetable_for_domain .

    types: begin of t_s_domainvaluetables,
             domainname        type domname,
             valuetable_exists type flag_x,
             valuetable        type tabname,
           end of t_s_domainvaluetables.

    statics: domainvaluetables type hashed table of t_s_domainvaluetables
                               with unique key domainname.

    data: domain_info          type dd01v,
          name                 type ddobjname,
          domainvaluetables_wa type t_s_domainvaluetables.


* check, if we have information in buffer
    read table domainvaluetables into domainvaluetables_wa
               with table key domainname = domainname.
    if sy-subrc = 0.
      valuetable_exists = domainvaluetables_wa-valuetable_exists.
      valuetable = domainvaluetables_wa-valuetable.
      return.
    endif.


    clear valuetable_exists.
    clear valuetable.


    name = domainname.

    call function 'DDIF_DOMA_GET'
      exporting
        name          = name
      importing
        dd01v_wa      = domain_info
      exceptions
        illegal_input = 1
        others        = 2.

    if sy-subrc <> 0.
      raise internal_error.
    endif.

    if not domain_info-entitytab is initial.
      valuetable_exists = 'X'.
      valuetable = domain_info-entitytab.
    endif.

* write information to buffer
    domainvaluetables_wa-domainname = domainname.
    domainvaluetables_wa-valuetable_exists = valuetable_exists.
    domainvaluetables_wa-valuetable = valuetable.
    insert domainvaluetables_wa into table domainvaluetables.

  endmethod.


method get_ddic_info .

  data: ddif_tabname    type ddobjname,
        l_buf_ddic_info type t_s_ddic_info.                 "Y6BK093981

  clear ddic_info[].

  ddif_tabname = tabname.

* check, if we have information in buffer
  read table t_buf_ddic_tables transporting no fields
             with table key table_line = tabname.
  if sy-subrc = 0.
* >>> Y6BK093981
    read table t_buf_ddic_info into l_buf_ddic_info
         with table key tabname = tabname.
    ddic_info = l_buf_ddic_info-dfies.
* <<< Y6BK093981
    return.
  endif.

* cannot use DDIF_NAMETAB_GET, because we need domains
  call function 'DDIF_FIELDINFO_GET'
    exporting
      tabname        = ddif_tabname
    tables
      dfies_tab      = ddic_info
    exceptions
      not_found      = 1
      internal_error = 2
      others         = 3.

  if sy-subrc = 1. "NOT_FOUND
    raise not_found.
  elseif sy-subrc <> 0.
    raise internal_error.
  endif.

* store information in buffer
* there might be information for single fields of tabname-> delete them
  delete t_buf_ddic_info where tabname = tabname.
* >>> Y6BK093981
  l_buf_ddic_info-tabname = tabname.
  l_buf_ddic_info-dfies   = ddic_info.
  insert l_buf_ddic_info into table t_buf_ddic_info.
* <<< Y6BK093981
  insert tabname into table t_buf_ddic_tables.

endmethod.


  method get_domain_descr.
    types : lr_val     type range of text40.
    data lt_txt type standard table of ty_buf.

    field-symbols: <lt_table_structure> type table,
                   <ls_table_structure> type any.
    data : lo_element                type ref to cl_abap_elemdescr,
           lv_vtablename             type tabname,
           lv_eflag                  type flag_x,
           lt_ddic_info              type rsti_t_dfi,
           lt_text_identifier_result type txid_t_text_identifier_result,
           lv_txttab                 type dd08v-tabname,
           lv_checkfld               type tabname.

    data: lt_spec_params          type tfieldval,
          lif_text_identifier_obj type ref to if_text_identifier,
          lv_fieldname            type fieldname,
          lv_fields               type string,
          lv_join_condition       type string,
          lv_join_field           type string.
    data: lo_st_text_identifier_obj type ref to object, "These ones will be static variables in global class
          lx_cx                     type ref to cx_dynamic_check,
          lv_lang                   type fieldname,
          lv_lang_txt               type fieldname,
          lv_where                  type string,
          lv_lang_code              type langu.
    statics:lt_values             type ddfixvalues,
            lt_st_last_vsourcetab type tfieldval,
            lv_st_last_vsourcetab type tabname,
            lv_domainname         type domname.
    create object lo_st_text_identifier_obj type cl_text_identifier.
    "Get the name and description of the domain
    lo_element ?= cl_abap_typedescr=>describe_by_data( im_field ) .
    data(ls_field) = lo_element->get_ddic_field( ).
    "Check if value table exists in the domain
    zcl_dv_domain_desc=>check_if_valuetable_for_domain( exporting domainname = ls_field-domname importing valuetable = lv_vtablename valuetable_exists = lv_eflag ) .
    "If it exists then get values from it.
    if lv_eflag = 'X'.

      "If reading from the same table several times, then use buffer
      if lv_st_last_vsourcetab <> lv_vtablename and lv_lang_code <> im_langu.
        create object lo_st_text_identifier_obj type cl_text_identifier.
        lif_text_identifier_obj ?= lo_st_text_identifier_obj.
        "get domain info, define what field is key and has same domain as given domain
        zcl_dv_domain_desc=>get_ddic_info( exporting tabname = lv_vtablename importing ddic_info = lt_ddic_info ).
        loop at lt_ddic_info assigning field-symbol(<lfs_ddic_info>) where keyflag = 'X' and domname = ls_field-domname.
          lv_fieldname = <lfs_ddic_info>-fieldname.
          exit.
        endloop.
        "get name language field if it exists
        loop at lt_ddic_info assigning <lfs_ddic_info> where datatype = 'LANG'.
          lv_lang = <lfs_ddic_info>-fieldname.
          exit.
        endloop.
        lif_text_identifier_obj->identify_text( exporting tabname = lv_vtablename fieldname = lv_fieldname special_parameters = lt_spec_params valuesource_tables = lt_st_last_vsourcetab
                                                importing text_identifier_results = lt_text_identifier_result
                                                exceptions internal_error = 1 illegal_call = 2 illegal_table = 3 illegal_field = 4 special_parameter_mismatch = 5 others = 6 ).

        check sy-subrc = 0.
        " get the name of the field that contains text
        if lt_text_identifier_result is not initial.
          "check if value table has a text table
          call function 'DDUT_TEXTTABLE_GET'
            exporting
              tabname    = lv_vtablename
            importing
              texttable  = lv_txttab
              checkfield = lv_checkfld.

          "if no text table
          if lv_txttab is initial.
            read table lt_text_identifier_result assigning field-symbol(<fs_res>) index 1.
            if sy-subrc = 0 and <fs_res>-textsource_field is not initial.
              try.
                  "Put field names in an internal table for use in the dynamic query
                  lv_fields = |{ lv_fieldname } AS key, { <fs_res>-textsource_target }~{ <fs_res>-textsource_field } AS text |.
                  if lv_lang is initial.
                    select (lv_fields)"(lt_fields)
                      into  table @lt_txt
                      from (lv_vtablename).
                  else.                           "if there's language field
                    if im_langu is supplied.
                      lv_where = |{ lv_lang } = '{ im_langu }'|.
                      lv_lang_code = im_langu.
                    else.
                      lv_where = |{ lv_lang } = '{ sy-langu }'|.
                      lv_lang_code = sy-langu.
                    endif.
                    select (lv_fields)"(lt_fields)
                      into  table @lt_txt
                      from (lv_vtablename)
                      where (lv_where).
                  endif.
                catch cx_sy_dynamic_osql_semantics into lx_cx.
                  "RAISE Exception
              endtry.
              .
            endif.
          else. " IF text table exists

            "get ddic info about the field
            clear lt_ddic_info.
            get_ddic_info( exporting tabname = lv_txttab importing ddic_info = lt_ddic_info ).
            "get name language field if it exists, get the last field
            loop at lt_ddic_info assigning <lfs_ddic_info> where datatype = 'LANG'.
              lv_lang_txt = <lfs_ddic_info>-fieldname.
              exit.
            endloop.
            " get join fields infromation
            read table lt_text_identifier_result assigning field-symbol(<lfs_res>) index 1.
            if sy-subrc = 0.
              loop at <lfs_res>-text_table_key_desc assigning field-symbol(<lfs_key_desc>) where matching_fieldname = lv_fieldname. "must be there
                lv_join_field = <lfs_key_desc>-fieldname.
              endloop.
              if sy-subrc <> 0.
                raise internal_error.
              endif.
              try .
                  lv_fields         = | { lv_vtablename }~{ lv_fieldname } AS key, { <lfs_res>-textsource_target }~{ <lfs_res>-textsource_field } AS text |.
                  lv_join_condition = | { lv_vtablename } INNER JOIN { <lfs_res>-textsource_target } ON { lv_vtablename }~{ <lfs_key_desc>-matching_fieldname } = { <lfs_res>-textsource_target }~{ lv_join_field } |.
                  if im_langu is supplied.
                    lv_where = |{ <lfs_res>-textsource_target }~{ lv_lang_txt } = '{ im_langu }' AND { lv_vtablename }~{ lv_fieldname } <> @space |.
                    lv_lang_code = im_langu.
                  else.
                    lv_where = |{ <lfs_res>-textsource_target }~{ lv_lang_txt } = '{ sy-langu }' AND { lv_vtablename }~{ lv_fieldname } <> @space |.
                    lv_lang_code = sy-langu.
                  endif.
                  select distinct (lv_fields)"(lt_fields)
                    into  table @lt_txt
                    from (lv_join_condition)
                    where (lv_where).

                catch cx_sy_dynamic_osql_semantics into lx_cx.
                  raise internal_error.
              endtry.
            else.
              raise internal_error.
            endif.
          endif.
          sort lt_txt by key.
          delete adjacent duplicates from lt_txt comparing key.
          t_buf_desctxt = lt_txt.
*          LOOP AT lt_text_identifier_result ASSIGNING FIELD-SYMBOL(<fs_res>).
*
*          ENDLOOP.
        endif.
        if lv_txttab is not initial.
*          SELECT lv_fieldname,
        endif.

      endif.
      read table t_buf_desctxt assigning field-symbol(<fs_st_buffer>) with table  key key = im_field.
      if sy-subrc = 0.
        rv_desc_txt = <fs_st_buffer>-text.
      endif.
    else. "get from value from range
      if  ls_field-domname ne lv_domainname. " get domain fixed or range values only if domain is different.
        lt_values =  lo_element->get_ddic_fixed_values( im_langu ) .
        lv_domainname = ls_field-domname.
      endif.

      if lt_values is not initial.
        loop at lt_values assigning field-symbol(<fs_value>).
          if im_field in value lr_val( ( sign = 'I' option = <fs_value>-option low = <fs_value>-low high = <fs_value>-high ) ) .
            rv_desc_txt = <fs_value>-ddtext.
            exit.
          endif.
        endloop.
      else.
        raise  no_domain_values.
      endif.
    endif.
    if rv_desc_txt is initial.
      raise no_text_for_value.
    endif.
  endmethod.
ENDCLASS.
