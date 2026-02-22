*&---------------------------------------------------------------------*
*& Report  ZSM30LOG
*&
*&---------------------------------------------------------------------*
*& SM30 更改日志
*& Baitianzhen 2022-10-17
*&---------------------------------------------------------------------*
REPORT zsm30log2 NO STANDARD PAGE HEADING.

TYPE-POOLS: abap,slis.
TABLES: zsm30log.
DATA: fldct TYPE lvc_t_fcat,
      slayt TYPE lvc_s_layo,
      varnt TYPE disvariant,
      repid TYPE sy-repid,
      tsort TYPE lvc_t_sort WITH HEADER LINE.
DATA: it_dd03 TYPE TABLE OF dd03p WITH HEADER LINE.
DATA: BEGIN OF gt_log OCCURS 0,
        vname TYPE zsm30log-vname,
        tname TYPE zsm30log-tname,
        erdat TYPE zsm30log-erdat,
        stamp TYPE zsm30log-stamp,
        ernam TYPE zsm30log-ernam,
        dtype TYPE zsm30log-dtype,
        lines TYPE zsm30log-lines,
      END OF gt_log.
DATA: BEGIN OF gt_out OCCURS 0,
        vname TYPE zsm30log-vname,
        tname TYPE zsm30log-tname,
        erdat TYPE zsm30log-erdat,
        stamp TYPE zsm30log-stamp,
        ernam TYPE zsm30log-ernam,
        lined TYPE zsm30log-lines,
        linen TYPE zsm30log-lines,
        lineu TYPE zsm30log-lines,
      END OF gt_out.
DATA: BEGIN OF gs_keys,
        vname TYPE zsm30log-vname,
        erdat TYPE zsm30log-erdat,
        stamp TYPE zsm30log-stamp,
        dtype TYPE zsm30log-dtype,
      END OF gs_keys.
DATA: gv_tabnam TYPE dd02l-tabname.
DATA: cxroot TYPE REF TO cx_root,
      excmsg TYPE char255.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
SELECT-OPTIONS: s_vname FOR zsm30log-vname,
                s_tname FOR zsm30log-tname,
                s_erdat FOR zsm30log-erdat,
                s_ernam FOR zsm30log-ernam.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE btxt2.
PARAMETERS: p_nocvt AS CHECKBOX,
            p_tech  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据选择'.
  btxt2 = '选项'.
  %_s_vname_%_app_%-text = '维护视图名称'.
  %_s_tname_%_app_%-text = '表名称'.
  %_s_erdat_%_app_%-text = '修改日期'.
  %_s_ernam_%_app_%-text = '账号'.
  %_p_tech_%_app_%-text  = '列名显示字段名'.
  %_p_nocvt_%_app_%-text = '数据以外部格式显示'.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM outdata.

*&---------------------------------------------------------------------*
*&      Form  GETDATA
*&---------------------------------------------------------------------*
FORM getdata.
  CLEAR: gt_log[],gt_out[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_log
    FROM zsm30log
    WHERE vname IN s_vname AND
          tname IN s_tname AND
          erdat IN s_erdat AND
          ernam IN s_ernam AND
          srtf2 EQ 0.
  SORT gt_log BY vname erdat stamp.

  LOOP AT gt_log.
    CLEAR gt_out.
    READ TABLE gt_out WITH KEY vname = gt_log-vname
                               erdat = gt_log-erdat
                               stamp = gt_log-stamp
                               BINARY SEARCH.
    IF sy-subrc = 0.
      IF gt_log-dtype = 'D'.
        gt_out-lined = gt_log-lines.
      ELSEIF gt_log-dtype = 'N'.
        gt_out-linen = gt_log-lines.
      ELSE.
        gt_out-lineu = gt_log-lines.
      ENDIF.
      MODIFY gt_out INDEX sy-tabix.
    ELSE.
      gt_out-vname = gt_log-vname.
      gt_out-erdat = gt_log-erdat.
      gt_out-stamp = gt_log-stamp.
      gt_out-ernam = gt_log-ernam.
      gt_out-tname = gt_log-tname.
      IF gt_log-dtype = 'D'.
        gt_out-lined = gt_log-lines.
      ELSEIF gt_log-dtype = 'N'.
        gt_out-linen = gt_log-lines.
      ELSE.
        gt_out-lineu = gt_log-lines.
      ENDIF.
      INSERT gt_out INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GETDATA

*&---------------------------------------------------------------------*
*&      Form  outdata
*&---------------------------------------------------------------------*
FORM outdata.
  slayt-zebra      = 'X'.
  slayt-cwidth_opt = 'X'.
  slayt-no_keyfix  = 'X'.
  repid            = sy-repid.
  varnt-report     = sy-repid.

  tsort-spos = 1.
  tsort-fieldname = 'ERDAT'.
  tsort-down = 'X'.
  APPEND tsort.
  tsort-spos = 2.
  tsort-fieldname = 'STAMP'.
  tsort-down = 'X'.
  APPEND tsort.

  PERFORM catset TABLES fldct
                 USING: 'VNAME'   'ZSM30LOG' 'VNAME' '视图名',
                        'TNAME'   'ZSM30LOG' 'TNAME' '表名',
                        'ERDAT'   'ZSM30LOG' 'ERDAT' '修改日期',
                        'STAMP'   'ZSM30LOG' 'STAMP' '时间',
                        'ERNAM'   'ZSM30LOG' 'ERNAM' '账号',
                        'LINEN'   '        ' '     ' '插入行数',
                        'LINEU'   '        ' '     ' '修改行数',
                        'LINED'   '        ' '     ' '删除行数'.

  varnt-handle = 1.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc          = fldct[]
      it_sort_lvc              = tsort[]
      i_save                   = 'A'
      is_variant               = varnt
      is_layout_lvc            = slayt
      i_callback_program       = repid
      i_callback_user_command  = 'USRCMD1'
      i_callback_pf_status_set = 'SET_STATUS'
    TABLES
      t_outtab                 = gt_out.
ENDFORM.                    " outdata

*---------------------------------------------------------------------*
*       FORM catset                                            *
*---------------------------------------------------------------------*
FORM catset TABLES fldcat USING p_field p_reftab p_reffld p_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname  =  p_field.
  ls_fldcat-scrtext_l  =  p_text.
  ls_fldcat-coltext    =  p_text.
  ls_fldcat-ref_table  =  p_reftab.
  ls_fldcat-ref_field  =  p_reffld.
  ls_fldcat-col_opt    = 'A'.

  IF 'LINED LINEN LINEU' CS ls_fldcat-fieldname.
    ls_fldcat-hotspot = 'X'.
  ENDIF.

  APPEND ls_fldcat TO fldcat .
  CLEAR ls_fldcat .
ENDFORM.                    "catset

*&--------------------------------------------------------------------*
*&      Form  user_command
*&--------------------------------------------------------------------*
FORM usrcmd1 USING r_ucomm LIKE sy-ucomm
                    r_field TYPE slis_selfield.
  DATA: lv_title TYPE lvc_title.
  DATA: cl_stru  TYPE REF TO cl_abap_structdescr,
        cl_tabl  TYPE REF TO cl_abap_tabledescr,
        cl_elem  TYPE REF TO cl_abap_elemdescr,
        ref_data TYPE REF TO data.
  DATA: lt_dbtab TYPE REF TO data .
  FIELD-SYMBOLS: <lt_dbtab> TYPE STANDARD TABLE.

  CASE r_ucomm.
    WHEN '&IC1'. "双击
      CHECK r_field-tabindex <> 0 .
      READ TABLE gt_out INDEX r_field-tabindex.

      CASE r_field-fieldname.
        WHEN 'VNAME'.
          CALL FUNCTION 'RS_TOOL_ACCESS'
            EXPORTING
              operation    = 'SHOW'
              object_name  = gt_out-vname
              object_type  = 'TABL'
            EXCEPTIONS
              not_executed = 01.
        WHEN 'TNAME'.
          CALL FUNCTION 'RS_TOOL_ACCESS'
            EXPORTING
              operation    = 'SHOW'
              object_name  = gt_out-tname
              object_type  = 'TABL'
            EXCEPTIONS
              not_executed = 01.
        WHEN 'LINED' OR 'LINEN'.
          CASE r_field-fieldname.
            WHEN 'LINED'.
              CHECK gt_out-lined <> 0.
              lv_title = '删除数据'.
            WHEN 'LINEN'.
              CHECK gt_out-linen <> 0.
              lv_title = '插入数据'.
          ENDCASE.

          gs_keys-vname = gt_out-vname.
          gs_keys-erdat = gt_out-erdat.
          gs_keys-stamp = gt_out-stamp.
          gs_keys-dtype = r_field-fieldname+4(1). "D/N

          TRY.
              CREATE DATA lt_dbtab TYPE TABLE OF (gt_out-vname).
              ASSIGN lt_dbtab->* TO <lt_dbtab>.

              IMPORT tab = <lt_dbtab> FROM DATABASE zsm30log(z1) ID gs_keys.
            CATCH  cx_root INTO cxroot.
              excmsg =  cxroot->get_text( ).
              MESSAGE s000(oo) WITH excmsg(50) excmsg+50(50) excmsg+100.
              RETURN.
          ENDTRY.

          gv_tabnam = gs_keys-vname.
          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
            EXPORTING
              i_structure_name = gv_tabnam
              i_grid_title     = lv_title
              is_layout_lvc    = slayt
            TABLES
              t_outtab         = <lt_dbtab>
            EXCEPTIONS
              OTHERS           = 1.
        WHEN 'LINEU'.
          CHECK gt_out-lineu <> 0.
          PERFORM compare USING gt_out-vname gt_out-erdat gt_out-stamp.
      ENDCASE.
    WHEN 'REFRESH'.
      PERFORM getdata.
      r_field-row_stable = 'X'.
      r_field-col_stable = 'X'.
      r_field-refresh    = 'X'.
  ENDCASE.
ENDFORM.                    "USRCMD1

*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STD_FULL' EXCLUDING rt_extab.
ENDFORM.                    "SET_STATUS

*&---------------------------------------------------------------------*
*&      Form  compare
*&---------------------------------------------------------------------*
FORM compare USING vname erdat stamp.
  DATA: lt_lfcat TYPE lvc_t_fcat WITH HEADER LINE,
        lt_fldct TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        ls_slayt TYPE lvc_s_layo.
  DATA: lt_old TYPE REF TO data,
        lw_old TYPE REF TO data,
        lt_new TYPE REF TO data,
        lw_new TYPE REF TO data,
        lt_out TYPE REF TO data,
        lw_out TYPE REF TO data.
  DATA: lt_col TYPE TABLE OF lvc_s_scol WITH HEADER LINE.
  DATA: colint TYPE c VALUE '0'.
  DATA: keyln TYPE i.
  FIELD-SYMBOLS: <fs_told> TYPE STANDARD TABLE,
                 <fs_wold>,
                 <fs_tnew> TYPE STANDARD TABLE,
                 <fs_wnew>,
                 <fs_tout> TYPE STANDARD TABLE,
                 <fs_wout>,
                 <fs_opt>,<fs_col>,<fso>,<fsn>.
  FIELD-SYMBOLS: <table_x> TYPE x,
                 <key_x>   TYPE x.

  gv_tabnam = vname.
  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = gv_tabnam
      langu         = sy-langu
    TABLES
      dd03p_tab     = it_dd03
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF it_dd03[] IS INITIAL.
    MESSAGE e000(oo) WITH '获取表结构失败'.
  ELSE.
    DELETE it_dd03 WHERE datatype = ''.
    LOOP AT it_dd03 WHERE keyflag = 'X'.
      keyln = keyln + it_dd03-intlen.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = gv_tabnam
    CHANGING
      ct_fieldcat            = lt_fldct[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2.
  IF p_tech = 'X'.
    LOOP AT lt_fldct.
      lt_fldct-seltext_l    = lt_fldct-fieldname.
      lt_fldct-seltext_m    = lt_fldct-fieldname.
      lt_fldct-seltext_s    = lt_fldct-fieldname.
      lt_fldct-reptext_ddic = lt_fldct-fieldname.
      MODIFY lt_fldct.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fldct[]
    IMPORTING
      et_fieldcat_lvc = lt_lfcat[]
    TABLES
      it_data         = gt_out
    EXCEPTIONS
      it_data_missing = 1.

  CLEAR lt_lfcat.
  lt_lfcat-fieldname = 'ZOPRAT' .
  lt_lfcat-inttype   = 'C'.
  lt_lfcat-intlen    = 6 .
  lt_lfcat-col_pos   = 0 .
  lt_lfcat-coltext   = '数据'.
  lt_lfcat-reptext   = '数据'.
  INSERT lt_lfcat INDEX 1.

  CLEAR lt_lfcat.
  PERFORM catset TABLES lt_lfcat
                 USING 'CELLCOLOR' 'ETLOGHEAD_COLOR' 'CT' ''.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = lt_lfcat[]
    IMPORTING
      ep_table        = lt_out.
  ASSIGN lt_out->* TO <fs_tout>.
  CREATE DATA lw_out LIKE LINE OF <fs_tout>.
  ASSIGN lw_out->* TO <fs_wout>.

  CREATE DATA lt_old TYPE TABLE OF (vname).
  ASSIGN lt_old->* TO <fs_told>.
  CREATE DATA lw_old LIKE LINE OF <fs_told>.
  ASSIGN lw_old->* TO <fs_wold>.

  CREATE DATA lt_new TYPE TABLE OF (vname).
  ASSIGN lt_new->* TO <fs_tnew>.
  CREATE DATA lw_new LIKE LINE OF <fs_tnew>.
  ASSIGN lw_new->* TO <fs_wnew>.

  ASSIGN: <fs_wnew> TO <table_x> CASTING,
          <table_x>(keyln) TO <key_x>.

  TRY.
      gs_keys-vname = vname.
      gs_keys-erdat = erdat.
      gs_keys-stamp = stamp.

      gs_keys-dtype = 'B'.
      IMPORT tab = <fs_told> FROM DATABASE zsm30log(z1) ID gs_keys.
      SORT <fs_told>.

      gs_keys-dtype = 'A'.
      IMPORT tab = <fs_tnew> FROM DATABASE zsm30log(z1) ID gs_keys.
    CATCH  cx_root INTO cxroot.
      excmsg =  cxroot->get_text( ).
      MESSAGE s000(oo) WITH '导入数据出错:'
                            excmsg(50) excmsg+50(50) excmsg+100 .
      RETURN.
  ENDTRY.

  ASSIGN COMPONENT 'ZOPRAT' OF STRUCTURE <fs_wout> TO <fs_opt>.
  ASSIGN COMPONENT 'CELLCOLOR' OF STRUCTURE <fs_wout> TO <fs_col>.
  LOOP AT <fs_tnew> INTO <fs_wnew>.
    CLEAR lt_col[].
    CLEAR <fs_wold>.
    READ TABLE <fs_told> INTO <fs_wold> WITH KEY <key_x> BINARY SEARCH.
    DELETE <fs_told> INDEX sy-tabix.

    LOOP AT it_dd03 WHERE keyflag = ''.
      ASSIGN COMPONENT it_dd03-fieldname OF STRUCTURE <fs_wnew> TO <fsn>.
      ASSIGN COMPONENT it_dd03-fieldname OF STRUCTURE <fs_wold> TO <fso>.
      IF <fsn> <> <fso>.
        lt_col-fname = it_dd03-fieldname.
        lt_col-color-col = '3'.
        lt_col-color-int = colint.
        APPEND lt_col.
      ENDIF.
    ENDLOOP.
    TRANSLATE colint USING '0110'.

    <fs_col> = lt_col[].
    MOVE-CORRESPONDING <fs_wold> TO <fs_wout> .
    <fs_opt> = '改前'.
    APPEND <fs_wout> TO <fs_tout>.

    MOVE-CORRESPONDING <fs_wnew> TO <fs_wout> .
    <fs_opt> = '改后'.
    APPEND <fs_wout> TO <fs_tout>.
  ENDLOOP.

  ls_slayt-ctab_fname = 'CELLCOLOR'.
  ls_slayt-sel_mode   = 'D'.
  ls_slayt-detailinit = 'X'.

  DELETE lt_lfcat WHERE fieldname = 'CELLCOLOR'.
  LOOP AT lt_lfcat.
    IF p_nocvt = ''.
      lt_lfcat-no_convext = 'X'.
    ENDIF.
    lt_lfcat-no_out  = ''.
    lt_lfcat-col_opt = 'A'.
    MODIFY lt_lfcat.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_grid_title    = '修改数据'
      it_fieldcat_lvc = lt_lfcat[]
      is_layout_lvc   = ls_slayt
    TABLES
      t_outtab        = <fs_tout>
    EXCEPTIONS
      OTHERS          = 1.
ENDFORM.                    "compare
