*&---------------------------------------------------------------------*
*程序描述: 通用程序,给SM30增加排序/查找/导出/导入/批量修改/记录更改日志等功能
*说明:需要把表做一个维护视图，不支持直接对表做SM30，也不支持视图簇
*----------------------------------------------------------------------*
*Baitianzhen 2019-7-24
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* 灰掉定位按钮
* 在排序后,定位按钮会失效
*----------------------------------------------------------------------*
MODULE hide_position_button OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'VIM_POSI_PUSH'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                    "hide_position_button OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZSM30ADDON_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE zsm30addon_command INPUT.
  IF function(4) = 'SEAR'.
    PERFORM tc_search.
  ELSEIF function(4) = 'SORT'.
    PERFORM sort_table.
  ELSEIF function(4) = 'EXPT'.
    PERFORM export.
  ELSEIF function = 'EXPS'.
    PERFORM export_tmpl.
  ELSEIF function = 'UPLD'.
    PERFORM upload.
  ELSEIF function = 'BATMOD'.
    PERFORM batch_change.
  ELSEIF function = 'CLIP'.
    PERFORM paste.
  ENDIF.
ENDMODULE.                    "zsm30addon_command INPUT

*&---------------------------------------------------------------------*
*& 批量更改指定列的值
*& 选择需要批量更改的行,然后把光标放到需要更改的列内,按批量更改按钮
*& 如果没有选择行,则更改所有的行的指定列的值
*&---------------------------------------------------------------------*
FORM batch_change.
  DATA: lv_fldnam TYPE char30.
  DATA: ls_column TYPE cxtab_column.
  DATA: lv_string TYPE string.
  FIELD-SYMBOLS: <lv_fldval> TYPE any.

  READ TABLE <vim_tctrl>-cols INTO ls_column
                              WITH KEY screen-name = f.
  IF sy-subrc NE 0 OR ls_column-screen-input = 0.
    RETURN.
  ENDIF.

  SPLIT f AT '-' INTO lv_fldnam lv_fldnam.
  PERFORM popup_getvalue USING vim_view_name lv_fldnam '' ''
                         CHANGING lv_string sy-subrc.
  CHECK sy-subrc = 0.

  LOOP AT extract.
    IF mark_extract > 0.
      CHECK <xmark> = 'M'.
    ENDIF.

    ASSIGN COMPONENT lv_fldnam OF STRUCTURE extract TO <lv_fldval>.
    CHECK sy-subrc = 0.
    <lv_fldval> = lv_string.

    IF <xact> NE 'N'.
      <xact> = 'U'.
    ENDIF.
    <status>-upd_flag = 'X'.
    MOVE-CORRESPONDING <vim_extract_struc> TO <table1>.
    PERFORM complete_exprofields.
    PERFORM update_tab.
  ENDLOOP.
ENDFORM.                    "batch_change

*&---------------------------------------------------------------------*
*& 单列粘贴
*& 把光标放入需要批量粘贴的开始单元格，点击粘贴后就会把剪贴板数据一次粘贴到本列
*&---------------------------------------------------------------------*
FORM paste.
  DATA: lv_fldnam TYPE char30.
  DATA: ls_column TYPE cxtab_column.
  DATA: lv_froml  TYPE sy-tabix.
  DATA: lv_clip   TYPE char255.
  DATA: lt_clip   TYPE TABLE OF char255.
  FIELD-SYMBOLS <lv_fldval> TYPE any.

  SPLIT f AT '-' INTO lv_fldnam lv_fldnam.
  READ TABLE <vim_tctrl>-cols INTO ls_column
                              WITH KEY screen-name = f.
  IF sy-subrc NE 0 OR ls_column-screen-input = 0.
    RETURN.
  ENDIF.

  cl_gui_frontend_services=>clipboard_import(
    IMPORTING
      data                 = lt_clip
    EXCEPTIONS
      OTHERS               = 4 ).
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cl_gui_cfw=>flush( ).
  IF lt_clip IS INITIAL.
    MESSAGE s000(oo) WITH '剪贴板为空'.
    RETURN.
  ENDIF.

  lv_froml = aktuell + l - 1.
  LOOP AT extract FROM lv_froml.
    READ TABLE lt_clip INTO lv_clip INDEX 1.
    CHECK sy-subrc = 0.
    DELETE lt_clip INDEX 1.

    ASSIGN COMPONENT lv_fldnam OF STRUCTURE extract TO <lv_fldval>.
    READ TABLE x_namtab INTO x_namtab WITH KEY viewfield = lv_fldnam.
    PERFORM convert_value USING lv_clip x_namtab-convexit x_namtab-lowercase
                          CHANGING <lv_fldval>.

    IF <xact> NE 'N'.
      <xact> = 'U'.
    ENDIF.
    <status>-upd_flag = 'X'.
    MOVE-CORRESPONDING <vim_extract_struc> TO <table1>.
    PERFORM complete_exprofields.
    PERFORM update_tab.
  ENDLOOP.
  IF lt_clip IS NOT INITIAL.
    MESSAGE s000(oo) WITH '剪贴板数据未粘贴完'.
  ENDIF.
ENDFORM.                    "paste

*&---------------------------------------------------------------------*
*& 导出上载数据的模板
*&---------------------------------------------------------------------*
FORM export_tmpl.
  DATA: lt_clip  TYPE TABLE OF char2048.
  DATA: ls_clip  TYPE char2048.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: ls_exfld TYPE vimexclfld.

  LOOP AT x_namtab INTO x_namtab WHERE datatype NE 'CLNT'.
    READ TABLE excl_rpl_tab INTO ls_exfld WITH KEY fieldname = x_namtab-viewfield.
    CHECK sy-subrc NE 0.

    CONCATENATE ls_clip cl_abap_char_utilities=>horizontal_tab
                x_namtab-viewfield INTO ls_clip.
  ENDLOOP.
  SHIFT ls_clip.
  APPEND ls_clip TO lt_clip.

  cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data   = lt_clip
      CHANGING
        rc     = lv_subrc
      EXCEPTIONS
        OTHERS = 4 ).
  IF sy-subrc = 0.
    MESSAGE s000(oo) WITH '已复制表头,请打开Excel粘贴'.
  ELSE.
    MESSAGE w000(oo) WITH '复制到剪贴板失败'.
  ENDIF.
ENDFORM.                    "export_tmpl

*&---------------------------------------------------------------------*
*& 批量上载新建数据
*& 按照导出模板在Excel整理数据，整理完复制数据后,按粘贴按钮即可把数据批量导入
*& 如果是C类型数据,且没有指定区分大小写,则转为大写
*& 如果已经存在相同主键数据,则本条数据不导入,并有提示
*& 如果存在转换例程,会转为内部格式
*&---------------------------------------------------------------------*
FORM upload.
  DATA: lt_clip TYPE TABLE OF char2048,
        lv_clip TYPE char2048,
        lt_fval TYPE TABLE OF char255,
        lv_fval TYPE char255.
  DATA: lv_insrow TYPE i,
        lv_delrow TYPE i.
  DATA: ls_exfld TYPE vimexclfld.
  FIELD-SYMBOLS: <lv_field> TYPE any.

  nextline = 1.
  LOOP AT extract INTO extract.
    IF <xact> = 'N'.
      nextline = nextline + 1.
    ENDIF.
  ENDLOOP.

  cl_gui_frontend_services=>clipboard_import(
    IMPORTING
      data                 = lt_clip
    EXCEPTIONS
      OTHERS               = 4 ).
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  cl_gui_cfw=>flush( ).
  IF lt_clip IS INITIAL.
    MESSAGE s000(oo) WITH '剪贴板为空'.
    RETURN.
  ENDIF.

  LOOP AT lt_clip INTO lv_clip.
    SPLIT lv_clip AT cl_abap_char_utilities=>horizontal_tab INTO TABLE lt_fval.
    LOOP AT x_namtab INTO x_namtab WHERE datatype NE 'CLNT'.
      READ TABLE excl_rpl_tab INTO ls_exfld WITH KEY fieldname = x_namtab-viewfield.
      CHECK sy-subrc NE 0.
      READ TABLE lt_fval INTO lv_fval INDEX 1.
      CHECK sy-subrc = 0.
      DELETE lt_fval INDEX 1.
      ASSIGN COMPONENT x_namtab-viewfield OF STRUCTURE <table1> TO <lv_field>.
      PERFORM convert_value USING lv_fval x_namtab-convexit x_namtab-lowercase
                            CHANGING <lv_field>.
    ENDLOOP.

    READ TABLE total INTO total WITH KEY <f1_x> BINARY SEARCH.
    IF sy-subrc = 0.
      lv_delrow = lv_delrow + 1.
    ELSE.
      <xact> = 'L'.
      <status>-upd_flag = 'X'.
      <table2_x> = <initial_x>.
      PERFORM complete_exprofields.
      PERFORM update_tab.
      nextline  = nextline + 1.
      lv_insrow = lv_insrow + 1.
    ENDIF.
  ENDLOOP.
  MESSAGE s000(oo) WITH '删除主键重复的'(m02) lv_delrow
                        '条，实际插入条数'(m03) lv_insrow.

  IF nextline <= looplines.
    nextline = 1.
  ELSE.
    nextline = nextline - ceil( looplines / 2 ).
  ENDIF.
ENDFORM.                    "upload

*&---------------------------------------------------------------------*
*& 排序功能
*& 把光标放到要排序的列内,然后点击排序按钮
*&---------------------------------------------------------------------*
FORM sort_table.
  DATA: lv_field TYPE name_feld.

  SPLIT f AT '-' INTO lv_field lv_field.
  IF function = 'SORTA'.
    SORT extract STABLE BY (lv_field).
  ELSE.
    SORT extract STABLE BY (lv_field) DESCENDING.
  ENDIF.
ENDFORM.                    "sort_table

*&---------------------------------------------------------------------*
*& 搜索功能
*& 把光标放到要搜索的列内,然后按Ctrl+F,或者点击搜索按钮即可弹出搜索框
*& 如果是字符型数据,会自动变为模糊搜索,如果是其他类型数据,则为精确搜索
*& 按Ctrl+G,搜索下一个
*&---------------------------------------------------------------------*
FORM tc_search.
  DATA: lv_subrc  TYPE sy-subrc.
  DATA: lv_fldnam TYPE char30.
  STATICS: lv_finds TYPE string,
           lv_colnm TYPE string,
           lv_whstr TYPE string,
           lv_itype TYPE char1,
           lv_index TYPE i.
  FIELD-SYMBOLS: <lv_fldval> TYPE any.

  lv_subrc = 1.
  IF function = 'SEARF'.
    SPLIT f AT '-' INTO lv_fldnam lv_fldnam.
    CONCATENATE 'EXTRACT-' lv_fldnam INTO lv_whstr.
    ASSIGN (lv_whstr) TO <lv_fldval>.

    PERFORM popup_getvalue USING vim_view_name lv_fldnam 'X' 'X'
                           CHANGING lv_finds sy-subrc.
    CHECK sy-subrc = 0.

    lv_itype = x_namtab-inttype.
    LOOP AT extract INTO extract.
      IF x_namtab-inttype = 'C'.
        CHECK <lv_fldval> CS lv_finds. "C类型的数据为模糊搜索
      ELSE.
        CHECK <lv_fldval> = lv_finds.
      ENDIF.

      nextline = sy-tabix.
      l = 1.
      lv_index = sy-tabix + 1.
      lv_colnm = lv_fldnam.
      lv_subrc = 0.
      EXIT.
    ENDLOOP.
  ELSE.
    CHECK lv_colnm IS NOT INITIAL AND lv_finds IS NOT INITIAL.

    CONCATENATE 'EXTRACT-' lv_colnm INTO lv_whstr.
    ASSIGN (lv_whstr) TO <lv_fldval>.

    LOOP AT extract INTO extract FROM lv_index.
      IF lv_itype = 'C'.
        CHECK <lv_fldval> CS lv_finds.
      ELSE.
        CHECK <lv_fldval> = lv_finds.
      ENDIF.

      l = 1.
      nextline = sy-tabix.
      lv_index = sy-tabix + 1.
      lv_subrc = 0.
      EXIT.
    ENDLOOP.
  ENDIF.
  IF lv_subrc = 1.
    MESSAGE s000(oo) WITH '没有符合条件的记录'(m04).
  ENDIF.
ENDFORM.                    "tc_search

*&---------------------------------------------------------------------*
*& 导出数据,
*& 可以把数据导出到本地UTF-8编码的文本文件,也可以使用ALV显示数据
*& 如果选定的了行,只导出选择的行,没有没有选择任何行,则导出所有行
*&---------------------------------------------------------------------*
FORM export.
  DATA: lt_clip TYPE TABLE OF char2048.
  DATA: lv_clip TYPE char2048.
  DATA: lv_char TYPE char255.
  DATA: lv_file TYPE string,
        lv_path TYPE string.
  DATA: lt_fldc TYPE lvc_t_fcat,
        ls_fldc TYPE lvc_s_fcat.
  FIELD-SYMBOLS: <lv_field> TYPE any.

  LOOP AT x_namtab INTO x_namtab.
    CONCATENATE lv_clip cl_abap_char_utilities=>horizontal_tab
                x_namtab-viewfield INTO lv_clip.
    ls_fldc-fieldname = x_namtab-viewfield.
    ls_fldc-ref_table = x_namtab-bastabname.
    ls_fldc-ref_field = x_namtab-bastabfld.
    ls_fldc-reptext   = x_namtab-viewfield.
    ls_fldc-coltext   = x_namtab-viewfield.
    ls_fldc-col_opt   = 'A'.
    APPEND ls_fldc TO lt_fldc.
    CLEAR ls_fldc.
  ENDLOOP.
  SHIFT lv_clip.
  APPEND lv_clip TO lt_clip.

  LOOP AT extract INTO extract.
    IF mark_extract > 0.
      CHECK <xmark> = 'M'.
    ENDIF.

    CLEAR lv_clip.
    LOOP AT x_namtab INTO x_namtab.
      ASSIGN COMPONENT x_namtab-viewfield OF STRUCTURE extract TO <lv_field>.
      WRITE <lv_field> TO lv_char LEFT-JUSTIFIED.

      CONCATENATE lv_clip cl_abap_char_utilities=>horizontal_tab
                  lv_char INTO lv_clip.
    ENDLOOP.
    SHIFT lv_clip.
    APPEND lv_clip TO lt_clip.
  ENDLOOP.

  IF lt_clip IS INITIAL.
    MESSAGE s000(oo) WITH '无内容'(m05).
  ELSE.
    IF function = 'EXPTTAB'.
      CONCATENATE vim_view_name '_' sy-datum '_' sy-uzeit INTO lv_file.
      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          default_extension    = 'txt'
          default_file_name    = lv_file
        CHANGING
          filename             = lv_path
          path                 = lv_path
          fullpath             = lv_file
        EXCEPTIONS
          OTHERS               = 4 ).
      IF lv_file IS NOT INITIAL AND sy-subrc = 0.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                 = lv_file
            filetype                 = 'DAT'
            codepage                 = '4110'
            write_lf_after_last_line = ''
          TABLES
            data_tab                 = lt_clip
          EXCEPTIONS
            OTHERS                   = 22.
        IF sy-subrc NE 0.
          CLEAR function.
          MESSAGE e000(oo) WITH 'DOWNLOAD Error'.
        ENDIF.
      ENDIF.
    ELSEIF function = 'EXPTALV'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
        EXPORTING
          it_fieldcat_lvc = lt_fldc
        TABLES
          t_outtab        = extract
        EXCEPTIONS
          program_error   = 1.
    ENDIF.
  ENDIF.
ENDFORM.                    "Export

*&---------------------------------------------------------------------*
*& 在需要输入值的时候弹出窗口供用户输入
*&---------------------------------------------------------------------*
FORM popup_getvalue USING pv_tabnam pv_fldnam pv_obl pv_cons
                    CHANGING cv_ostr cv_subrc.
  DATA: lt_sval TYPE TABLE OF sval,
        ls_sval TYPE sval,
        lv_code TYPE char1.

  READ TABLE x_namtab INTO x_namtab WITH KEY viewfield = pv_fldnam.
  IF x_namtab-inttype = 'C' AND pv_cons = 'X' AND
     'MATN1 ALPHA' CS x_namtab-convexit.
    ls_sval-tabname   = 'RSDXX'.
    ls_sval-fieldname = 'FINDSTR'.
  ELSE.
    ls_sval-tabname   = pv_tabnam.
    ls_sval-fieldname = pv_fldnam.
  ENDIF.
  ls_sval-fieldtext = '输入'.
  ls_sval-field_obl = pv_obl.  "是否必输
  APPEND ls_sval TO lt_sval.

  "如果参照表/字段不为空,则特殊处理,"为金额/数量等字段使用
  IF x_namtab-reftable IS NOT INITIAL.
    ls_sval-tabname    = x_namtab-reftable.
    ls_sval-fieldname  = x_namtab-reffield.
    ls_sval-field_attr = '04'.
    APPEND ls_sval TO lt_sval.
  ENDIF.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = '输入'
    IMPORTING
      returncode  = lv_code
    TABLES
      fields      = lt_sval.
  IF lv_code = 'A'.
    cv_subrc = 1.
  ELSE.
    READ TABLE lt_sval INTO ls_sval INDEX 1.
    cv_ostr = ls_sval-value.
    cv_subrc = 0.
  ENDIF.
ENDFORM.                    "popup_getvalue

*&---------------------------------------------------------------------*
*& 修改日志
*&---------------------------------------------------------------------*
FORM savesm30log.
  DATA: BEGIN OF ls_keys,
          vname TYPE zsm30log-vname,
          erdat TYPE zsm30log-erdat,
          stamp TYPE zsm30log-stamp,
          dtype TYPE zsm30log-dtype,
        END OF ls_keys.
  DATA: ls_save TYPE zsm30log.
  DATA: lv_stam TYPE timestampl,
        lv_stas TYPE char30.
  DATA: lr_bakt TYPE REF TO data,
        lr_baks TYPE REF TO data,
        lr_oldt TYPE REF TO data,
        lr_olds TYPE REF TO data,
        lr_newt TYPE REF TO data,
        lr_news TYPE REF TO data.
  FIELD-SYMBOLS: <lt_bak> TYPE STANDARD TABLE,
                 <lt_old> TYPE STANDARD TABLE,
                 <ls_old> TYPE any,
                 <lt_new> TYPE STANDARD TABLE,
                 <ls_new> TYPE any.

  CREATE DATA lr_bakt LIKE TABLE OF total.
  ASSIGN lr_bakt->* TO <lt_bak>.

  CREATE DATA lr_oldt TYPE TABLE OF (x_header-maintview).
  ASSIGN lr_oldt->* TO <lt_old>.
  CREATE DATA lr_olds LIKE LINE OF <lt_old>.
  ASSIGN lr_olds->* TO <ls_old>.

  CREATE DATA lr_newt TYPE TABLE OF (x_header-maintview).
  ASSIGN lr_newt->* TO <lt_new>.
  CREATE DATA lr_news LIKE LINE OF <lt_new>.
  ASSIGN lr_news->* TO <ls_new>.

  GET TIME STAMP FIELD lv_stam.
  WRITE lv_stam TIME ZONE sy-zonlo TO lv_stas.
  ls_keys-vname = x_header-maintview.
  ls_keys-erdat = sy-datlo.
  ls_keys-stamp = lv_stas+11(15).
  ls_save-ernam = sy-uname.
  ls_save-tname = x_header-roottab.

  "在程序 LZXXXF00的FORM get_data_zxxx.添加：
  "  EXPORT total TO MEMORY ID 'TOTAL_BAK'.
  IMPORT total = <lt_bak> FROM MEMORY ID 'TOTAL_BAK'.

***新加的数据
  LOOP AT total.
    CHECK <action> = 'N'.
    MOVE-CORRESPONDING <vim_total_struc> TO <ls_new>.
    APPEND <ls_new> TO <lt_new>.
  ENDLOOP.
  IF <lt_new> IS NOT INITIAL.
    ls_keys-dtype = 'N'.
    ls_save-lines  = lines( <lt_new> ).
    EXPORT tab = <lt_new> TO DATABASE zsm30log(z1) ID ls_keys FROM ls_save.
  ENDIF.

***删除的数据
  CLEAR <lt_new>.
  LOOP AT total.
    CHECK <action> = 'D'.
    MOVE-CORRESPONDING <vim_total_struc> TO <ls_new>.
    APPEND <ls_new> TO <lt_new>.
  ENDLOOP.
  IF <lt_new> IS NOT INITIAL.
    ls_keys-dtype = 'D'.
    ls_save-lines  = lines( <lt_new> ).
    EXPORT tab = <lt_new> TO DATABASE zsm30log(z1) ID ls_keys FROM ls_save.
  ENDIF.

***修改的数据
  CLEAR <lt_new>.
  LOOP AT total INTO total.
    CHECK <action> = 'U'.
    MOVE-CORRESPONDING total TO <ls_new>.
    APPEND <ls_new> TO <lt_new>.

    READ TABLE <lt_bak> INTO total WITH KEY <vim_xtotal_key> BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING total TO <ls_old>.
      APPEND <ls_old> TO <lt_old>.
    ENDIF.
  ENDLOOP.
  IF <lt_new> IS NOT INITIAL.
    ls_keys-dtype = 'A'. "After
    ls_save-lines  = lines( <lt_new> ).
    EXPORT tab = <lt_new> TO DATABASE zsm30log(z1) ID ls_keys FROM ls_save.
  ENDIF.
  IF <lt_old> IS NOT INITIAL.
    ls_keys-dtype = 'B'. "Before
    ls_save-lines  = lines( <lt_old> ).
    EXPORT tab = <lt_old> TO DATABASE zsm30log(z1) ID ls_keys FROM ls_save.
  ENDIF.

  EXPORT total TO MEMORY ID 'TOTAL_BAK'.
ENDFORM.                    "savesm30log

*&---------------------------------------------------------------------*
*& 数据由CHAR类型的外部格式转为目标数据类型、内部格式
*&---------------------------------------------------------------------*
FORM convert_value USING value(pv_input) pv_convexit pv_lowercase
                   CHANGING cv_output.
  STATICS ls_usr01 TYPE usr01.
  DATA lv_fmnam TYPE rs38l_fnam.
  DATA lv_ftype TYPE c.

  IF ls_usr01 IS INITIAL.
    SELECT SINGLE * FROM usr01 INTO ls_usr01 WHERE bname = sy-uname.
  ENDIF.

  DESCRIBE FIELD cv_output TYPE lv_ftype.
  IF pv_lowercase = '' AND lv_ftype = 'C'.
    TRANSLATE pv_input TO UPPER CASE.
  ENDIF.

  IF pv_convexit NE ''.
    CONCATENATE 'CONVERSION_EXIT_' pv_convexit '_INPUT' INTO lv_fmnam.
    CALL FUNCTION lv_fmnam
      EXPORTING
        input  = pv_input
      IMPORTING
        output = cv_output
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
      CLEAR function.
      MESSAGE e000(oo) WITH 'CONVERSION ERROR' pv_input.
    ENDIF.
  ELSE.
    TRY.
        CASE lv_ftype.
          WHEN 'I' OR 'P' OR 'F' OR 'a' OR 'e' OR 'b' OR 's'.
            IF ls_usr01-dcpfm = 'X'.
              TRANSLATE pv_input USING ', '.
            ELSE.
              TRANSLATE pv_input USING '. '.
              TRANSLATE pv_input USING ',.'.
            ENDIF.
            CONDENSE pv_input NO-GAPS.
            cv_output = pv_input.
          WHEN 'D'.
            IF pv_input CN ' 0123456789'.
              CALL FUNCTION 'DATE_STRING_CONVERT'
                EXPORTING
                  date_format = ls_usr01-datfm
                  date_string = pv_input
                IMPORTING
                  result_date = cv_output.
            ELSEIF pv_input = ''.
              CLEAR cv_output.
            ELSE.
              cv_output = pv_input.
            ENDIF.
            IF cv_output IS NOT INITIAL.
              CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                EXPORTING
                  date   = cv_output
                EXCEPTIONS
                  OTHERS = 1.
              IF sy-subrc NE 0.
                CLEAR function.
                MESSAGE e000(oo) WITH '日期格式非法：' cv_output.
              ENDIF.
            ENDIF.
          WHEN 'T'.
            IF pv_input CN ' 0123456789'.
              CONCATENATE pv_input(2) pv_input+3(2) pv_input+6(2) INTO cv_output.
            ELSEIF pv_input = ''.
              CLEAR cv_output.
            ELSE.
              cv_output = pv_input.
            ENDIF.
            CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
              EXPORTING
                time   = cv_output
              EXCEPTIONS
                OTHERS = 1.
            IF sy-subrc NE 0.
              CLEAR function.
              MESSAGE e000(oo) WITH '时间格式非法：' cv_output.
            ENDIF.
          WHEN 'N'.
            CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
              EXPORTING
                input  = pv_input
              IMPORTING
                output = cv_output.
          WHEN OTHERS.
            cv_output = pv_input.
        ENDCASE.
      CATCH cx_root.
        MESSAGE w000(oo) WITH '数据转换失败：' pv_input.
        RETURN.
    ENDTRY.
  ENDIF.
ENDFORM.                    "convert_value
