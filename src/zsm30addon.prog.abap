*&---------------------------------------------------------------------*
*& SM30添加自定义按钮
*& Baitianzhen 2019-7-24
*&---------------------------------------------------------------------*
REPORT  zsm30status NO STANDARD PAGE HEADING .

TABLES: tvdir,tvimf,tadir.
DATA: h TYPE d020s.
DATA: f TYPE TABLE OF d021s WITH HEADER LINE.
DATA: e TYPE TABLE OF d022s WITH HEADER LINE.
DATA: m TYPE TABLE OF d023s WITH HEADER LINE.
DATA: dy_id  TYPE dynpro_id,
      dy_msg TYPE char256,
      dy_lnr TYPE sy-tabix,
      dy_wrd TYPE d021s-fnam.
DATA: funcpool TYPE trdir-name.
DATA: funcf00  TYPE trdir-name.
DATA: funcform TYPE trdir-name.
DATA: it_pcode TYPE STANDARD TABLE OF char256 WITH HEADER LINE.
DATA: subrc TYPE sy-subrc,
      lines TYPE i.
DATA: trkorr TYPE e070-trkorr.
DATA: tabix TYPE sy-tabix.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
PARAMETERS p_mview TYPE viewname16 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE btxt2.
PARAMETERS: p_copy AS CHECKBOX DEFAULT 'X',
            p_modu AS CHECKBOX DEFAULT 'X',
            p_wlog AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '维护视图'.
  btxt2 = '选项'.
  %_p_mview_%_app_%-text = '维护视图名称'.
  %_p_copy_%_app_%-text  = '复制工具栏'.
  %_p_modu_%_app_%-text  = '屏幕流添加MODULE'.
  %_p_wlog_%_app_%-text  = '记录SM30修改日志'.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  SELECT SINGLE * FROM tvdir WHERE tabname = p_mview.
  IF sy-subrc NE 0 OR tvdir-area = ''.
    MESSAGE s000(oo) WITH '还没有维护视图'.
  ELSEIF tvdir-bastab = 'X'.
    MESSAGE s000(oo) WITH '不是针对视图创建的Table maintenance'.
  ELSE.
    SELECT SINGLE * FROM tadir
      WHERE pgmid = 'R3TR' AND
            object = 'FUGR' AND
    obj_name = tvdir-area.
    CONCATENATE 'SAPL' tvdir-area INTO funcpool.
    IF p_copy = 'X'.
      SELECT SINGLE * FROM tvimf
        WHERE tabname  EQ p_mview AND
              event    EQ 'ST'.
      IF sy-subrc = 0.
        PERFORM confirm USING '警告' '已经有ST事件，是否覆盖?' '2'.
        CHECK sy-subrc = 0.
      ENDIF.

      CALL FUNCTION 'RS_CUA_COPY'
        EXPORTING
          cobjectname          = funcpool
          objectname           = 'ZSM30ADDON'
          suppress_checks      = 'X'
        EXCEPTIONS
          already_exists       = 01
          not_excecuted        = 02
          object_not_found     = 03
          object_not_specified = 04
          permission_failure   = 05.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING
          operation        = 'ACTIVATE'
          object_name      = funcpool
          enclosing_object = funcpool
          object_type      = 'CUAD'
        EXCEPTIONS
          OTHERS           = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      tvimf-tabname  = p_mview.
      tvimf-event    = 'ST'.
      tvimf-formname = funcpool.
      MODIFY tvimf FROM tvimf.

      UPDATE eudb SET langu = sy-langu
        WHERE relid = 'CU' AND
              name  = funcpool.
    ENDIF.

    IF tadir-devclass NE '$TMP'.
      PERFORM tvimf_request USING p_mview.
    ENDIF.

    IF p_wlog = 'X'.
      SELECT SINGLE * FROM tvimf
        WHERE tabname  EQ p_mview AND
              event    EQ '01' AND
              formname NE 'SAVESM30LOG'.
      IF sy-subrc = 0.
        MESSAGE s000(oo) WITH '已经有01事件的维护，请删除后重试'.
        RETURN.
      ENDIF.

      tvimf-tabname  = p_mview.
      tvimf-event    = '01'.
      tvimf-formname = 'SAVESM30LOG'.
      INSERT tvimf FROM tvimf.

      CONCATENATE 'L' tvdir-area 'F00' INTO funcf00.
      CONCATENATE 'FORM*GET_DATA_' p_mview '*' INTO funcform.
      READ REPORT funcf00 INTO it_pcode.
      LOOP AT it_pcode WHERE table_line CP funcform.
        tabix = sy-tabix.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        LOOP AT it_pcode FROM tabix.
          IF it_pcode CS 'ENDFORM'.
            tabix = sy-tabix.
            EXIT.
          ELSEIF it_pcode CS `EXPORT total TO MEMORY ID 'TOTAL_BAK'`.
            tabix = 0.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF tabix NE 0.
          INSERT `  EXPORT total TO MEMORY ID 'TOTAL_BAK'.` INTO it_pcode INDEX tabix.
          INSERT REPORT funcf00 FROM it_pcode.
        ENDIF.
      ENDIF.
    ENDIF.

***屏幕流添加MODULE
    IF p_modu = 'X'.
      dy_id-prog = funcpool.
      dy_id-dnum = tvdir-liste.
      IMPORT DYNPRO h f e m ID dy_id.

      LOOP AT e WHERE line CS 'HIDE_POSITION_BUTTON'.
      ENDLOOP.
      IF sy-subrc NE 0.
        LOOP AT e WHERE line CS 'LISTE_INITIALISIEREN'.
          lines = sy-tabix.
        ENDLOOP.
        IF sy-subrc = 0.
          lines = lines + 1.
          INSERT ' MODULE HIDE_POSITION_BUTTON.' INTO e INDEX lines.
        ENDIF.
      ENDIF.
      LOOP AT e WHERE line CS 'ZSM30ADDON_COMMAND'.
      ENDLOOP.
      IF sy-subrc NE 0.
        APPEND ' MODULE ZSM30ADDON_COMMAND.' TO e.
      ENDIF.

      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          global_lock     = ' '       " 'LIMU DYNP'
          object          = dy_id
          object_class    = 'SCRP'
          master_language = sy-langu
          korrnum         = trkorr
        IMPORTING
          korrnum         = trkorr
        EXCEPTIONS
          OTHERS          = 04.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      EXPORT DYNPRO h f e m ID dy_id.
      GENERATE DYNPRO h f e m ID dy_id MESSAGE dy_msg LINE dy_lnr WORD dy_wrd.

      CLEAR it_pcode[].
      READ REPORT funcpool INTO it_pcode.
      LOOP AT it_pcode WHERE table_line CS 'ZSM30ADDONI01'.
      ENDLOOP.
      IF sy-subrc NE 0.
        APPEND `  INCLUDE ZSM30ADDONI01.` TO it_pcode.
        INSERT REPORT funcpool FROM it_pcode.
      ENDIF.
      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          global_lock     = ' '
          object          = funcpool
          object_class    = 'ABAP'
          master_language = sy-langu
          korrnum         = trkorr
        IMPORTING
          korrnum         = trkorr
        EXCEPTIONS
          OTHERS          = 04.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  tvimf_request
*&---------------------------------------------------------------------*
FORM tvimf_request USING viewname.
  DATA: trkorr TYPE e070-trkorr,
        task   TYPE e070-trkorr,
        e071k  TYPE e071k OCCURS 0 WITH HEADER LINE,
        e071   TYPE e071 OCCURS 0 WITH HEADER LINE.
  DATA: vim_name TYPE vim_name.

  vim_name = viewname.

  e071-objfunc  = 'K'.
  e071-pgmid    = 'R3TR'.
  e071-object   = 'TABU'.
  e071-obj_name = 'TVIMF'.
  APPEND e071.

  e071k-pgmid      = 'R3TR'.
  e071k-mastertype = 'TABU'.
  e071k-object     = 'TABU'.
  e071k-mastername = 'TVIMF'.
  e071k-objname    = 'TVIMF'.
*  CONCATENATE vim_name 'ST' INTO e071k-tabkey RESPECTING BLANKS. "R3不支持
  CONCATENATE space 'ST' INTO e071k-tabkey SEPARATED BY vim_name.
  APPEND e071k.
*  CONCATENATE vim_name '01' INTO e071k-tabkey RESPECTING BLANKS.
  CONCATENATE space '01' INTO e071k-tabkey SEPARATED BY vim_name.
  APPEND e071k.

  CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
    EXPORTING
      iv_category = 'SYST'
    IMPORTING
      ev_order    = trkorr
      ev_task     = task
    EXCEPTIONS
      OTHERS      = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
    EXPORTING
      wi_simulation         = ' '
      wi_suppress_key_check = ' '
      wi_trkorr             = task
    TABLES
      wt_e071               = e071
      wt_e071k              = e071k
    EXCEPTIONS
      OTHERS                = 68.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. "tvimf_request

*&---------------------------------------------------------------------*
*& 返回 sy-subrc=0:是     4:否     8:取消
*& 缺省BUTTON '1' 左面按钮, '2' 第二个按钮 'A'
*&---------------------------------------------------------------------*
FORM confirm USING pv_title pv_question pv_button.
  DATA: lv_cflag  TYPE char1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = pv_title
      text_question  = pv_question
      default_button = pv_button
    IMPORTING
      answer         = lv_cflag.
  IF lv_cflag = '1'.
    sy-subrc = 0.
  ELSEIF lv_cflag = '2'.
    sy-subrc = 4.
  ELSE.
    sy-subrc = 8.
  ENDIF.
ENDFORM. "confirm
