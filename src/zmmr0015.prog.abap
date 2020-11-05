*&---------------------------------------------------------------------*
*& Report ZMMR0015
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr0015.

*--------------------------------------------------------------------*
* Main Class
*--------------------------------------------------------------------*
CLASS lcl_zmmr0015 DEFINITION FINAL.

  PUBLIC SECTION.
    TYPE-POOLS: icon.
    METHODS: constructor IMPORTING i_banfn TYPE ANY TABLE
                                   i_bnfpo TYPE ANY TABLE
                                   i_matnr TYPE ANY TABLE
                                   i_werks TYPE ANY TABLE,

      get_pr_data RETURNING VALUE(e_table_data) TYPE zmmc0012,
      get_pr_log  RETURNING VALUE(e_table_data) TYPE zmmc0013,

      set_pr_conclusion CHANGING ch_pr_item TYPE zmmc0012,
      set_pr_exclusion CHANGING ch_pr_item TYPE zmmc0012,
      set_pr_log IMPORTING i_pr_item        TYPE zmmc0012
                 RETURNING VALUE(not_found) TYPE flag.


  PRIVATE SECTION.

    CONSTANTS: BEGIN OF gc_action,
                 conclusion TYPE zmmt010-action VALUE 'CONCLUIR',
                 exclusion  TYPE zmmt010-action VALUE 'EXCLUIR',
               END OF gc_action.

    DATA: gr_banfn TYPE RANGE OF eban-banfn,
          gr_bnfpo TYPE RANGE OF eban-bnfpo,
          gr_matnr TYPE RANGE OF eban-matnr,
          gr_werks TYPE RANGE OF eban-werks.

    DATA: gt_generated_log TYPE TABLE OF zmmt010.

ENDCLASS.

CLASS lcl_zmmr0015 IMPLEMENTATION.

  METHOD constructor.

    gr_banfn[] = i_banfn[].
    gr_bnfpo[] = i_bnfpo[].
    gr_matnr[] = i_matnr[].
    gr_werks[] = i_werks[].

  ENDMETHOD.

  METHOD get_pr_data.

    SELECT a~banfn a~bnfpo a~matnr b~maktx a~werks a~loekz a~ebakz
      FROM eban AS a
      INNER JOIN makt AS b
      ON a~matnr = b~matnr
      INTO TABLE e_table_data
      WHERE a~banfn IN gr_banfn
        AND a~bnfpo IN gr_bnfpo
        AND a~matnr IN gr_matnr
        AND a~werks IN gr_werks.

  ENDMETHOD.

  METHOD get_pr_log.

* Adjust icon
    LOOP AT gt_generated_log ASSIGNING FIELD-SYMBOL(<log>).

      APPEND INITIAL LINE TO e_table_data ASSIGNING FIELD-SYMBOL(<table_data>).
      MOVE-CORRESPONDING <log> TO <table_data>.

      IF <log>-type = 'S'.
        <table_data>-icon = icon_led_green.
      ELSE.
        <table_data>-icon = icon_led_red.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_pr_conclusion.

    DATA: lt_pr_header TYPE zmmc0012,
          lt_pritem    TYPE TABLE OF bapimereqitemimp,
          lt_pritemx   TYPE TABLE OF bapimereqitemx,
          lt_return    TYPE TABLE OF bapiret2.

    CLEAR gt_generated_log.

    SORT ch_pr_item BY banfn.

* Get purchase requisition header
    lt_pr_header[] = ch_pr_item[].
    DELETE ADJACENT DUPLICATES FROM lt_pr_header COMPARING banfn.

* Call funtion for each purchase requisition
    LOOP AT lt_pr_header ASSIGNING FIELD-SYMBOL(<pr_header>).

      CLEAR: lt_pritem, lt_return, lt_pritemx.

* Groups items by purchase requisition
      LOOP AT ch_pr_item ASSIGNING FIELD-SYMBOL(<pr_item>) WHERE banfn = <pr_header>-banfn.

        APPEND INITIAL LINE TO lt_pritem ASSIGNING FIELD-SYMBOL(<pritem>).
        <pritem>-preq_item = <pr_item>-bnfpo.
        <pritem>-closed    = abap_true.

        APPEND INITIAL LINE TO lt_pritemx ASSIGNING FIELD-SYMBOL(<pritemx>).
        <pritemx>-preq_item  = <pr_item>-bnfpo.
        <pritemx>-preq_itemx = abap_true.
        <pritemx>-closed     = abap_true.

        APPEND INITIAL LINE TO gt_generated_log ASSIGNING FIELD-SYMBOL(<log>).
        <log>-banfn = <pr_header>-banfn.
        <log>-bnfpo = <pr_item>-bnfpo.
        <log>-matnr = <pr_item>-matnr.
        <log>-action = gc_action-conclusion.

      ENDLOOP.

* Set purchase requisition as completed
      CALL FUNCTION 'BAPI_PR_CHANGE'
        EXPORTING
          number  = <pr_header>-banfn
        TABLES
          return  = lt_return
          pritem  = lt_pritem
          pritemx = lt_pritemx.

* Save changes log
      IF line_exists( lt_return[ type = 'E' ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT gt_generated_log ASSIGNING <log> WHERE banfn = <pr_header>-banfn.

          LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<return>) WHERE type = 'E'.
            MOVE-CORRESPONDING <return> TO <log>.
            <log>-msgno = <return>-number.
          ENDLOOP.

        ENDLOOP.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        LOOP AT gt_generated_log ASSIGNING <log> WHERE banfn = <pr_header>-banfn.

          READ TABLE lt_return ASSIGNING <return> WITH KEY type = 'S'
                                                           id   = '06'
                                                           number = '403'.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <return> TO <log>.
            <log>-msgno = <return>-number.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

    IF lines( gt_generated_log ) > 0.
      MODIFY zmmt010 FROM TABLE gt_generated_log.
    ENDIF.

  ENDMETHOD.

  METHOD set_pr_exclusion.

    DATA: lt_pr_header TYPE zmmc0012,
          lt_pritem    TYPE TABLE OF bapimereqitemimp,
          lt_pritemx   TYPE TABLE OF bapimereqitemx,
          lt_return    TYPE TABLE OF bapiret2.

    CLEAR gt_generated_log.

    SORT ch_pr_item BY banfn.

* Get purchase requisition header
    lt_pr_header[] = ch_pr_item[].
    DELETE ADJACENT DUPLICATES FROM lt_pr_header COMPARING banfn.

* Call funtion for each purchase requisition
    LOOP AT lt_pr_header ASSIGNING FIELD-SYMBOL(<pr_header>).

      CLEAR: lt_pritem, lt_return, lt_pritemx.

* Groups items by purchase requisition
      LOOP AT ch_pr_item ASSIGNING FIELD-SYMBOL(<pr_item>) WHERE banfn = <pr_header>-banfn.

        APPEND INITIAL LINE TO lt_pritem ASSIGNING FIELD-SYMBOL(<pritem>).
        <pritem>-preq_item  = <pr_item>-bnfpo.
        <pritem>-delete_ind = abap_true.

        APPEND INITIAL LINE TO lt_pritemx ASSIGNING FIELD-SYMBOL(<pritemx>).
        <pritemx>-preq_item  = <pr_item>-bnfpo.
        <pritemx>-preq_itemx = abap_true.
        <pritemx>-delete_ind = abap_true.

        APPEND INITIAL LINE TO gt_generated_log ASSIGNING FIELD-SYMBOL(<log>).
        <log>-banfn = <pr_header>-banfn.
        <log>-bnfpo = <pr_item>-bnfpo.
        <log>-matnr = <pr_item>-matnr.
        <log>-action = gc_action-exclusion.

      ENDLOOP.

* Set purchase requisition as deleted
      CALL FUNCTION 'BAPI_PR_CHANGE'
        EXPORTING
          number  = <pr_header>-banfn
        TABLES
          return  = lt_return
          pritem  = lt_pritem
          pritemx = lt_pritemx.

* Save changes log
      IF line_exists( lt_return[ type = 'E' ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT gt_generated_log ASSIGNING <log> WHERE banfn = <pr_header>-banfn.

          LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<return>) WHERE type = 'E'.
            MOVE-CORRESPONDING <return> TO <log>.
            <log>-msgno = <return>-number.
          ENDLOOP.

        ENDLOOP.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        LOOP AT gt_generated_log ASSIGNING <log> WHERE banfn = <pr_header>-banfn.

          READ TABLE lt_return ASSIGNING <return> WITH KEY type = 'S'
                                                           id   = '06'
                                                           number = '403'.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <return> TO <log>.
            <log>-msgno = <return>-number.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

    IF lines( gt_generated_log ) > 0.
      MODIFY zmmt010 FROM TABLE gt_generated_log.
    ENDIF.

  ENDMETHOD.

  METHOD set_pr_log.

    SELECT *
      FROM zmmt010
      INTO TABLE gt_generated_log
      FOR ALL ENTRIES IN i_pr_item
      WHERE banfn = i_pr_item-banfn
        AND bnfpo = i_pr_item-bnfpo.
    IF sy-subrc <> 0.
      not_found = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*--------------------------------------------------------------------*
* Global Objects
*--------------------------------------------------------------------*
TABLES: eban.

DATA: go_report      TYPE REF TO lcl_zmmr0015,
      gt_alv         TYPE zmmc0012,
      gt_log         TYPE zmmc0013,
      go_alv_9000    TYPE REF TO cl_gui_alv_grid,
      go_alv_9001    TYPE REF TO cl_gui_alv_grid,
      go_cont_9000   TYPE REF TO cl_gui_docking_container,
      go_cont_9001   TYPE REF TO cl_gui_docking_container,
      go_parent_9000 TYPE REF TO cl_gui_container,
      go_parent_9001 TYPE REF TO cl_gui_container.

*--------------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_banfn FOR eban-banfn OBLIGATORY,
                  s_bnfpo FOR eban-bnfpo," OBLIGATORY,
                  s_matnr FOR eban-matnr,
                  s_werks FOR eban-werks.

SELECTION-SCREEN: END OF BLOCK b1.

*--------------------------------------------------------------------*
* Main event
*--------------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT go_report
    EXPORTING
      i_banfn = s_banfn[]
      i_bnfpo = s_bnfpo[]
      i_matnr = s_matnr[]
      i_werks = s_werks[].

  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATUS_9000'.
  SET TITLEBAR 'TITLE_9000'.

  DATA: ls_layout  TYPE lvc_s_layo,
        ls_variant TYPE disvariant,
        lt_sort    TYPE lvc_t_sort.

* Edit layout
  CLEAR ls_layout.
  ls_layout-sel_mode   = 'A'.

  gt_alv = go_report->get_pr_data( ).

  DATA: lt_fcat TYPE lvc_t_fcat.
  CLEAR lt_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMS0009'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

    IF <fcat>-fieldname = 'LOEKZ' OR <fcat>-fieldname = 'EBAKZ'.
      <fcat>-outputlen = 8.
      <fcat>-just = 'C'.
    ELSE.
      <fcat>-col_opt = abap_true.
    ENDIF.

    IF <fcat>-fieldname = 'BANFN' OR <fcat>-fieldname = 'BNFPO'.
      <fcat>-key = abap_true.
    ENDIF.

  ENDLOOP.

  IF go_cont_9000 IS BOUND.

    go_alv_9000->set_frontend_fieldcatalog( it_fieldcatalog = lt_fcat ).

    DATA: ls_stable TYPE lvc_s_stbl.
    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    go_alv_9000->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable        " With Stable Rows/Columns
      EXCEPTIONS
        finished       = 1                " Display was Ended (by Export)
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.

* Cria o Container para a tela do ALV
    CREATE OBJECT go_cont_9000
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_top
        repid     = sy-repid
        dynnr     = '9000'
        extension = 1000
      EXCEPTIONS
        OTHERS    = 6.

* Casting para o tipo genério
    go_parent_9000 = go_cont_9000.

* Criia Instância do ALV, referenciando o Container gerado acima
    CREATE OBJECT go_alv_9000
      EXPORTING
        i_parent = go_parent_9000.

    ls_variant-report = sy-repid.
    ls_variant-username = sy-uname.

*APPEND INITIAL LINE TO lt_sort ASSIGNING FIELD-SYMBOL(<
    CLEAR lt_sort.
    APPEND VALUE #( fieldname = 'BANFN' up = 'X' ) TO lt_sort.
    APPEND VALUE #( fieldname = 'BNFPO' up = 'X' ) TO lt_sort.

* Exibe o ALV
    go_alv_9000->set_table_for_first_display(
     EXPORTING
       is_variant                    = ls_variant                " Layout
       i_save                        = 'A'                 " Save Layout
       is_layout                     = ls_layout                 " Layout
      CHANGING
       it_outtab                     = gt_alv                " Output Table
       it_fieldcatalog               = lt_fcat                 " Field Catalog
       it_sort                       = lt_sort                 " Sort Criteria
     EXCEPTIONS
       invalid_parameter_combination = 1                " Wrong Parameter
       program_error                 = 2                " Program Errors
       too_many_lines                = 3                " Too many Rows in Ready for Input Grid
       OTHERS                        = 4
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  DATA: lt_selected_data TYPE zmmc0012.

  CASE sy-ucomm.

    WHEN '&F03' OR '&F15' OR '&F12'.

      go_alv_9000->free( ).
      go_cont_9000->free( ).

      CALL METHOD cl_gui_cfw=>flush.

      CLEAR: go_alv_9000, go_cont_9000.

      LEAVE TO SCREEN 0.

    WHEN '&CONCL'.

* Get selected rows
      CLEAR lt_selected_data.
      go_alv_9000->check_changed_data( ).
      go_alv_9000->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).

      IF lines( lt_index_rows ) = 0.
        MESSAGE 'Favor selecionar uma linha para processamento'(002) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

* Process rows
      LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<index_row>).
        READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX <index_row>.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lt_selected_data ASSIGNING FIELD-SYMBOL(<select_data>).
          <select_data> = <alv>.
        ENDIF.
      ENDLOOP.

      go_report->set_pr_conclusion( CHANGING ch_pr_item = lt_selected_data ).

* Display log screen
      CALL SCREEN 9001.

    WHEN '&DELET'.

* Get selected rows
      CLEAR lt_selected_data.
      go_alv_9000->check_changed_data( ).
      go_alv_9000->get_selected_rows( IMPORTING et_index_rows = lt_index_rows ).

      IF lines( lt_index_rows ) = 0.
        MESSAGE 'Favor selecionar uma linha para processamento'(002) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

* Process rows
      LOOP AT lt_index_rows ASSIGNING <index_row>.
        READ TABLE gt_alv ASSIGNING <alv> INDEX <index_row>.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lt_selected_data ASSIGNING <select_data>.
          <select_data> = <alv>.
        ENDIF.
      ENDLOOP.

      go_report->set_pr_exclusion( CHANGING ch_pr_item = lt_selected_data ).

* Display log screen
      CALL SCREEN 9001.

    WHEN '&LOG'.

* Get selected rows
      CLEAR lt_selected_data.
      go_alv_9000->check_changed_data( ).
      go_alv_9000->get_selected_rows( IMPORTING et_index_rows = lt_index_rows ).

      IF lines( lt_index_rows ) = 0.
        MESSAGE 'Favor selecionar uma linha para processamento'(002) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

* Process rows
      LOOP AT lt_index_rows ASSIGNING <index_row>.
        READ TABLE gt_alv ASSIGNING <alv> INDEX <index_row>.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lt_selected_data ASSIGNING <select_data>.
          <select_data> = <alv>.
        ENDIF.
      ENDLOOP.

      IF go_report->set_pr_log( EXPORTING i_pr_item = lt_selected_data ) = abap_true.
        MESSAGE 'Não foram encontrados registros de log para a req.compra selecionada'(004) TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
* Display log screen
        CALL SCREEN 9001.
      ENDIF.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'STATUS_9001'.
  SET TITLEBAR 'TITLE_9001'.

  gt_log = go_report->get_pr_log( ).

* Edit layout
  CLEAR ls_layout.
  ls_layout-cwidth_opt = abap_true.

  IF go_cont_9001 IS BOUND.

    go_alv_9001->set_frontend_layout( is_layout = ls_layout ).

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    go_alv_9001->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable        " With Stable Rows/Columns
      EXCEPTIONS
        finished       = 1                " Display was Ended (by Export)
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.

* Cria o Container para a tela do ALV
    CREATE OBJECT go_cont_9001
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_top
        repid     = sy-repid
        dynnr     = '9001'
        extension = 1000
      EXCEPTIONS
        OTHERS    = 6.

* Casting para o tipo genério
    go_parent_9001 = go_cont_9001.

* Criia Instância do ALV, referenciando o Container gerado acima
    CREATE OBJECT go_alv_9001
      EXPORTING
        i_parent = go_parent_9001.

    CLEAR lt_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZMMS0010'
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT lt_fcat ASSIGNING <fcat>.

      IF <fcat>-fieldname CS 'MESSAGE_V'.
        <fcat>-no_out = abap_true.
      ENDIF.

      IF <fcat>-fieldname = 'ICON'.
        <fcat>-icon = abap_true.
        <fcat>-coltext = 'Status'(003).
      ENDIF.

    ENDLOOP.

* Exibe o ALV
    go_alv_9001->set_table_for_first_display(
     EXPORTING
*     i_buffer_active               =                  " Buffering Active
*     i_bypassing_buffer            =                  " Switch Off Buffer
*     i_consistency_check           =                  " Starting Consistency Check for Interface Error Recognition
        i_structure_name              =   'ZMMT010'               " Internal Output Table Structure Name
*     is_variant                    =                  " Layout
*     i_save                        =                  " Save Layout
*     i_default                     = 'X'              " Default Display Variant
       is_layout                     = ls_layout                 " Layout
*     is_print                      =                  " Print Control
*     it_special_groups             =                  " Field Groups
*     it_toolbar_excluding          =                  " Excluded Toolbar Standard Functions
*     it_hyperlink                  =                  " Hyperlinks
*     it_alv_graphics               =                  " Table of Structure DTC_S_TC
*     it_except_qinfo               =                  " Table for Exception Quickinfo
*     ir_salv_adapter               =                  " Interface ALV Adapter
      CHANGING
        it_outtab                     =  gt_log                " Output Table
       it_fieldcatalog               = lt_fcat                 " Field Catalog
*     it_sort                       =                  " Sort Criteria
*     it_filter                     =                  " Filter Criteria
     EXCEPTIONS
       invalid_parameter_combination = 1                " Wrong Parameter
       program_error                 = 2                " Program Errors
       too_many_lines                = 3                " Too many Rows in Ready for Input Grid
       OTHERS                        = 4
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN '&F03' OR '&F15' OR '&F12'.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
