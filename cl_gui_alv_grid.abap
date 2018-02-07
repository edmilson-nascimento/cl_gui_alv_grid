report z_cl_gui_alv_grid .

*--------------------------------------------------------------------*
* Tipos
*--------------------------------------------------------------------*
type-pools: abap, cntb.

*--------------------------------------------------------------------*
* Classes - Definição
*--------------------------------------------------------------------*
class gcl_report definition.

  public section.

    types:
      ty_so_screen type standard table of makt.

    class-data:
      so_screen    type line of ty_so_screen.

    methods search
      importing
        !matnr type ranges_matnr .

    methods generate_grid .


  protected section .


  private section.

    types:
    begin of ty_outtab,
      matnr type matnr,
      maktx type maktx,
      icon  type icon_int,
    end of ty_outtab.

    class-data:
      gt_outtab type standard table of ty_outtab,
      gs_outtab type                   ty_outtab,
      lo_grid   type ref to            cl_gui_alv_grid .

    methods fieldcat
      importing
        !ls_structure type any
      changing
        !gt_fieldcat  type lvc_t_fcat .

    methods call_screen_default .

    methods handler_toolbar
      for event toolbar of cl_gui_alv_grid
      importing
        e_object e_interactive .

    methods handler_user_command
      for event
        !user_command of cl_gui_alv_grid
      importing
        !e_ucomm .

    methods handler_hotspot_click
      for event
        !hotspot_click of cl_gui_alv_grid
      importing
        !e_row_id
        !e_column_id
        !es_row_no .

    methods executa_on_off .

    methods refresh .

endclass.                    "gcl_report DEFINITION


*----------------------------------------------------------------------*
* Declações                                                            *
*----------------------------------------------------------------------*
data:
  obj type ref to gcl_report .


*----------------------------------------------------------------------*
* Tela de seleção                                                      *
*----------------------------------------------------------------------*
selection-screen begin of block bloc1 with frame title text-001.
select-options s_matnr for gcl_report=>so_screen-matnr .
selection-screen: end of block bloc1.

*--------------------------------------------------------------------*
* Lógica princial
*--------------------------------------------------------------------*
start-of-selection.

  create object obj .

  obj->search(
    exporting
      matnr = s_matnr[]
  ).

  obj->generate_grid( ).

*--------------------------------------------------------------------*
* Classes - Implementação
*--------------------------------------------------------------------*
class gcl_report implementation.

  method search .

    if lines( matnr ) gt 0 .

      select matnr maktx
        into table gt_outtab
        from makt
       where matnr in matnr .

    endif.

  endmethod .                    "search

  method generate_grid .

    data:
      lt_fcat   type lvc_t_fcat,
      lt_sort   type lvc_t_sort,
      ls_sort   type lvc_s_sort,
      ls_vari   type disvariant,
      ls_layout type lvc_s_layo.

    field-symbols:
      <ls_fcat> type lvc_s_fcat.

    if lo_grid is not initial.
      lo_grid->free( ).
      clear lo_grid.
    endif.

    create object lo_grid
      exporting
*       i_shellstyle      = 0
*       i_lifetime        =
        i_parent          = cl_gui_container=>default_screen
*       i_appl_events     = space
*       i_parentdbg       =
*       i_applogparent    =
*       i_graphicsparent  =
*       i_name            =
*       i_fcat_complete   = space
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5 .

    if sy-subrc ne 0 .
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    me->fieldcat(
      exporting
        ls_structure = gs_outtab
      changing
        gt_fieldcat  = lt_fcat
    ) .

*   Layout de sáida do ALV
    ls_layout-grid_title = 'Material' .
    ls_layout-sel_mode   = 'A'.
    ls_layout-cwidth_opt = 'X'.   " optimize column width
    ls_layout-stylefname = 'STYLE'.
    ls_layout-zebra      = abap_true.
    ls_vari-report = sy-repid.

*   Ordenação de saída
    ls_sort-spos      = 1.
    ls_sort-fieldname = 'MAKTX'.
    ls_sort-up        = abap_true.
    ls_sort-subtot    = abap_false.
    append ls_sort to lt_sort.

    call method lo_grid->set_table_for_first_display
      exporting
        is_layout                     = ls_layout
        i_save                        = 'A'
        is_variant                    = ls_vari
      changing
        it_outtab                     = gt_outtab
        it_sort                       = lt_sort
        it_fieldcatalog               = lt_fcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

*   Eventos
    set handler me->handler_hotspot_click for lo_grid.
    set handler me->handler_user_command  for lo_grid.
    set handler me->handler_toolbar       for lo_grid.

    lo_grid->refresh_table_display( ).

*   Exibir relatório na tela default do report
    call_screen_default( ).

  endmethod.                    "generate_grid


  method fieldcat .

    data:
      ls_fieldcat type line of lvc_t_fcat,
      lobj_stdesc type ref to  cl_abap_structdescr,
      lt_fields   type         cl_abap_structdescr=>included_view,
      ls_fields   type line of cl_abap_structdescr=>included_view,
      ls_desc     type x030l .


    field-symbols:
      <fs_fieldcat> like line of gt_fieldcat .

    refresh:
      gt_fieldcat .

*   Determine structure descriptor
    try.
        lobj_stdesc ?= cl_abap_structdescr=>describe_by_data( ls_structure ).
      catch cx_root.
*       raise no_field_catalog.
    endtry.

*   if it is ddic structure, determine field catalog using alv fm
    if lobj_stdesc->is_ddic_type( ) is not initial.
    endif.

*   get structure fields
    lt_fields = lobj_stdesc->get_included_view( ).

*   build field catalog
    loop at lt_fields into ls_fields.

      ls_fieldcat-col_pos   = sy-tabix.
      ls_fieldcat-fieldname = ls_fields-name.
      if ls_fields-type->is_ddic_type( ) is not initial.
        ls_desc              = ls_fields-type->get_ddic_header( ).
        ls_fieldcat-rollname = ls_desc-tabname.
      else.
        ls_fieldcat-inttype  = ls_fields-type->type_kind.
        ls_fieldcat-intlen   = ls_fields-type->length.
*       ls_fieldcat-decimals = ls_fields-type->decimals.
      endif.

      if ls_fieldcat-fieldname eq 'MARK' .
        ls_fieldcat-mark = abap_on .
      endif .

      append ls_fieldcat to gt_fieldcat .
      clear: ls_fieldcat, ls_desc .

    endloop.

  endmethod .                    "fieldcat

  method call_screen_default.

    data:
      lt_excl type table of sy-ucomm .

*   Retira os botões exec e save do pf-status da
*   tela default 1000 apos chamada do relátorio
    append  'CRET' to lt_excl.
    append  'SPOS' to lt_excl.
    append  'GET'  to lt_excl.

    call function 'RS_SET_SELSCREEN_STATUS'
      exporting
        p_status  = sy-pfkey
        p_program = sy-repid
      tables
        p_exclude = lt_excl.

*   exibi relatório na tela default do report
    call selection-screen 1000 .

    free:
      lt_excl .

  endmethod.                    "call_screen_default

  method handler_hotspot_click.

    data: ls_outtab like line of gt_outtab .

    case e_column_id.
      when 'MATNR'.
        read table gt_outtab into ls_outtab
        index e_row_id-index.
        if sy-subrc eq 0 .
          set parameter id 'MAT'  field  ls_outtab-matnr .
          call transaction 'MM03' and skip first screen .
        endif.
    endcase.

  endmethod.                    "HANDLER_HOTSPOT_CLICK

  method: handler_user_command.

    case e_ucomm.

      when 'PROC'.
        me->executa_on_off( ) .

    endcase.

  endmethod.                    "handler_user_command

  method handler_toolbar.

    data:  ls_tool_stru  type stb_button,
          lt_tool_stru  type ttb_button.

    ls_tool_stru-butn_type  = cntb_btype_sep .
    append ls_tool_stru to lt_tool_stru .
    clear ls_tool_stru .

    ls_tool_stru-function   = 'PROC' .
    ls_tool_stru-text       = 'On/Off' .
    ls_tool_stru-quickinfo  = 'Liga / Desliga' .
    ls_tool_stru-icon       = icon_oo_overwrite .
    append ls_tool_stru to lt_tool_stru .
    clear ls_tool_stru .

    append lines of lt_tool_stru to e_object->mt_toolbar.

  endmethod.                    "handler_toolbar


  method executa_on_off .

    data:
      lt_index_rows	type lvc_t_row,
      ls_index_rows type lvc_s_row,
      lt_row_no	    type lvc_t_roid.

    field-symbols:
      <fs_outtab> like line of gt_outtab .

    call method lo_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows
        et_row_no     = lt_row_no.

    loop at lt_index_rows into ls_index_rows .
      read table gt_outtab assigning <fs_outtab>
      index ls_index_rows-index .
      if sy-subrc eq 0 .
        if <fs_outtab>-icon eq '@S_POSI@' .
          <fs_outtab>-icon = '@S_NEGA@' .
        else.
          <fs_outtab>-icon = '@S_POSI@' .
        endif .
      endif .
    endloop .

    me->refresh( ) .

  endmethod .                    "executa_on_off

  method refresh .

    call method lo_grid->refresh_table_display .
    call method cl_gui_cfw=>flush.

  endmethod .                    "refresh

endclass.                    "gcl_report IMPLEMENTATION
