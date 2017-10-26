# Relatório ALV CL_GUI_ALV_GRID #

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)
Existem vários exemplos e modelos diferente de usar a classe `CL_GUI_ALV_GRID` para exibir relatórios ALV. Sempre que a classe é utilizada, ela necessita de um `container` para que o ALV seja exibido. Ao invés de criar um container pra isso, eu preferi utilizar o próprio `container`, que é gerado quando se cria a tela `1000` em um relatório, que no caso, é a tela de seleção. Sim, eu ~~posso~~ vou utilizar o container na tela de seleção para exibir o ALV como eu aprendi com [Gerson Lívio](mailto:gerson@litsolutions.com.br).
A funcionalidade de `SELECT ROWS`

Para isso eu criei uma classe local básica `GCL_REPORT` com os seguintes métodos:

* public section
	* search
	* generate_grid
	* public section
	* search
	* generate_grid

* protected

* private section
	* fieldcat
	* call_screen_default
	* handler_toolbar
	* handler_user_command
	* handler_hotspot_click
	* executa_on_off
	* refresh

## Informações exibidas ##
Para que fique melhor o entendimento, optei por colocar menos informações e mais funcionalidades. A tabela `MAKT - Textos breves de material` foi escolhida apenas por ser relacionada ao modulo que eu tratava quando desenvolvi a solução.

### public section ###
Métodos da sessão publica.
#### search ####
Este tem como objetivo buscar no banco de dados as informações para que sejam exibidas no relatório.
```abap
method search .

    if lines( matnr ) gt 0 .

      select matnr maktx
        into table gt_outtab
        from makt
       where matnr in matnr .

    endif.

  endmethod .                    "search
```
#### generate_grid ####
Apos a busca dos dados, nesta rotina será feita a criação do objeto `lo_grid` da classe `cl_gui_alv_grid`, a composição ~~estática~~ dinâmica do `fieldcat`, configuração de layout de saída, ordenação de campos, atribuição de eventos e **utilização do mesmo** `container` para exibição do relatório. As chamadas de métodos `private` serão exemplificadas na respectiva sessão.
```abap 
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
    set handler handler_hotspot_click for lo_grid.
    set handler handler_user_command  for lo_grid.
    set handler handler_toolbar       for lo_grid.

    lo_grid->refresh_table_display( ).

*   Exibir relatório na tela default do report
    call_screen_default( ).

  endmethod.                    "generate_grid
```
### protected section ###
Não foram implementados métodos para essa sessão.

### public private ###
#### fieldcat ####
Essa rotina é excelente para que seja criado o `fieldcat`. Eu julgo muito maçante e improdutivo a criação sendo feita de forma que sejam informados todos os campos da estrutura de saída. Desta forma que foi implementado, utiliza-se o método `describe_by_data` da classe standard `cl_abap_structdescr`, desde forma, o objeto retornado (no caso `lobj_stdesc`) tem acesso a todas as características do campo e pode utilizar estas informações para que seja feita a criação do `fieldcat` de forma mais produtiva.
```abap
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

```
#### call_screen_default ####
Esse método é importante para o aproveitamento de tela. Caso não fosse esse implementação, teria que ser criada uma nova tela com um `container`(que é a premissa para o uso do `CL_GUI_ALV_GRID`. Isso é feito na instrução `call selection-screen 1000`. Alem da seleção da tela, tambem é feita a exclusão de botões que aparecem na tela de seleção, visto que alguns botões não tem utilidade depois do relatório gerado.

```abap
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
```
#### handler_toolbar ####
Neste método, são informados os novos botões/funcionalidade que o relatório irá contemplar. No caso, ele terá um botão _on/off_ (conforme imagem abaixo) para fazer uma ação que depois pode ser alterada conforme a necessidade. Para ficar mais organizado, achei melhor colocar uma divisória entre os botões standard e o que eu adicionei.
[![N|Solid](https://uploaddeimagens.com.br/images/001/149/485/original/2017-10-26_091903.png)](#)
```abap
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
  ```
#### handler_user_command ####
Este método recupera a execução após o relatório ser gerado. Trata os botões que são adicionados para novas funcionalidades.
```abap
  method: handler_user_command.

    case e_ucomm.

      when 'PROC'.
        executa_on_off( ) .

    endcase.

  endmethod.                    "handler_user_command
  ```
  #### handler_hotspot_click ####
Usado para facilitar a interpretação das informações, direcionando para transação de exibição.
```abap
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
  ```
#### executa_on_off ####
Um exemplo criado para que seja feita alguma alteração nas informações ao clicar no botão. Desta forma, o botão que é exibido é alterado a cada vez que é clicado o botão que foi adicionado. Claro que essa ação é um exemplo e pode ser adequada de acordo com o cenário.
```abap
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
```
#### refresh ####
Atualizar as informações de exibição do ALV.
```abap
  method refresh .

    call method lo_grid->refresh_table_display .
    call method cl_gui_cfw=>flush.

  endmethod .                    "refresh
```

Desta forma, tem-se o relatório ALV usando um mesmo `container` e sem a necessidade de criação de novas telas.
:+1:

