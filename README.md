# Relatório ALV CL_GUI_ALV_GRID #

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)
Existem varios exemplos e modelos diferente de usar a classe `CL_GUI_ALV_GRID` para exibir relatórios ALV. Sempre que a classe é utilizada, ela necessita de um `container` para que o ALV seja exibido. Ao inves de criar um container pra isso, eu preferi utilizar o próprio `container`, que é gerado quando se cria a tela `1000` em um relatório, que no caso, é a tela de seleção. Sim, eu ~~posso~~ vou utilizar o container na tela de seleção para exibir o ALV como eu aprendi com [Gerson Lívio](mailto:gerson@litsolutions.com.br).
A funcionalidade de `SELECT ROWS`

Para isso eu criei uma classe local basica `GCL_REPORT` com os seguintes métodos:

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
Apos a busca dos dados, nesta rotina será feita a criação do objeto `lo_grid` da classe `cl_gui_alv_grid`, a composição ~~estática~~ dinâmica do `fieldcat`, configuração de layout de saída, ordenção de campos, atribuição de eventos e **utilização do mesmo** `container` para exibição do relatório. As chamadas de métodos `private` serão exemplificadas na respectiva sessão.
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
Métodos da sessão protegida.

### public private ###
Métodos da sessão privada.


:+1:

