# Relatório ALV CL_GUI_ALV_GRID #

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)

Existem varios exemplos e modelos diferente de usar a classe ´CL_GUI_ALV_GRID´ para exibir relatórios ALV. Sempre que a classe é utilizada, ela necessita de um ´container´ para que o ALV seja exibido. Ao inves de criar um container pra isso, eu preferi utilizar o próprio ´container´, que é gerado quando se cria a tela ´1000´ em um relatório, que no caso, é a tela de seleção. Sim, eu posso (e vou) utilizar o container na tela de seleção para exibir o ALV como eu aprendi com [Gerson Lívio](gerson@litsolutions.com.br>).

Para isso eu criei uma classe local basica ´GCL_REPORT´ com os seguintes métodos:

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
Para que fique melhor o entendimento, optei por colocar menos informações e mais funcionalidades. A tabela *MAKT - Textos breves de material* foi escolhida apenas por ser relacionada ao modulo que eu tratava quando desenvolvi a solução.

### public section ###
Métodos da sessão publica.
#### search ####
```abap
  method search .

    if s_matnr[] is not initial .

      select matnr maktx
      into table gt_outtab
      from makt
      where matnr in s_matnr .

    endif.

  endmethod .                    "search
```
### protected section ###
Métodos da sessão protegida.

### public private ###
Métodos da sessão privada.


:+1:

