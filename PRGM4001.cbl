      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      05/05/2022.
      * Purpose:   USO DO OCCURS - ARRAY ESTATICO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4001.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FINANCIAMENTO.
           03 WS-CLIENTE               PIC X(20).
           03 WS-PRODUTO               PIC X(20).
           03 WS-VALOR                 PIC 9(06)V99 COMP.
           03 WS-PARCELAS              PIC 9(05)V99 COMP
                                       OCCURS 12 TIMES.
       01  WS-VARIAVEIS.
           03 WS-VLR-PARCELAS          PIC 9(05)V99.
           03 WS-INDICE                PIC 99.
           03 WS-RESTO                 PIC 9(03)V99.
           03 WS-PARCELAS-MSK          PIC  ZZ.ZZZ,99.
           03 WS-VALOR-MSK             PIC ZZZ.ZZZ,99.

       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM
                   UNTIL WS-INDICE EQUALS TO 12.
           PERFORM P370-MOSTRA-CABECALHO THRU P370-MOSTRA-CABECALHO-FIM.
           PERFORM P360-MOSTRA-PARCELAS
                   THRU P360-MOSTRA-PARCELAS-FIM
                   VARYING WS-INDICE FROM 1 BY 1
                   UNTIL   WS-INDICE IS GREATER THAN 12.
           PERFORM P300-FINALIZA THRU P300-FINALIZA-FIM.
           PERFORM P400-PARA THRU P400-PARA-FIM.

       PROCEDURE-MAIN-FIM.


       P100-INICIO.
           INITIALISE WS-VARIAVEIS WS-FINANCIAMENTO.
           DISPLAY "INICIO PROCESSAMENTO.".
           DISPLAY 'INFORME O NOME DO CLIENTE: '
           ACCEPT WS-CLIENTE.
           DISPLAY 'QUAL O PRODUTO FINANCIADO? '
           ACCEPT WS-PRODUTO.
           DISPLAY 'QUAL VALOR DO PRODUTO? '
           ACCEPT WS-VALOR.
       P100-INICIO-FIM.

       P200-PROCESSA.
           ADD 1           TO WS-INDICE
           END-ADD.
           DIVIDE WS-VALOR BY 12 GIVING WS-VLR-PARCELAS
                           REMAINDER WS-RESTO
                           ON SIZE ERROR
                           PERFORM P350-ERRO THRU P350-ERRO-FIM
           END-DIVIDE.
           MOVE WS-VLR-PARCELAS TO WS-PARCELAS(WS-INDICE).
           MOVE WS-VALOR        TO WS-VALOR-MSK.
       P200-PROCESSA-FIM.

       P300-FINALIZA.
           DISPLAY "FINAL PROCESSAMENTO.".
       P300-FINALIZA-FIM.

       P350-ERRO.
           DISPLAY 'MEIO DO PROCESSAMENTO...'.
           DISPLAY "ERRO NO CALCULO.........:" WS-VALOR.
           PERFORM P400-PARA THRU P400-PARA-FIM.
       P350-ERRO-FIM.

       P370-MOSTRA-CABECALHO.
           DISPLAY 'O CLIENTE ' WS-CLIENTE.
           DISPLAY 'FINANCIOU O VALOR DE R$ ' WS-VALOR-MSK.
           DISPLAY 'EM 12 PARCELAS CONFORME ABAIXO: '.
           DISPLAY  'SOBROU DE RESTO DE DIVISAO -> R$ ' WS-RESTO.
       P370-MOSTRA-CABECALHO-FIM.

       P360-MOSTRA-PARCELAS.
           MOVE WS-PARCELAS(WS-INDICE) TO WS-PARCELAS-MSK.
           DISPLAY 'WS-PARCELAS - ' WS-INDICE
                   '  VALOR DA PARCELA: R$' WS-PARCELAS-MSK.
       P360-MOSTRA-PARCELAS-FIM.

       P400-PARA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P400-PARA-FIM.

       END PROGRAM PRGM4001.
