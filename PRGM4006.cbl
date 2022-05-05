      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      05/05/2022.
      * Purpose:   MELHORES USOS DO IF THE ELSE.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4006.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-TABELA.
           03 WS-REGISTRO                 OCCURS 5 TIMES
                                          ASCENDING KEY WS-CHAVE
                                          INDEXED   BY  I.
               05 WS-CHAVE                PIC 99.
               05 WS-NOME                 PIC X(06).
       77  WS-CODIGO                      PIC 99.

       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM
                   UNTIL WS-CODIGO EQUALS TO 99.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       PROCEDURE-MAIN-FIM.

       P100-INICIO.
           INITIALISE  WS-TABELA WS-CODIGO
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
           MOVE '01MARCOS02CARLOS03MARINA04ANA   05LOPES'
               TO WS-TABELA.

           DISPLAY "INICIO PROCESSAMENTO.".
       P100-INICIO-FIM.

       P200-PROCESSA.
           DISPLAY 'PARA SAIR INFORME CODIGO IGUAL <99> '.
           DISPLAY 'INFORME O CÓDIGO PARA BUSCA: '.
           ACCEPT WS-CODIGO.
           IF WS-CODIGO NOT EQUALS TO 99 THEN
           SEARCH ALL WS-REGISTRO
                  AT END
                     DISPLAY 'DADO NAO ENCONTRADO'
                  WHEN WS-CHAVE(WS-CODIGO) EQUALS TO WS-CODIGO
                     DISPLAY 'ENCONTRADO: ' WS-CHAVE(WS-CODIGO)
                             ' - '          WS-NOME(WS-CODIGO)
                             ' POSICAO: '   WS-CODIGO
           END-SEARCH.
       P200-PROCESSA-FIM.

       P350-ERRO.
           DISPLAY 'MEIO DO PROCESSAMENTO...'.
      *     DISPLAY "ERRO NO CALCULO.........:" WS-VALOR.
           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.
       P350-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGM4006.
