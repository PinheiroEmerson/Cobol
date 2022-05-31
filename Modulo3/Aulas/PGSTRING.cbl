      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGSTRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-AUX.
           05  WS-TM-1                     PIC 99.
           05  WS-TM-2                     PIC 99.
           05  WS-TM-3                     PIC 99.
       COPY 'LAYOUT001'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM P100-INICIO   THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM.
           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       P100-INICIO.
           INITIALISE WS-AUX
                   REPLACING NUMERIC BY ZEROES.
       P100-INICIO-FIM.

       P200-PROCESSA.
           MOVE 'MARIA'                TO WS-PRIMEIRO-NOME.
           MOVE 'CAMPOS'               TO WS-ULTIMO-NOME.
           MOVE '5551985109610'        TO WS-TELEFONE.
           MOVE 'RUA DEZ, 03'          TO WS-RUA.
           MOVE 'SAO JOSE'             TO WS-BAIRRO.
           MOVE 'SAO PAULO'            TO WS-CIDADE.
           MOVE 'SP'                   TO WS-UF.
           MOVE '01120360'             TO WS-CEP.
           MOVE 'BRASILEIRA'           TO WS-NACIONALIDADE.
           MOVE 'ENFERMEIRA'           TO WS-PROFISSAO.

           PERFORM P400-SEM-TRATAMENTO  THRU P400-SEM-TRATAMENTO-FIM.
           PERFORM P300-TRABALHA-CAMPOS THRU P300-TRABALHA-CAMPOS-FIM.
       P200-PROCESSA-FIM.

       P300-TRABALHA-CAMPOS.

      *TIRAR ESPAÇOS DO NOME E MOSTRAR FORMATADO NO VIDEO
           INSPECT FUNCTION REVERSE(WS-PRIMEIRO-NOME)
                   TALLYING WS-TM-1 FOR LEADING ' '.

           INSPECT FUNCTION REVERSE(WS-ULTIMO-NOME)
                   TALLYING WS-TM-2 FOR LEADING ' '.
           DISPLAY ' '.
           DISPLAY 'COM TRATAMENTO DAS STRINGS.'.
           DISPLAY '1 NOME COMPLETO: '
               WS-PRIMEIRO-NOME
                   (1:(FUNCTION LENGTH(WS-PRIMEIRO-NOME) - WS-TM-1))
                   ' '
               WS-ULTIMO-NOME
                   (1:(FUNCTION LENGTH(WS-ULTIMO-NOME)   - WS-TM-2)).
           DISPLAY '2 TELEFONE: ' '+' WS-PAIS ' (' WS-DDD ') '
                   WS-PREFIXO '-' WS-SUFIXO.
      *LIMPA VARIAVEIS
           PERFORM P100-INICIO         THRU P100-INICIO-FIM.

      *TRABALHA COM O ENDERECO
           INSPECT FUNCTION REVERSE(WS-RUA)
                   TALLYING WS-TM-1 FOR LEADING ' '.

           INSPECT FUNCTION REVERSE(WS-BAIRRO)
                   TALLYING WS-TM-2 FOR LEADING ' '.

           INSPECT FUNCTION REVERSE(WS-CIDADE)
                   TALLYING WS-TM-3 FOR LEADING ' '.

           DISPLAY '3 ENDERECO: '
               WS-RUA
                   (1:(FUNCTION LENGTH(WS-RUA) - WS-TM-1))
                   ' - '
               WS-BAIRRO
                   (1:(FUNCTION LENGTH(WS-BAIRRO) - WS-TM-2))
           DISPLAY '            '
               WS-CIDADE
                   (1:(FUNCTION LENGTH(WS-CIDADE) - WS-TM-3))
           DISPLAY '            '
                   WS-UF
               FUNCTION CONCATENATE(' - CEP: '
                                     WS-CEP-1
                                     '-'
                                     WS-CEP-2).
           DISPLAY '4 NACIONALIDADE: ' WS-NACIONALIDADE.
           DISPLAY '5 PROFISSAO: '     WS-PROFISSAO.
       P300-TRABALHA-CAMPOS-FIM.

       P400-SEM-TRATAMENTO.
           DISPLAY 'SEM TRATAMENTO DAS STRINGS.'.
           DISPLAY '1 - NOME COMPLETO: '   WS-PRIMEIRO-NOME
                                           WS-ULTIMO-NOME.
           DISPLAY '2 - TELEFONE: '        WS-TELEFONE.
           DISPLAY '3 - ENDERECO: '        WS-ENDERECO.
           DISPLAY '4 - NACIONALIDADE: '   WS-NACIONALIDADE.
           DISPLAY '5 - PROFISSAO: '       WS-PROFISSAO.

       P400-SEM-TRATAMENTO-FIM.

       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PGSTRING.
