       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          EX06.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        10/04/2017.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      VERIFICA AS INFORMACOES DA LISTA DE CADASTROS
                     E GERA UM ARQUIVO .DAT PARA OS CADASTROS VALIDOS
                     E UM RELATORIO COM OS INVALIDOS.

      *==============================================================
       ENVIRONMENT          DIVISION.

       CONFIGURATION        SECTION.
       SOURCE-COMPUTER.     IBM-PC.
       OBJECT-COMPUTER.     IBM-PC.
       SPECIAL-NAMES.       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
              SELECT CADCLI ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT CADOK ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT RELOCOR ASSIGN TO DISK.

      *==============================================================
       DATA                 DIVISION.

       FILE                 SECTION.

       FD     CADCLI
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADCLI.DAT".
       01     REG-ENT.
              02 CODIGO            PIC 9(03).
              02 CPF               PIC 9(11).
              02 NOME              PIC X(30).
              02 ESTADO            PIC X(02).
              02 CIDADE            PIC X(30).
              02 EMAIL             PIC X(30).
              02 TELEFONE          PIC 9(10).

       FD     CADOK
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADOK.DAT".
       01     REG-OK.
              02 CPF2              PIC 9(11).
              02 NOME2             PIC X(30).
              02 ESTADO2           PIC X(02).
              02 CIDADE2           PIC X(30).
              02 EMAIL2            PIC X(30).
              02 TELEFONE2         PIC 9(10).

       FD     RELOCOR
              LABEL RECORD IS OMITTED.
       01     REG-REL              PIC X(80).

       WORKING-STORAGE      SECTION.
       77 FIM-ARQ                  PIC X(03) VALUE "NAO".
       77 CT-LIN                   PIC 9(02) VALUE 40.
       77 CT-PAG                   PIC 9(02) VALUE ZEROES.
       77 CT-INVALIDOS             PIC 9(01) VALUE ZEROES.
       77 CT-EXIBE-INVLD           PIC 9(01) VALUE ZEROES.
       77 MSG-INVALIDO             PIC X(20) OCCURS 5 TIMES.
       77 CT-ESTADOS               PIC 9(02) VALUE ZEROES.

      * VARIAVEIS PARA A VALIDACAO DO CPF
       77 TEMP-CPF          PIC 9(11).
       77 TEMP-DIVISAO      PIC 9(11).
       77 ATUAL-VALOR       PIC 9(02).
       77 POS-DIGITO        PIC 9(02) VALUE 2.
       77 RESULT-DIGITO     PIC 9(03).
       77 SOMATORIA         PIC 9(03).
       77 QUOCIENTE         PIC 9(03).
       77 RESTO             PIC 9(02).
       77 DIGITO1           PIC 9(01).
       77 DIGITO2           PIC 9(01).
       77 DIGITO-VRF        PIC 9(02).
       77 DIGITO-VRF-TMP    PIC 9(02).
      *************************************

       01     TABELA-ESTADO.
              02 DEF-ESTADO.
                     03 FILLER     PIC X(02) VALUE "AC".
                     03 FILLER     PIC X(02) VALUE "AL".
                     03 FILLER     PIC X(02) VALUE "AP".
                     03 FILLER     PIC X(02) VALUE "AM".
                     03 FILLER     PIC X(02) VALUE "BA".
                     03 FILLER     PIC X(02) VALUE "CE".
                     03 FILLER     PIC X(02) VALUE "DF".
                     03 FILLER     PIC X(02) VALUE "ES".
                     03 FILLER     PIC X(02) VALUE "GO".
                     03 FILLER     PIC X(02) VALUE "MA".
                     03 FILLER     PIC X(02) VALUE "MT".
                     03 FILLER     PIC X(02) VALUE "MS".
                     03 FILLER     PIC X(02) VALUE "MG".
                     03 FILLER     PIC X(02) VALUE "PA".
                     03 FILLER     PIC X(02) VALUE "PB".
                     03 FILLER     PIC X(02) VALUE "PR".
                     03 FILLER     PIC X(02) VALUE "PE".
                     03 FILLER     PIC X(02) VALUE "PI".
                     03 FILLER     PIC X(02) VALUE "RJ".
                     03 FILLER     PIC X(02) VALUE "RN".
                     03 FILLER     PIC X(02) VALUE "RS".
                     03 FILLER     PIC X(02) VALUE "RO".
                     03 FILLER     PIC X(02) VALUE "RR".
                     03 FILLER     PIC X(02) VALUE "SC".
                     03 FILLER     PIC X(02) VALUE "SP".
                     03 FILLER     PIC X(02) VALUE "SE".
                     03 FILLER     PIC X(02) VALUE "TO".

       01     TABELA-ESTADOS-RED REDEFINES TABELA-ESTADO.
              02 ESTADOS           PIC X(02) OCCURS 27 TIMES.

       01     CAB-01.
              02 FILLER            PIC X(31) VALUE SPACES.
              02 FILLER            PIC X(17) VALUE "TURISMAR TURISMO".
              02 FILLER            PIC X(32) VALUE SPACES.

       01     CAB-02.
              02 FILLER            PIC X(17) VALUE SPACES.
              02 FILLER            PIC X(45)
                 VALUE "RELATORIO DE DADOS DE CLIENTES INCONSISTENTES".
              02 FILLER            PIC X(18).

       01     CAB-03.
              02 FILLER            PIC X(10) VALUE SPACES.
              02 FILLER            PIC X(17) VALUE "CODIGO DO CLIENTE".
              02 FILLER            PIC X(06) VALUE SPACES.
              02 FILLER            PIC X(15) VALUE "DADOS INVALIDOS".
              02 FILLER            PIC X(32) VALUE SPACES.

       01     CAB-04.
              02 FILLER            PIC X(80) VALUE ALL "-".

       01     DETALHE-01.
              02 FILLER            PIC X(10) VALUE SPACES.
              02 VAR-CODIGO        PIC 999.
              02 FILLER            PIC X(20) VALUE SPACES.
              02 VAR-INVALIDOS     PIC X(37).
              02 FILLER            PIC X(10) VALUE SPACES.


       01     DETALHE-02.
              02 FILLER            PIC X(33) VALUE SPACES.
              02 VAR-INVALIDOS2    PIC X(37).
              02 FILLER            PIC X(10) VALUE SPACES.

      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EX02.
              PERFORM INICIO.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM FIM.
              STOP RUN.

       INICIO.
              OPEN   INPUT  CADCLI
                     OUTPUT CADOK
                     OUTPUT RELOCOR.
              PERFORM LEITURA.

       LEITURA.
              READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
              PERFORM VALIDACAO.
              PERFORM LEITURA.

       VALIDACAO.
              PERFORM LIMPAR-INVALIDOS.
              PERFORM VALIDA-CPF.
              PERFORM VALIDA-NOME.
              PERFORM VALIDA-ESTADO.
              PERFORM VALIDA-CIDADE.
              PERFORM VALIDA-EMAIL.
              IF CT-INVALIDOS EQUAL ZEROES
                     PERFORM GRAVAOK
              ELSE
                     PERFORM IMPRESSAO.

       LIMPAR-INVALIDOS.
              MOVE ZEROES TO CT-INVALIDOS.
              MOVE SPACES TO MSG-INVALIDO(1).
              MOVE SPACES TO MSG-INVALIDO(2).
              MOVE SPACES TO MSG-INVALIDO(3).
              MOVE SPACES TO MSG-INVALIDO(4).
              MOVE SPACES TO MSG-INVALIDO(5).

       VALIDA-CPF.
              IF CPF EQUAL ZEROES OR EQUAL SPACES
                     PERFORM CPF-INVALIDO
              ELSE
                     PERFORM VALIDA-CPF-FORMATO.

       CPF-INVALIDO.
              ADD 1 TO CT-INVALIDOS.
              MOVE "CPF INVALIDO" TO MSG-INVALIDO(CT-INVALIDOS).

       VALIDA-CPF-FORMATO.
              PERFORM CALCULA-DIGITO-1.
              PERFORM CALCULA-DIGITO-2.
              DIVIDE CPF BY 100 GIVING TEMP-CPF REMAINDER DIGITO-VRF.
              MULTIPLY DIGITO1 BY 10 GIVING DIGITO-VRF-TMP.
              ADD DIGITO2 TO DIGITO-VRF-TMP.

              IF DIGITO-VRF-TMP NOT EQUAL DIGITO-VRF
                     PERFORM CPF-INVALIDO.

       CALCULA-DIGITO-1.
              MOVE CPF      TO TEMP-CPF.
              MOVE 2        TO POS-DIGITO.
              MOVE ZEROES   TO SOMATORIA.
              DIVIDE TEMP-CPF BY 100
                              GIVING TEMP-CPF.
              PERFORM CALCULA-DIGITO 9 TIMES.

              DIVIDE SOMATORIA BY 11
                               GIVING QUOCIENTE
                               REMAINDER RESTO.
              IF RESTO LESS THAN 2
                     MOVE 0 TO DIGITO1
              ELSE
                     SUBTRACT 11 FROM RESTO GIVING DIGITO1.

       CALCULA-DIGITO-2.
              MOVE CPF      TO TEMP-CPF.
              MOVE 2        TO POS-DIGITO.
              MOVE ZEROES   TO SOMATORIA.
              DIVIDE TEMP-CPF BY 10
                              GIVING TEMP-CPF.
              PERFORM CALCULA-DIGITO 10 TIMES.

              DIVIDE SOMATORIA BY 11
                               GIVING QUOCIENTE
                               REMAINDER RESTO.

              IF RESTO LESS THAN 2
                     MOVE 0 TO DIGITO2
              ELSE
                     SUBTRACT 11 FROM RESTO GIVING DIGITO2.

       CALCULA-DIGITO.
              DIVIDE TEMP-CPF BY 10
                              GIVING TEMP-CPF
                              REMAINDER ATUAL-VALOR.
              MULTIPLY ATUAL-VALOR BY POS-DIGITO GIVING RESULT-DIGITO.
              ADD 1 TO POS-DIGITO.
              ADD RESULT-DIGITO TO SOMATORIA.

       VALIDA-NOME.
              IF NOME EQUAL SPACES
                     PERFORM NOME-BRANCO.

       NOME-BRANCO.
              ADD 1 TO CT-INVALIDOS.
              MOVE "NOME NAO INFORMADO" TO MSG-INVALIDO(CT-INVALIDOS).

       VALIDA-ESTADO.
              MOVE ZEROES TO CT-ESTADOS.
              IF ESTADO EQUAL SPACES
                     PERFORM ESTADO-INVALIDO
              ELSE
                     PERFORM VALIDA-ESTADO-EXISTE.

       VALIDA-ESTADO-EXISTE.
              ADD 1 TO CT-ESTADOS.
              IF CT-ESTADOS NOT LESS THAN 28
                     PERFORM ESTADO-INVALIDO
              ELSE IF ESTADOS(CT-ESTADOS) NOT EQUAL ESTADO
                     PERFORM VALIDA-ESTADO-EXISTE.       

       ESTADO-INVALIDO.
              ADD 1 TO CT-INVALIDOS.
              MOVE "ESTADO INVALIDO" TO MSG-INVALIDO(CT-INVALIDOS).

       VALIDA-CIDADE.
              IF CIDADE EQUAL SPACES
                     PERFORM CIDADE-BRANCO.

       CIDADE-BRANCO.
              ADD 1 TO CT-INVALIDOS.
              MOVE "CIDADE NAO INFORMADA" TO MSG-INVALIDO(CT-INVALIDOS).

       VALIDA-EMAIL.
              IF EMAIL EQUAL SPACES AND NOT EQUAL "NAO TEM"
                     PERFORM EMAIL-BRANCO.

       EMAIL-BRANCO.
              ADD 1 TO CT-INVALIDOS.
              MOVE "EMAIL SEM DESCRICAO" TO MSG-INVALIDO(CT-INVALIDOS).

       GRAVAOK.
              MOVE   CPF           TO CPF2.
              MOVE   NOME          TO NOME2.
              MOVE   ESTADO        TO ESTADO2.
              MOVE   CIDADE        TO CIDADE2.
              MOVE   EMAIL         TO EMAIL2.
              MOVE   TELEFONE      TO TELEFONE2.
              WRITE  REG-OK.

       IMPRESSAO.
              IF CT-LIN GREATER THAN 39
                     PERFORM CABECALHO.
              PERFORM IMPED.

       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE   SPACES         TO REG-REL.
              WRITE  REG-REL FROM CAB-01.
              WRITE  REG-REL FROM CAB-02 AFTER ADVANCING 2 LINE.
              WRITE  REG-REL FROM CAB-03 AFTER ADVANCING 2 LINE.
              WRITE  REG-REL FROM CAB-04 AFTER ADVANCING 1 LINE.
              MOVE   ZEROES TO CT-LIN.

       IMPED.
              MOVE   CODIGO        TO VAR-CODIGO.
              MOVE   1             TO CT-EXIBE-INVLD.
              MOVE   MSG-INVALIDO(CT-EXIBE-INVLD) TO VAR-INVALIDOS.
              WRITE  REG-REL FROM DETALHE-01 AFTER ADVANCING 2 LINE.
              ADD 1 TO CT-LIN.

              IF CT-INVALIDOS GREATER THAN 1
                     PERFORM IMPERRO.

       IMPERRO.
              SUBTRACT 1 FROM CT-INVALIDOS.
              ADD      1 TO CT-EXIBE-INVLD.
              MOVE   MSG-INVALIDO(CT-EXIBE-INVLD) TO VAR-INVALIDOS2.
              WRITE  REG-REL FROM DETALHE-02 AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.
              PERFORM IMPERRO UNTIL CT-INVALIDOS EQUAL 1.

       FIM.
              CLOSE  CADCLI
                     CADOK
                     RELOCOR.
