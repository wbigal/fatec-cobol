       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          EX02.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        09/04/2017.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      GERA UM RELATORIO COM OS CLIENTES DO PERIODO
      *              DE 2010 A 2011.

      *==============================================================
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      IBM-PC.
       OBJECT-COMPUTER.      IBM-PC.
       SPECIAL-NAMES.        DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT          SECTION.
       FILE-CONTROL.
              SELECT CADCLI ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT RELCLI ASSIGN TO DISK.

      *==============================================================
       DATA                 DIVISION.
       FILE                 SECTION.

       FD     CADCLI
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADCLI.DAT".
       01     REG-CLI.
              02 CPF                  PIC 9(11).
              02 NOME                 PIC X(30).
              02 ESTADO               PIC X(02).
              02 CIDADE               PIC X(30).
              02 TELEFONE             PIC 9(08).
              02 DATA-ULT-VISITA.
                     03 DIA           PIC 9(02).
                     03 MES           PIC 9(02).
                     03 ANO           PIC 9(04).

       FD     RELCLI
              LABEL RECORD IS OMITTED.
       01     REG-REL              PIC X(80).

       WORKING-STORAGE      SECTION.
       77     FIM-ARQ              PIC X(03) VALUE "NAO".
       77     CT-LIN               PIC 9(02) VALUE 25.
       77     CT-PAG               PIC 9(02) VALUE ZEROES.
       77     TOTAL-CLIENTES       PIC 9(04) VALUE ZEROES.

       01     CAB-01.
              02 FILLER            PIC X(70) VALUE SPACES.
              02 FILLER            PIC X(05) VALUE "PAG. ".
              02 VAR-PAG           PIC 99.
              02 FILLER            PIC X(03) VALUE SPACES.

       01     CAB-02.
              02 FILLER            PIC X(31) VALUE SPACES.
              02 FILLER            PIC X(17) VALUE "TURISMAR TURISMO".
              02 FILLER            PIC X(32) VALUE SPACES.

       01     CAB-03.
              02 FILLER            PIC X(22) VALUE SPACES.
              02 FILLER            PIC X(35)
                 VALUE "CLIENTES NO PERIODO DE: 2010 A 2011".
              02 FILLER            PIC X(23) VALUE SPACES.

       01     CAB-04.
              02 FILLER            PIC X(18) VALUE SPACES.
              02 FILLER            PIC X(04) VALUE "NOME".
              02 FILLER            PIC X(21) VALUE SPACES.
              02 FILLER            PIC X(06) VALUE "ESTADO".
              02 FILLER            PIC X(13) VALUE SPACES.
              02 FILLER            PIC X(08) VALUE "TELEFONE".
              02 FILLER            PIC X(10) VALUE SPACES.

       01     CAB-05.
              02 FILLER            PIC X(80) VALUE ALL "-".

       01     CAB-06.
              02 FILLER            PIC X(80) VALUE SPACES.

       01     DETALHE.
              02 FILLER            PIC X(06) VALUE SPACES.
              02 VAR-NOME          PIC X(30).
              02 FILLER            PIC X(09) VALUE SPACES.
              02 VAR-ESTADO        PIC X(02).
              02 FILLER            PIC X(15) VALUE SPACES.
              02 VAR-FONE          PIC 99999999.
              02 FILLER            PIC X(11) VALUE SPACES.

       01     ROD-01.
              02 FILLER            PIC X(06) VALUE SPACES.
              02 FILLER            PIC X(20)
                 VALUE "Total de Clientes: ".
              02 VAR-TOTAL         PIC 9999.
              02 FILLER            PIC X(50) VALUE SPACES.

      *==============================================================
       PROCEDURE            DIVISION.
       PGM-EX02.
              PERFORM INICIO.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM FIM.
              STOP RUN.

       INICIO.
              OPEN   INPUT  CADCLI
                     OUTPUT RELCLI.
              PERFORM LEITURA.

       LEITURA.
              READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
              PERFORM SELECAO.
              PERFORM LEITURA.

       SELECAO.
              IF ANO EQUAL 2010 OR 2011
                     PERFORM IMPRECAO.

       IMPRECAO.
              IF CT-LIN GREATER THAN 24
                     PERFORM CABECALHO.
              PERFORM IMPDET.

       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE   CT-PAG        TO VAR-PAG.
              MOVE   SPACES        TO REG-REL.
              WRITE  REG-REL FROM CAB-01.
              WRITE  REG-REL FROM CAB-02 AFTER ADVANCING 1 LINE.
              WRITE  REG-REL FROM CAB-03 AFTER ADVANCING 3 LINES.
              WRITE  REG-REL FROM CAB-04 AFTER ADVANCING 3 LINES.
              WRITE  REG-REL FROM CAB-05 AFTER ADVANCING 1 LINE.
              WRITE  REG-REL FROM CAB-06 AFTER ADVANCING 1 LINE.
              MOVE   ZEROES        TO CT-LIN.

       IMPDET.
              MOVE   NOME          TO VAR-NOME.
              MOVE   ESTADO        TO VAR-ESTADO.
              MOVE   TELEFONE      TO VAR-FONE.
              WRITE  REG-REL FROM DETALHE AFTER 1 LINE.
              ADD 1 TO TOTAL-CLIENTES.
              ADD 1 TO CT-LIN.

       FIM.
              IF TOTAL-CLIENTES GREATER THAN ZEROES
                     PERFORM RODAPE.
              CLOSE  CADCLI
                     RELCLI.

       RODAPE.
              MOVE   TOTAL-CLIENTES       TO VAR-TOTAL.
              WRITE  REG-REL FROM ROD-01 AFTER ADVANCING 3 LINES.
