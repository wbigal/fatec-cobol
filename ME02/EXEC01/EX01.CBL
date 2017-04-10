       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          EX01.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        09/04/2017.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS: ESTE PROGRAMA GERA UM ARQUIVO DE CONTAS
                PAGAS E UM RELATORIO DE CONTAS NAO PAGAS.

      *==============================================================
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SOURCE-COMPUTER.     IBM-PC.
       OBJECT-COMPUTER.     IBM-PC.
       SPECIAL-NAMES.       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
              SELECT CADSOC1 ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT CADSOC2 ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT RELSOCIO ASSIGN TO DISK.

      *==============================================================
       DATA                 DIVISION.
       FILE SECTION.

       FD     CADSOC1
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADSOC1.DAT".
       01     REG-SOC1.
              02 CODIGO-PAGAMENTO1 PIC 9(02).
              02 NUMERO-SOCIO1     PIC 9(06).
              02 NOME-SOCIO1       PIC X(30).
              02 VALOR-PAGAMENTO1  PIC 9(09)V9(02).

       FD     CADSOC2
              LABEL RECORD ARE STANDART
              VALUE OF FILE-ID IS "CADSOC2.DAT".
       01     REG-SOC2.
              02 NUMERO-SOCIO2     PIC 9(02).
              02 NOME-SOCIO2       PIC X(30).
              02 VALOR-PAGAMENTO2  PIC 9(09)V9(02).

       FD     RELSOCIO
              LABEL RECORD IS OMITTED.
       01     REG-REL              PIC X(80).              

       WORKING-STORAGE      SECTION.
       77 FIM-ARQ                  PIC X(03) VALUE "NAO".
       77 CT-LIN                   PIC 9(02) VALUE 30.
       77 CT-PAG                   PIC 9(02) VALUE ZEROES.
       77 TOTAL-SOCIOS             PIC 9(05) VALUE ZEROES.
       77 VALOR-TOTAL              PIC 9(12)V9(02) VALUE ZEROES.

       01     CAB-01.
              02 FILLER     PIC X(70) VALUE SPACES.
              02 FILLER     PIC X(05) VALUE "PAG. ".
              02 VAR-PAG    PIC 99.
              02 FILLER     PIC X(03) VALUE SPACES.

       01     CAB-02.
              02 FILLER     PIC X(26) VALUE SPACES.
              02 FILLER PIC X(27) VALUE "RELACAO DE SOCIOS ATRASADOS".
              02 FILLER     PIC X(25) VALUE SPACES.

       01     CAB-03.
              02 FILLER     PIC X(05) VALUE SPACES.
              02 FILLER     PIC X(15) VALUE "NUMERO DO SOCIO".
              02 FILLER     PIC X(11) VALUE SPACES.
              02 FILLER     PIC X(13) VALUE "NOME DO SOCIO".
              02 FILLER     PIC X(11) VALUE SPACES.
              02 FILLER     PIC X(18) VALUE "VALOR DO PAGAMENTO".
              02 FILLER     PIC X(7) VALUE SPACES.

       01     DETALHE.
              02 FILLER            PIC X(09) VALUE SPACES.
              02 NUMERO-SOCIO      PIC 999.999.
              02 FILLER            PIC X(06) VALUE SPACES.
              02 NOME-SOCIO        PIC X(30).
              02 FILLER            PIC X(03) VALUE SPACES.
              02 VALOR-PAGAMENTO   PIC ZZZ.ZZZ.ZZ9,99.
              02 FILLER            PIC X(11) VALUE SPACES.

       01     ROD-01.
              02 FILLER            PIC X(05) VALUE SPACES.
              02 FILLER PIC X(27) VALUE "Total de Socios atrasados: ".
              02 VAR-SOCIOS        PIC 99999.
              02 FILLER            PIC X(45) VALUE SPACES.

       01     ROD-02.
              02 FILLER            PIC X(05) VALUE SPACES.
              02 FILLER PIC X(27) VALUE "Valor Total atrasado     :".
              02 VAR-TOTAL         PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
              02 FILLER            PIC X(32) VALUES SPACES.

      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EX01.
              PERFORM INICIO.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM FIM.
              STOP RUN.

       INICIO.
              OPEN   INPUT  CADSOC1
                     OUTPUT CADSOC2
                     OUTPUT RELSOCIO.
              PERFORM LEITURA.

       LEITURA.
              READ CADSOC1 AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
              PERFORM SELECAO.
              PERFORM LEITURA.

       SELECAO.
              IF CODIGO-PAGAMENTO1 EQUAL 1
                     PERFORM GRAVACAO
              ELSE IF CODIGO-PAGAMENTO1 EQUAL 2
                     PERFORM RELATORIO.

       GRAVACAO.
              MOVE   NUMERO-SOCIO1        TO NUMERO-SOCIO2.
              MOVE   NOME-SOCIO1          TO NOME-SOCIO2.
              MOVE   VALOR-PAGAMENTO1     TO VALOR-PAGAMENTO2.
              WRITE  REG-SOC2.

       RELATORIO.
              ADD 1 TO TOTAL-SOCIOS.
              ADD VALOR-PAGAMENTO1 TO VALOR-TOTAL.
              PERFORM IMPRESSAO.

       IMPRESSAO.
              IF CT-LIN GREATER THAN 29
                     PERFORM CABECALHO.
              PERFORM IMPDET.

       IMPDET.
              MOVE   NUMERO-SOCIO1        TO NUMERO-SOCIO.
              MOVE   NOME-SOCIO1          TO NOME-SOCIO.
              MOVE   VALOR-PAGAMENTO1     TO VALOR-PAGAMENTO.
              WRITE  REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.

       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE   CT-PAG               TO VAR-PAG.
              MOVE   SPACES               TO REG-REL.
              WRITE  REG-REL FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE  REG-REL FROM CAB-02 AFTER ADVANCING 3 LINES.
              WRITE  REG-REL FROM CAB-03 AFTER ADVANCING 3 LINES.
              MOVE   ZEROES               TO CT-LIN.
              
       RODAPE.
              MOVE   SPACES               TO REG-REL.
              MOVE   TOTAL-SOCIOS         TO VAR-SOCIOS.
              MOVE   VALOR-TOTAL          TO VAR-TOTAL.
              WRITE  REG-REL FROM ROD-01 AFTER ADVANCING 3 LINES.
              WRITE  REG-REL FROM ROD-02 AFTER ADVANCING 1 LINE.

       FIM.
              IF TOTAL-SOCIOS GREATER THAN ZEROES
                     PERFORM RODAPE.
              CLOSE CADSOC1
                    CADSOC2
                    RELSOCIO.

