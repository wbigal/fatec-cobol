       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          EX03.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        09/04/2017.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      SELECIONA OS REGISTROS DE CLIENTES DO
      *              SEXO FEMININO COM SALARIO SUPERIOR OU
      *              IGUAL A 5.000,00.

      *==============================================================
       ENVIRONMENT          DIVISION.

       CONFIGURATION        SECTION.
       SOURCE-COMPUTER.     IBM-PC.
       OBJECT-COMPUTER.     IBM-PC.
       SPECIAL-NAMES.       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
              SELECT CADENT ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT RELSAI ASSIGN TO DISK.

      *==============================================================
       DATA                 DIVISION.

       FILE                 SECTION.

       FD     CADENT
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADENT.DAT".
       01     REG-ENT.
              02 CODIGO            PIC 9(07).
              02 NOME              PIC X(30).
              02 SALARIO           PIC 9(05)V9(02).
              02 SEXO              PIC X(01).

       FD     RELSAI
              LABEL RECORD IS OMITTED.
       01     REG-REL              PIC X(80).

       WORKING-STORAGE      SECTION.
       77 FIM-ARQ                  PIC X(03) VALUE "NAO".
       77 CT-LIN                   PIC 9(02) VALUE 20.
       77 CT-PAG                   PIC 9(02) VALUE ZEROES.
       77 TOTAL-CLIENTES           PIC 9(05) VALUE ZEROES.
       77 TOTAL-SALARIOS           PIC 9(09)V9(02) VALUE ZEROES.

       01     CAB-01.
              02 FILLER            PIC X(70) VALUE SPACES.
              02 FILLER            PIC X(05) VALUE "PAG. ".
              02 VAR-PAG           PIC ZZ9.
              02 FILLER            PIC X(03) VALUE SPACES.

       01     CAB-02.
              02 FILLER            PIC X(21) VALUE SPACES.
              02 FILLER            PIC X(38)
                 VALUE "LISTAGEM DOS CLIENTES DO SEXO FEMININO".
              02 FILLER            PIC X(21) VALUE SPACES.

       01     CAB-03.
              02 FILLER            PIC X(23) VALUE SPACES.
              02 FILLER            PIC X(34)
                 VALUE "COM SALARIOS SUPERIORES A 5.000,00".
              02 FILLER            PIC X(23).

       01     CAB-04.
              02 FILLER            PIC X(10) VALUE SPACES.
              02 FILLER            PIC X(06) VALUE "CODIGO".
              02 FILLER            PIC X(06) VALUE SPACES.
              02 FILLER            PIC X(04) VALUE "NOME".
              02 FILLER            PIC X(32) VALUE SPACES.
              02 FILLER            PIC X(07) VALUE "SALARIO".
              02 FILLE             PIC X(15) VALUE SPACES.

       01     CAB-05.
              02 FILLER            PIC X(80) VALUE SPACES.

       01     DETALHE.
              02 FILLER            PIC X(10) VALUE SPACES.
              02 VAR-CODIGO        PIC 9999999.
              02 FILLER            PIC X(05) VALUE SPACES.
              02 VAR-NOME          PIC X(30).
              02 FILLER            PIC X(06) VALUE SPACES.
              02 VAR-SALARIO       PIC ZZ.999,99.
              02 FILLER            PIC X(13) VALUE SPACES.

       01     ROD-01.
              02 FILLER            PIC X(05) VALUE SPACES.
              02 FILLER            PIC X(32)
                 VALUE "Total de Clientes impressos:    ".
              02 VAR-TOTAL         PIC ZZ.999.
              02 FILLER            PIC X(37) VALUE SPACES.

       01     ROD-02.
              02 FILLER            PIC X(05) VALUE SPACES.
              02 FILLER            PIC X(31)
                 VALUE "Total de Salario:              ".
              02 VAR-TTL-SALARIOS  PIC ZZZ.ZZZ.ZZ9,99.
              02 FILLER            PIC X(29) VALUE SPACES.

      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EX02.
              PERFORM INICIO.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM FIM.
              STOP RUN.

       INICIO.
              OPEN   INPUT  CADENT
                     OUTPUT RELSAI.
              PERFORM LEITURA.

       LEITURA.
              READ CADENT AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
              PERFORM SELECIONA.
              PERFORM LEITURA.

       SELECIONA.
              IF SEXO EQUAL "F" AND SALARIO NOT LESS THAN 5000,00
                     PERFORM IMPRESSAO.

       IMPRESSAO.
              IF CT-LIN GREATER THAN 19
                     PERFORM CABECALHO.
              PERFORM IMPED.

       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE   CT-PAG         TO VAR-PAG.
              MOVE   SPACES         TO REG-REL.
              WRITE  REG-REL FROM CAB-01.
              WRITE  REG-REL FROM CAB-02 AFTER ADVANCING 1 LINE.
              WRITE  REG-REL FROM CAB-03 AFTER ADVANCING 1 LINE.
              WRITE  REG-REL FROM CAB-04 AFTER ADVANCING 3 LINE.
              WRITE  REG-REL FROM CAB-05 AFTER ADVANCING 1 LINE.
              MOVE   ZEROES TO CT-LIN.

       IMPED.
              MOVE   CODIGO        TO VAR-CODIGO.
              MOVE   NOME          TO VAR-NOME.
              MOVE   SALARIO       TO VAR-SALARIO.
              WRITE  REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1         TO CT-LIN.
              ADD 1         TO TOTAL-CLIENTES.
              ADD SALARIO   TO TOTAL-SALARIOS.

       FIM.
              IF TOTAL-CLIENTES GREATER THAN ZEROES
                     PERFORM RODAPE.

              CLOSE  CADENT
                     RELSAI.

       RODAPE.
              MOVE TOTAL-CLIENTES TO VAR-TOTAL.
              MOVE TOTAL-SALARIOS  TO VAR-TTL-SALARIOS.
              WRITE REG-REL FROM ROD-01 AFTER ADVANCING 3 LINES.
              WRITE REG-REL FROM ROD-02 AFTER ADVANCING 1 LINE.
