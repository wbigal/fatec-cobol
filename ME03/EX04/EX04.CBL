       IDENTIFICATION              DIVISION.

       PROGRAM-ID.          EX-04.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        14/05/2017.
       DATE-COMPILED.
       SECURITY.            APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      ESTE PROGRAMA LE UM ARQUIVO E GERA UM ARQUIVO
      *              RELATORIO COM OS DADOS DO CLIENTE ORDENADOS POR
      *              CPF.

      *==============================================================
       ENVIRONMENT          DIVISION.

       CONFIGURATION        SECTION.
       SOURCE-COMPUTER.     IBM-PC.
       OBJECT-COMPUTER.     IBM-PC.
       SPECIAL-NAMES.       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
              SELECT CAD-CLIENTE ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

              SELECT ARQ-SORT ASSIGN TO DISK.

              SELECT REL-GERAL ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

      *==============================================================
       DATA                 DIVISION.

       FILE                 SECTION.
       FD     CAD-CLIENTE
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CAD-CLIENTE.DAT".
       01     REG-ENT.
              02 CODIGO            PIC 9(03).
              02 CPF               PIC 9(11).
              02 NOME              PIC X(30).
              02 ESTADO            PIC X(02).
              02 CIDADE            PIC X(30).
              02 EMAIL             PIC X(30).

       SD     ARQ-SORT.
       01     TRAB-REG.
              02 CODIGO-TRAB       PIC 9(03).
              02 CPF-TRAB          PIC 9(11).
              02 NOME-TRAB         PIC X(30).
              02 ESTADO-TRAB       PIC X(02).
              02 CIDADE-TRAB       PIC X(30).
              02 EMAIL-TRAB        PIC X(30).

       FD     REL-GERAL
              LABEL RECORD IS OMITTED.
       01     REG-REL              PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ                  PIC X(03) VALUE "NAO".
       77 CT-LIN                   PIC 9(02) VALUE 20.
       77 CT-PAG                   PIC 9(02) VALUE ZEROES.
       77 ESTADO-ATUAL             PIC X(02) VALUE SPACES.
       77 CIDADE-ATUAL             PIC X(30) VALUE SPACES.

       01     CAB-01.
              02 FILLER            PIC X(15) VALUE SPACES.
              02 FILLER            PIC X(37)
                  VALUE "RELACAO DE CLIENTES POR ESTADO\CIDADE".
              02 FILLER            PIC X(04) VALUE SPACES.
              02 FILLER            PIC X(06) VALUE "PAG.  ".
              02 VAR-PAG           PIC ZZ9.
              02 FILLER            PIC X(15) VALUE SPACES.


       01     CAB-02.
              02 FILLER            PIC X(01) VALUE SPACES.
              02 FILLER            PIC X(03) VALUE "CPF".
              02 FILLER            PIC X(25) VALUE SPACES.
              02 FILLER            PIC X(04) VALUE "NOME".
              02 FILLER            PIC X(29) VALUE SPACES.
              02 FILLER            PIC X(05) VALUE "EMAIL".
              02 FILLER            PIC X(13) VALUE SPACES.

       01     CAB-ESTADO.
              02 FILLER            PIC X(01) VALUE SPACES.
              02 FILLER            PIC X(08) VALUE "ESTADO: ".
              02 VAR-ESTADO        PIC X(02).
              02 FILLER            PIC X(69) VALUE SPACES.

       01     CAB-CIDADE.
              02 FILLER            PIC X(01) VALUE SPACES.
              02 FILLER            PIC X(08) VALUE "CIDADE: ".
              02 VAR-CIDADE        PIC X(30).
              02 FILLER            PIC X(41) VALUE SPACES.

       01     LINHA-BRANCO.
              02 FILLER            PIC X(80) VALUE SPACES.

       01     DETALHE.
              02 FILLER            PIC X(01) VALUE SPACES.
              02 VAR-CPF           PIC ZZ9.999.999.99.
              02 FILLER            PIC X(02) VALUE SPACES.
              02 VAR-NOME          PIC X(30).
              02 FILLER            PIC X(02) VALUE SPACES.
              02 VAR-EMAIL         PIC X(30).
              02 FILLER            PIC X(01) VALUE SPACES.

      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EX04.
       SORT   ARQ-SORT
              ASCENDING     KEY ESTADO-TRAB
              ASCENDING     KEY CIDADE-TRAB
              ASCENDING     KEY CPF-TRAB
              USING         CAD-CLIENTE
              OUTPUT        PROCEDURE ROT-SAIDA
       STOP   RUN.

       ROT-SAIDA            SECTION.
              PERFORM INICIO.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM FIM.

       INICIO               SECTION.
              OPEN        OUTPUT   REL-GERAL.
              PERFORM     LEITURA.

       LEITURA              SECTION.
              RETURN ARQ-SORT
                     AT END MOVE "SIM" TO FIM-ARQ.
   
       PRINCIPAL            SECTION.
              PERFORM IMPRESSAO.
              PERFORM LEITURA.

       IMPRESSAO            SECTION.
              PERFORM CABECALHO.

              IF ESTADO-ATUAL NOT EQUAL ESTADO-TRAB OR
                 CIDADE-ATUAL NOT EQUAL CIDADE-TRAB
                     PERFORM IMPRIME-ESTADO-CIDADE.

              PERFORM IMPRIME-DETALHE.

       CABECALHO            SECTION.
              IF CT-LIN GREATER THAN 19
                     PERFORM IMPRIME-TITULO.

       IMPRIME-TITULO       SECTION.
              ADD 1 TO CT-PAG.
              MOVE   CT-PAG        TO VAR-PAG.
              MOVE   SPACES        TO REG-REL.
              WRITE  REG-REL FROM CAB-01.
              MOVE   ZEROES        TO CT-LIN.

       IMPRIME-ESTADO-CIDADE SECTION.
              IF ESTADO-ATUAL NOT EQUAL SPACES
                     PERFORM COMPLETA-LINHAS UNTIL CT-LIN EQUAL 20.

              PERFORM CABECALHO.
              MOVE   ESTADO-TRAB   TO ESTADO-ATUAL.
              MOVE   ESTADO-TRAB   TO VAR-ESTADO.
              MOVE   CIDADE-TRAB   TO CIDADE-ATUAL.
              MOVE   CIDADE-TRAB   TO VAR-CIDADE.
              WRITE  REG-REL FROM CAB-ESTADO AFTER ADVANCING 1 LINE.
              WRITE  REG-REL FROM CAB-CIDADE AFTER ADVANCING 2 LINE.
              WRITE  REG-REL FROM CAB-02 AFTER ADVANCING 2 LINE.
              WRITE  REG-REL FROM LINHA-BRANCO AFTER ADVANCING 1 LINE.

       COMPLETA-LINHAS      SECTION.
              WRITE  REG-REL FROM LINHA-BRANCO AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.

       IMPRIME-DETALHE      SECTION.
              MOVE    CPF-TRAB     TO VAR-CPF.
              MOVE    NOME-TRAB    TO VAR-NOME.
              MOVE    EMAIL-TRAB   TO VAR-EMAIL.
              WRITE   REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.

       FIM                  SECTION.
              CLOSE REL-GERAL.
