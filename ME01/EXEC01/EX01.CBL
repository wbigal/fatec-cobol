      *==============================================================
      * Identification Division: Descreve metadados do programa
      *==============================================================
       IDENTIFICATION       DIVISION.

       PROGRAM-ID.          EX01.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        12/03/2017.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      COPIA OS DADOS DE "CADCLI1" PARA "CADCLI2".

      *==============================================================
      * Environment Division: Faz a configuracao do ambiente de
      * execucao.
      *==============================================================
       ENVIRONMENT          DIVISION.

       CONFIGURATION SECTION.
              SOURCE-COMPUTER.     IBM-PC.
              OBJECT-COMPUTER.     IBM-PC.
              SPECIAL-NAMES.       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
              SELECT CADCLI1       ASSIGN TO     DISK
              ORGANIZATION IS      LINE   SEQUENTIAL.

              SELECT CADCLI2       ASSIGN TO     DISK
              ORGANIZATION IS      LINE   SEQUENTIAL.

      *==============================================================
      * Data Division: Define como serao os dados armazenados
      *==============================================================
       DATA          DIVISION.

       FILE          SECTION.

      * Definicao do arquivo CADCLI1
       FD     CADCLI1
              LABEL         RECORD        ARE           STANDARD
              VALUE         OF FILE-ID    IS            "CADCLI1.DAT".
       01     REG-ENT.
              02     COD-ENT       PIC 9(05).
              02     NOME-ENT      PIC X(20).

      * Definicao do arquivo CADCLI2
       FD     CADCLI2
              LABEL         RECORD        ARE           STANDARD
              VALUE         OF FILE-ID    IS            "CADCLI2.DAT".
       01     REG-SAI.
              03     NOME-SAI      PIC X(20).
              03     COD-SAI       PIC 9(05).

       WORKING-STORAGE SECTION.
       77     FIM-ARQ       PIC X(03)     VALUE "NAO".

      *==============================================================
      * Procedure Division: Definicao dos procedimentos do programa
      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EXEC01.
              PERFORM INICIO.
              PERFORM PRINCIPAL
                      UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM TERMINO.
              STOP RUN.

       INICIO.
              OPEN   INPUT  CADCLI1
                     OUTPUT CADCLI2.
              PERFORM LEITURA.

       LEITURA.
              READ   CADCLI1
                     AT END
                     MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
              PERFORM GRAVACAO.
              PERFORM LEITURA.

       GRAVACAO.
              MOVE   COD-ENT       TO COD-SAI.
              MOVE   NOME-ENT      TO NOME-SAI.
              WRITE  REG-SAI.

       TERMINO.
              CLOSE  CADCLI1
                     CADCLI2.

       
