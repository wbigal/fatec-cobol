       IDENTIFICATION              DIVISION.

       PROGRAM-ID.          EX-03.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        14/05/2017.
       DATE-COMPILED.
       SECURITY.            APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      ESTE PROGRAMA LE UM ARQUIVO E GERA UM ARQUIVO
      *              ARQUIVO DE SAIDA COM APENAS OS DEPTOS 01 E 02
      *              ORDENADOS PELO CODIGO.

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

              SELECT TRAB ASSIGN TO DISK.

              SELECT CADSAI ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL.

      *==============================================================
       DATA                 DIVISION.

       FILE                 SECTION.
       FD     CADENT
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADENT.DAT".
       01     REG-ENT.
              02 CODIGO            PIC 9(05).
              02 NOME              PIC X(30).
              02 DEPARTAMENTO      PIC 9(02).

       SD     TRAB.
       01     TRAB-REG.
              02 CODIGO-TRAB       PIC 9(05).
              02 NOME-TRAB         PIC X(30).
              02 DEPARTAMENTO-TRAB PIC 9(02).

       FD     CADSAI
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADSAI.DAT".
       01     REG-SAI.
              02 CODIGO-SAI        PIC 9(05).
              02 NOME-SAI          PIC X(30).
              02 DEPARTAMENTO-SAI  PIC 9(02).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ      PIC X(03) VALUE "NAO".

      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EX03.
       SORT   TRAB
              ASCENDING     KEY CODIGO-TRAB
              USING         CADENT
              OUTPUT        PROCEDURE ROT-SAIDA
       STOP   RUN.

       ROT-SAIDA            SECTION.
              PERFORM INICIO.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM FIM.

       INICIO               SECTION.
              OPEN        OUTPUT   CADSAI.
              PERFORM     LEITURA.

       LEITURA              SECTION.
              RETURN TRAB
                     AT END MOVE "SIM" TO FIM-ARQ.
   
       PRINCIPAL            SECTION.
              PERFORM SELECAO.
              PERFORM LEITURA.

       SELECAO              SECTION.
              IF DEPARTAMENTO-TRAB EQUAL 01 OR 02
                     PERFORM GRAVACAO.

       GRAVACAO             SECTION.
              MOVE    CODIGO-TRAB       TO CODIGO-SAI.
              MOVE    DEPARTAMENTO-TRAB TO DEPARTAMENTO-SAI.
              MOVE    NOME-TRAB         TO NOME-SAI.
              WRITE   REG-SAI.

       FIM                  SECTION.
              CLOSE CADSAI.
