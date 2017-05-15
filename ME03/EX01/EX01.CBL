       IDENTIFICATION              DIVISION.

       PROGRAM-ID.          EX-01.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        14/05/2017.
       DATE-COMPILED.
       SECURITY.            APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      ESTE PROGRAMA LE UM ARQUIVO E GERA UM ARQUIVO
      *              DE SAIDA ORDENADO PELO CAMPO CODIGO E NOME.

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
              02 SEXO              PIC X(01).

       SD     TRAB.
       01     TRAB-REG.
              02 CODIGO-TRAB       PIC 9(05).
              02 NOME-TRAB         PIC X(30).
              02 SEXO-TRAB         PIC X(01).

       FD     CADSAI
              LABEL RECORD ARE STANDARD
              VALUE OF FILE-ID IS "CADSAI.DAT".
       01     REG-SAI.
              02 CODIGO-SAI        PIC 9(05).
              02 NOME-SAI         PIC X(30).
              02 SEXO-SAI          PIC X(01).

      *==============================================================
       PROCEDURE            DIVISION.

       PGM-EX01.
       SORT   TRAB
              ASCENDING     KEY CODIGO-TRAB
              ASCENDING     KEY NOME-TRAB
              USING         CADENT
              GIVING        CADSAI
       STOP   RUN.
