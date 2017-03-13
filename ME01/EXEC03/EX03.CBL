       IDENTIFICATION              DIVISION.
       PROGRAM-ID.          EX03.
       AUTHOR.              WAGNER BIGAL.
       INSTALLATION.        FATEC-SP.
       DATE-WRITTEN.        12/03/2017.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.      Este programa calcula a media de tres
      *              notas de um aluno.

      *==============================================================
       ENVIRONMENT                 DIVISION.

       CONFIGURATION               SECTION.
              SOURCE-COMPUTER.     IBM-PC.
              OBJECT-COMPUTER.     IBM-PC.
              SPECIAL-NAMES.       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
              SELECT        CADALU        ASSIGN        TO DISK
              ORGANIZATION  IS            LINE          SEQUENTIAL.

              SELECT        CADATU        ASSIGN        TO DISK
              ORGANIZATION  IS            LINE          SEQUENTIAL.

      *==============================================================
       DATA                        DIVISION.

       FILE                        SECTION.
       FD     CADALU
              LABEL         RECORD        ARE    STANDARD
              VALUE         OF FILE-ID    IS     "CADALU.DAT".
       01     REG-ENT.
              02     NUMERO        PIC 9(05).
              02     NOME          PIC X(20).
              02     NOTA1         PIC 9(02)V99.
              02     NOTA2         PIC 9(02)V99.
              02     NOTA3         PIC 9(02)V99.

       FD     CADATU
              LABEL         RECORD        ARE    STANDARD
              VALUE         OF FILE-ID    IS     "CADATU.DAT".
       01     REG-SAI.
              02     NUMERO-SAI    PIC 9(05).
              02     MEDIA-SAI     PIC 9(02)V99.

       WORKING-STORAGE             SECTION.
       77     FIM-ARQ       PIC X(03)     VALUE "NAO".
       77     SOMA-NOTA     PIC 9(02)V99  VALUE ZEROS.
       77     MEDIA-CALC    PIC 9(02)V99  VALUE ZEROS.

      *==============================================================
       PROCEDURE                   DIVISION.

       PRM-EX03.
              PERFORM INICIO.
              PERFORM PRINCIPAL
                      UNTIL FIM-ARQ EQUAL "SIM".
              PERFORM TERMINO.
              STOP RUN.

       INICIO.
              OPEN   INPUT  CADALU
                     OUTPUT CADATU.
              PERFORM LEITURA.

       LEITURA.
              READ CADALU
                     AT END
                     MOVE "SIM" TO FIM-ARQ.
       PRINCIPAL.
              PERFORM CALCMEDIA.
              PERFORM GRAVACAO.
              PERFORM LEITURA.

       CALCMEDIA.
              ADD NOTA1, NOTA2, NOTA2 GIVING SOMA-NOTA.
              DIVIDE SOMA-NOTA BY 3 GIVING MEDIA-CALC.

       GRAVACAO.
              MOVE   NUMERO        TO NUMERO-SAI.
              MOVE   MEDIA-CALC    TO MEDIA-SAI.
              WRITE  REG-SAI.

       TERMINO.
              CLOSE  CADALU
                     CADATU.
