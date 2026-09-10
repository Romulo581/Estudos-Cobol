       IDENTIFICATION DIVISION.
           PROGRAM-ID. GravarDados.
           AUTHOR. ROMULO CESAR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL 
           SELECT NOTAS-PRIMEIRO-B ASSIGN TO  "NotasBimestre1"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT NOTAS-SEGUNDO-B ASSIGN TO "NotasBimestre2"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTAS-TERCEIRO-B ASSIGN TO "NotasBimentre3"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTAS-QUANTO-B ASSIGN TO "NotasBimentre4"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-QUINTO-B ASSIGN TO "NotasBimentre5"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  NOTAS-PRIMEIRO-B
       01  DET-B-01
           88 FINALIZARBIMESTRE VALUER HIGH-VALUES.
           05 MATRICULA-ALUNO               PIC 9(006).
           05 NOME-ALUNO.
               10  PRIMEIRO-NOME            PIC X(020).
               10  SEGUNDO-NOME             PIC X(020).
           05  GENERO                       PIC A(001).
           05  NOTA-B-01.
               10  NOTA-INTEIRA             PIC 9(002).
               FILLER                       PIC X(001) VALUE ",".
               10  NOTA-DECIMAL             PIC 9(002).
       FD  NOTAS-SEGUNDO-B
       FD  NOTAS-TERCEIRO-B
       FD  NOTAS-QUANTO-B
       FD  NOTA-QUINTO-B