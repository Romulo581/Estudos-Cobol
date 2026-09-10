       IDENTIFICATION DIVISION.
           PROGRAM-ID. GravarDados.
           AUTHOR. ROMULO CESAR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL 
           SELECT NOTA-PRIMEIRO-B ASSIGN TO  "NotasBimestre1"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT NOTA-SEGUNDO-B ASSIGN TO "NotasBimestre2"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-TERCEIRO-B ASSIGN TO "NotasBimentre3"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-QUARTO-B ASSIGN TO "NotasBimentre4"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-QUINTO-B ASSIGN TO "NotasBimentre5"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  NOTA-PRIMEIRO-B.
       01  DET-B-01.
           88 FIN-B-01  VALUE                  HIGH-VALUES.
           05 MATRICULA-ALUNO-B01              PIC 9(006).
           05 NOME-ALUNO-B01.
               10  PRIMEIRO-NOME-B01           PIC X(020).
               10  SEGUNDO-NOME-B01            PIC X(020).
           05  GENERO-B01                      PIC A(001).
           05  NOTA-B-01.
               10  NOTA-INTEIRA-B01            PIC 9(002).
               10  NOTA-DECIMAL-B01            PIC 9(002).

       FD  NOTA-SEGUNDO-B.
       01  DET-B-02.
           88 FIN-B-02 VALUE                   HIGH-VALUES.
           05 MATRICULA-ALUNO-B02              PIC 9(006).
           05 NOME-ALUNO-B02.
               10 PRIMEIRO-NOME-B02             PIC X(020).
               10 SEGUNDO-NOME-B02             PIC X(020).
           05  GENERO-B02                      PIC A(001).
           05  NOTA-B-02.
               10 NOTA-INTEIRA-B02             PIC 9(002).
               10 NOTA-DECIMAL-B02             PIC 9(002).

       FD  NOTA-TERCEIRO-B.
       01  DET-B-03.
           88 FIN-B-03 VALUE                   HIGH-VALUES.
           05 MATRICULA-ALUNO-B03              PIC 9(006).
           05 NOME-ALUNO-B03.
               10 PRIMEIRO-NOME-B03            PIC X(020).
               10 SEGUNDO-NOME-B03             PIC X(020).
           05  GENERO-B03                      PIC A(001).
           05 NOTA-B-03.
               10 NOTA-INTEIRA-B03             PIC 9(002).
               10 NOTA-DECIMAL-B03             PIC 9(002).

       FD  NOTA-QUARTO-B.
       01  DET-B-04.
           88 FIN-B-04 VALUE                   HIGH-VALUES.
           05  MATRICULA-ALUNO-B04             PIC 9(006).
           05  NOME-ALUNO-B04.
               10 PRIMEIRO-NOME-B04            PIC X(020).
               10 SEGUNDO-NOME-B04             PIC X(020).
           05 GENERO-B04                       PIC A(001).
           05 NOTA-B-04.
               10 NOTA-INTEIRA-B04             PIC 9(002).
               10 NOTA-DECIMAL-B04             PIC 9(002).

       FD  NOTA-QUINTO-B.
       01  DET-B-05.
           88 FIN-B-05 VALUE                   HIGH-VALUES.
           05 MATRICULA-ALUNO-B05              PIC 9(006).
           05 NOME-ALUNO-B05.
               10 PRIMEIRO-NOME-B05            PIC X(020).
               10 SEGUNDO-NOME-B05             PIC X(020).
           05 GENERO-B05                       PIC A(001).
           05 NOTA-B-05.
               10 NOTA-INTEIRA-B05             PIC 9(002).
               10 NOTA-DECIMAL-B05             PIC 9(002).
       WORKING-STORAGE.
       