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
               10 PRIMEIRO-NOME-B02            PIC X(020).
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
       01  NOTA-FORM-B-01.
           05  WS-NOTA-B-01.
               10 WS-NIT-B01                       PIC 9(002) VALUE 0.   
               FILLER                              PIC A(001) VALUE ",".
               10 WS-NDC-B01                       PIC 9(002) VALUE 0.

       01  NOTA-FORM-B-02.
           05  WS-NOTA-B-02.
               10 WS-NIT-B02                       PIC 9(002) VALUE 0.   
               FILLER                              PIC A(001) VALUE ",".
               10 WS-NDC-B02                       PIC 9(002) VALUE 0.       

       01  NOTA-FORM-B-03.
           05  WS-NOTA-B-03.
               10 WS-NIT-B03                       PIC 9(002) VALUE 0.   
               FILLER                              PIC A(001) VALUE ",".
               10 WS-NDC-B03                       PIC 9(002) VALUE 0. 

       01  NOTA-FORM-B-04.
           05  WS-NOTA-B-04.
               10 WS-NIT-B04                       PIC 9(002) VALUE 0.          
               FILLER                              PIC A(001) VALUE ",".
               10 WS-NDC-B04                       PIC 9(002) VALUE 0.
       
       01  NOTA-FORM-B-05.
           05  WS-NOTA-B-05.
               10 WS-NIT-B05                       PIC 9(002) VALUE 0.           
               FILLER                              PIC A(001) VALUE ",".
               10 WS-NDC-B05                       PIC 9(002) VALUE 0.

       01  WS-NA-B01.                              PIC X(020) VALUE " ".
       01  WS-NA-B02.                              PIC X(020) VALUE " ".
       01  WS-NA-B03.                              PIC X(020) VALUE " ".
       01  WS-NA-B04.                              PIC X(020) VALUE " ".
       01  WS-NA-B05.                              PIC X(020) VALUE " ".
       
       01  WS-NOME-IGUAL                           PIC X(001) VALUE "N".

       01  LEITURA-B01-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B02-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B03-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B04-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B05-FINALIZADA                  PIC X VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           PERFORM HEADER.

           OPEN INPUT NOTA-PRIMEIRO-B.
           PERFORM PROCESSAR-REG-B01.
           CLOSE NOTA-PRIMEIRO-B.

           OPEN INPUT NOTA-SEGUNDO-B.
           PERFORM PROCESSAR-REG-B02.
           CLOSE NOTA-SEGUNDO-B.

           OPEN INPUT NOTA-TERCEIRO-B.
           PERFORM PRECESSAR-REG-B03.
           CLOSE NOTA-TERCEIRO-B.

           OPEN INPUT NOTA-QUARTO-B.
           PERFORM PROCESSAR-REG-B04.
           CLOSE NOTA-QUARTO-B.

           OPEN INPUT NOTA-QUINTO-B.
           PERFORM PROCESSAR-REG-B05.
           CLOSE NOTA-QUINTO-B.

           PERFORM EXIBIR-RESUMO.

       STOP RUN.

       HEADER.
       DISPLAY "=====================================================".
       DISPLAY "|_________ Media escolar Anual dos alunos __________|".
       DISPLAY "=====================================================".

       PROCESSAR-REG-B01.
           PERFORM UNTIL LEITURA-B01-FINALIZADA = "S"
               PERFORM LER-REG-B01
               IF LEITURA-B01-FINALIZADA = "N"
                   PERFORM ERRO-LEITURA-B01
               END-IF
           END-PERFORM.

       LER-REG-B01.
           READ NOTA-PRIMEIRO-B INTO DET-B-01
               AT END
                   MOVE "S" TO LEITURA-B01-FINALIZADA
               NOT AT END
                   MOVE NOTA-INTEIRA-B01 TO WS-NIT-B01
                   MOVE NOTA-DECIMAL-B01 TO WS-NDC-B01
                   MOVE NOME-ALUNO-B01   TO WS-NA-B01
                   CONTINUE
           END-READ.
       ERRO-LEITURA-B01.
           DISPLAY "Erro na leitura do Arquivo NotasBimestre1".

       EXIBIR-RESUMO.
           PERFORM CALC

       CALC.
           IF WS-NA-B01 = WS-NA-B02
               AND WS-NA-B02 = WS-NA-B03
               AND WS-NA-B03 = WS-NA-B04
               AND WS-NA-B04 = WS-NA-B05
           
               MOVE "S" WS-NOME-IGUAL
           ELSE
               MOVE "N" WS-NOME-IGUAL
           END-IF
