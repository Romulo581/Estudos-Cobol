       IDENTIFICATION DIVISION.
           PROGRAM-ID. GravarDados.
           AUTHOR. ROMULO CESAR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL 
           SELECT NOTA-PRIMEIRO-B ASSIGN TO    "NotasBimestre1"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT NOTA-SEGUNDO-B ASSIGN TO     "NotasBimestre2"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-TERCEIRO-B ASSIGN TO    "NotasBimestre3"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-QUARTO-B ASSIGN TO      "NotasBimestre4"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT NOTA-QUINTO-B ASSIGN TO      "NotasBimestre5"
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
      *    Formatação para as notas dos arquivos 
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
      
      *    Variavel para validar o nome dos arquivos 
       01  WS-NA-B01.                              PIC X(020) VALUE " ".
       01  WS-NA-B02.                              PIC X(020) VALUE " ".
       01  WS-NA-B03.                              PIC X(020) VALUE " ".
       01  WS-NA-B04.                              PIC X(020) VALUE " ".
       01  WS-NA-B05.                              PIC X(020) VALUE " ".
       
      *    Variaveis para validar a matricula dos arquivos 
       01  WS-MT-B01.                              PIC 9(006) VALUE 0.
       01  WS-MT-B02.                              PIC 9(006) VALUE 0.
       01  WS-MT-B03.                              PIC 9(006) VALUE 0.
       01  WS-MT-B04.                              PIC 9(006) VALUE 0.
       01  WS-MT-B05.                              PIC 9(006) VALUE 0.
      
      *    Variavel do calculo da media
       01  WS-CAL-MED.
           05 WS-MED-INTEIRO                       PIC 9(002) VALUE 0.
           FILLER                                  PIC 9(001) VALUE ",".
           05 WS-MED-DECIMAL                       PIC 9(002) VALUE 0.

      *    Variaveis para confirmar a validação  
       01  WS-NOME-IGUAL                           PIC X(001) VALUE "N".
       01  WS-MATRI-IGUAL                          PIC X(001) VALUE "N".
      
      *    Validando leitura dos arquivos
       01  WS-LEITURA-VALIDA                       PIC X(001) VALUE "N".

      *    Variaveis para finalizar a leitura do arquivo 
       01  LEITURA-B01-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B02-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B03-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B04-FINALIZADA                  PIC X VALUE "N".
       01  LEITURA-B05-FINALIZADA                  PIC X VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           PERFORM HEADER.

           OPEN INPUT NOTA-PRIMEIRO-B
                      NOTA-SEGUNDO-B
                      NOTA-TERCEIRO-B
                      NOTA-QUARTO-B
                      NOTA-QUINTO-B.
           PERFORM ARQ-PROCESSA.   
           CLOSE INPUT NOTA-PRIMEIRO-B
                      NOTA-SEGUNDO-B
                      NOTA-TERCEIRO-B
                      NOTA-QUARTO-B
                      NOTA-QUINTO-B.


           PERFORM EXIBIR-RESUMO.

       STOP RUN.
       
       HEADER.
       DISPLAY "=====================================================".
       DISPLAY "|_________ Media escolar Anual dos alunos __________|".
       DISPLAY "=====================================================".
      
      *    Processamento do Arquivo  
       
      *    Validando leitura dos arquivos 
       VALID-LEITURA.
           IF LEITURA-B01-FINALIZADA       EQUAL "S"
              AND LEITURA-B02-FINALIZADA   EQUAL "S"
              AND LEITURA-B03-FINALIZADA   EQUAL "S"
              AND LEITURA-B04-FINALIZADA   EQUAL "S"
              AND LEITURA-B05-FINALIZADA   EQUAL "S"
              MOVE "S" TO WS-LEITURA-VALIDA
           ELSE
               MOVE "N" TO WS-LEITURA-VALIDA
               PERFORM ERRO-LEIT-ARQS
           END-IF.

      *    Montando variaveis dos arquivo do Bimestre01 
       LER-REG-B01.
           READ NOTA-PRIMEIRO-B INTO DET-B-01
               AT END
                   MOVE "S" TO LEITURA-B01-FINALIZADA
               NOT AT END
                   MOVE NOTA-INTEIRA-B01       TO WS-NIT-B01
                   MOVE NOTA-DECIMAL-B01       TO WS-NDC-B01
                   MOVE NOME-ALUNO-B01         TO WS-NA-B01
                   MOVE MATRICULA-ALUNO-B01    TO WS-MT-B01
                   CONTINUE
           END-READ.

      *    Montando variaveis dos arquivo do Bimestre02 
       LER-REG-B02.
           READ NOTA-SEGUNDO-B INTO DET-B-02
               AT END
                   MOVE "S" TO LEITURA-B02-FINALIZADA
               NOT AT END
                   MOVE NOTA-INTEIRA-B02       TO WS-NIT-B02
                   MOVE NOTA-DECIMAL-B02       TO WS-NDC-B02
                   MOVE NOME-ALUNO-B02         TO WS-NA-B02
                   MOVE MATRICULA-ALUNO-B02    TO WS-MT-B02
                   CONTINUE
           END-READ.

      *    Montando variaveis dos arquivo do Bimestre03 
       LER-REG-B03.
           READ NOTA-TERCEIRO-B INTO DET-B-03
               AT END
                   MOVE "S" TO LEITURA-B03-FINALIZADA
               NOT AT END
                   MOVE NOTA-INTEIRA-B03       TO WS-NIT-B03
                   MOVE NOTA-DECIMAL-B03       TO WS-NDC-B03
                   MOVE NOME-ALUNO-B03         TO WS-NA-B03
                   MOVE MATRICULA-ALUNO-B03    TO WS-MT-B03
                   CONTINUE
           END-READ.

      *    Montando variaveis dos arquivo do Bimestre04 
       LER-REG-B04.
           READ NOTA-QUARTO-B INTO DET-B-04
               AT END
                   MOVE "S" TO LEITURA-B04-FINALIZADA
               NOT AT END
                   MOVE NOTA-INTEIRA-B04       TO WS-NIT-B04
                   MOVE NOTA-DECIMAL-B04       TO WS-NDC-B04
                   MOVE NOME-ALUNO-B04         TO WS-NA-B04
                   MOVE MATRICULA-ALUNO-B04    TO WS-MT-B04
                   CONTINUE
           END-READ.

      *    Montando variaveis dos arquivo do Bimestre04 
       LER-REG-B05.
           READ NOTA-QUINTO-B INTO DET-B-05
               AT END
                   MOVE "S" TO LEITURA-B05-FINALIZADA
               NOT AT END
                   MOVE NOTA-INTEIRA-B05       TO WS-NIT-B05
                   MOVE NOTA-DECIMAL-B05       TO WS-NDC-B05
                   MOVE NOME-ALUNO-B05         TO WS-NA-B05
                   MOVE MATRICULA-ALUNO-B05    TO WS-MT-B05
                   CONTINUE
           END-READ.


      *    PERFORM para validar as Matriculas e nomes  
       VALID-ARQ.
      *    Validando nome 
           IF  WS-NA-B01 = WS-NA-B02
               AND WS-NA-B02 = WS-NA-B03
               AND WS-NA-B03 = WS-NA-B04
               AND WS-NA-B04 = WS-NA-B05
               MOVE "S" TO WS-NOME-IGUAL
      *        Validando a matricula 
               IF  WS-MT-B01 = WS-MT-B02
                   AND WS-MT-B02 = WS-MT-B03
                   AND WS-MT-B03 = WS-MT-B04
                   AND WS-MT-B04 = WS-MT-B05
                   MOVE "S" TO WS-MATRI-IGUAL

               ELSE 
                   MOVE "N" TO WS-MATRI-IGUAL
                   PERFORM ERRO-MATRIC
               END-IF

           ELSE
               MOVE "N" TO WS-NOME-IGUAL
               PERFORM ERRO-NOME
           END-IF.

       MAT-IMP.
           IF WS-MATRI-IGUAL EQUAL "S"
              DISPLAY "Matricula: " WS-MT-B01
              
           ELSE
               PERFORM ERRO-MATRIC
           END-IF.
       
      *    PERFORM para imprimir valigadar logica 
       NOM-IMP.
           IF WS-NOME-IGUAL        EQUAL "S"
              AND WS-MATRI-IGUAL   EQUAL "S"
              DISPLAY "Aluno: " WS-NA-B01

           ELSE 
               PERFORM ERRO-IMP-NOM
           END-IF.
       
       MED-IMP.
           IF WS-MATRI-IGUAL = "S"
              AND WS-NOME-IGUAL = "S"
                   COMPUTE WS-CAL-MED = (NOTA-FORM-B-01 +
                                         NOTA-FORM-B-02 +
                                         NOTA-FORM-B-03 +
                                         NOTA-FORM-B-04 +
                                         NOTA-FORM-B-05) / 5
                   DISPLAY "A média é: " WS-CAL-MED
           ELSE 
               PERFORM ERRO-NO-CALCULO-MED
           END-IF.

       EXIBIR-RESUMO.
           IF WS-LEITURA-VALIDA EQUAL "S"
              PERFORM NOM-IMP
              PERFORM MED-IMP
              PERFORM MAT-IMP
           ELSE
               DISPLAY "ERRO 404"
           END-IF.
           
      *    Campo de mensagem de erro 
       ERRO-LEITURA-B01.
           DISPLAY "=============================================".
           DISPLAY "| Erro na leitura do Arquivo NotasBimestre1 |".
           DISPLAY "=============================================".
       
       ERRO-LEITURA-B02.
           DISPLAY "=============================================".
           DISPLAY "| Erro na leitura do Arquivo NotasBimestre2 |".
           DISPLAY "=============================================".

       ERRO-LEITURA-B03.
           DISPLAY "=============================================".
           DISPLAY "| Erro na leitura do Arquivo NotasBimestre3 |".
           DISPLAY "=============================================".

       ERRO-LEITURA-B04.
           DISPLAY "=============================================".
           DISPLAY "| Erro na leitura do Arquivo NotasBimestre4 |".
           DISPLAY "=============================================".

       ERRO-LEITURA-B05.
           DISPLAY "=============================================".
           DISPLAY "| Erro na leitura do Arquivo NotasBimestre5 |".
           DISPLAY "=============================================".

       ERRO-LEIT-ARQS.
           DISPLAY "=============================================".
           DISPLAY "|     Erro na leitura de algum Arquivo      |".
           DISPLAY "=============================================".

       ERRO-IMP-NOM.
           DISPLAY "=============================================".
           DISPLAY "|     Erro ao imprimir o nome do aluno      |".
           DISPLAY "=============================================".
      
       ERRO-MATRIC.
           DISPLAY "=============================================".
           DISPLAY "| A matricula é divergente de outro arquivo |".
           DISPLAY "=============================================".

       ERRO-NOME.
           DISPLAY "=============================================".
           DISPLAY "|    O Nome é divergente de outro arquivo   |".
           DISPLAY "=============================================".
       
       ERRO-NO-CALCULO-MED.
           DISPLAY "=============================================".
           DISPLAY "| Erro no calculo ou no processo de leitura |".
           DISPLAY "=============================================".
      *    Final do campo de erros
