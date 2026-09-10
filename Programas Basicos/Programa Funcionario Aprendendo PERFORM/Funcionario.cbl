       IDENTIFICATION DIVISION.
           PROGRAM-ID. ContagemFuncionarios.
           AUTHOR. ROMULO CESAR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FUNCIONARIOS ASSIGN TO "FUNCIONARIOS.DATA"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FUNCIONARIOS.
       01  DETALHEFUNCIONARIO.
           88 FINALREGISTRO VALUE HIGH-VALUES.
           05 MATRICULA-FUNCIONARIO    PIC 9(005).
           05 NOME-FUNCIONARIO.
               10 PRIMEIRO-NOME        PIC X(020).
               10 ULTIMO-NOME          PIC X(020).
           05 GENERO                   PIC A(001).
           05 DATA-CONTRATACAO.
               10 ANO-CONTRATACAO      PIC 9(004).
               10 MES-CONTRATACAO      PIC 9(002).
               10 DIA-CONTRATACAO      PIC 9(002).
       
       WORKING-STORAGE.
       01  CONTADORES
           05 TOTAL-HOMENS             PIC 9(003) VALUE 0.
           05 TOTAL-MULHERES           PIC 9(003) VALUE 0.

       01  WS-DATA-FORMATADA.
           05 WS-DIA-CONTRATACAO       PIC 9(002) VALUE 0.
           05 FILLER                   PIC X(001) VALUE "/".
           05 WS-MES-CONTRATACAO       PIC 9(002) VALUE 0.
           05 FILLER                   PIC X(001) VALUE "/".
           05 WS-ANO-CONTRATACAO       PIC 9(004) VALUE 0.

       01  LEITURA-FINALIZADA          PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM HEADER.
           OPEN INPUT FUNCIONARIOS.
           PERFORM PROCESSAR-REGISTROS.
           CLOSE FUNCIONARIOS.
           PERFORM EXIBIR-RESUMO.
           STOP RUN.

       PROCESSAR-REGISTROS.
           PERFORM UNTIL LEITURA-FINALIZADA = "S"
               PERFORM LER-REGISTRO
               IF LEITURA-FINALIZADA NOT = "S"
                   PERFORM EXIBIR-FUNCIONARIO
                   PERFORM CONTAR-GENERO
               END-IF
           END-PERFORM.
       HEADER.
           DISPLAY "=============================".
           DISPLAY "| Contagem de Funcionários  |".
           DISPLAY "=============================".
           
      
       LER-REGISTRO.
           READ FUNCIONARIOS INTO DETALHEFUNCIONARIO
               AT END
                   MOVE "S" TO LEITURA-FINALIZADA
               NOT AT END
                   MOVE ANO-CONTRATACAO TO WS-ANO-CONTRATACAO
                   MOVE MES-CONTRATACAO TO WS-MES-CONTRATACAO
                   MOVE DIA-CONTRATACAO TO WS-DIA-CONTRATACAO
                   CONTINUE
           END-READ.

       EXIBIR-FUNCIONARIO.
           INSPECT PRIMEIRO-NOME REPLACING ALL " " BY LOW-VALUES
           INSPECT ULTIMO-NOME   REPLACING ALL " " BY LOW-VALUES
           DISPLAY MATRICULA-FUNCIONARIO " " GENERO " "
               PRIMEIRO-NOME " " ULTIMO-NOME " "
               WS-DATA-FORMATADA.
           DISPLAY "=================================================".

       CONTAR-GENERO.
           IF GENERO = "M"
               ADD 1 TO TOTAL-HOMENS
           ELSE
               IF GENERO = "F"
                   ADD 1 TO TOTAL-MULHERES
               ELSE
               PERFORM ERRO-GENERO
               END-IF
           END-IF.
           
       EXIBIR-RESUMO.
           DISPLAY "===================================".
           DISPLAY "Resumo: ".
           DISPLAY "Total de Homens:.. " TOTAL-HOMENS.
           DISPLAY "Total de Mulheres: " TOTAL-MULHERES.
           DISPLAY "===================================".
       
       ERRO-GENERO.
           DISPLAY "O Genero Não Informado".