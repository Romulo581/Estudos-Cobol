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
           88 FINALREGISTRO VALUE HIGH-VELUES.
           05 MATRICULA-FUNCIONARIO    PIC 9(005).
           05 NOME-FUNCIONARIO0.
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

       01  LEITURA-FINALIZADA          PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "=============================".
           DISPLAY "Contagem de Funcionários".
           DISPLAY "=============================".
      
      * Abrindo o Arquivo que foi declarado na File Section FD**
           OPEN INPUT FUNCIONARIOS.
           
      * Processamento dos Registros do arquivo
           PERFORM UNIT LEITURA-FINALIZADA = "S"
               READ FUNCIONARIOS INTO DETALHEFUNCIONARIO