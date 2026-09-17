@echo off
chcp 65001 > nul
echo =================================================================
echo  EM QUEM EU VOTO — Agendamento de Atualizacao Diaria no Windows
echo =================================================================
echo.
echo Este assistente configura uma Tarefa Agendada no Windows para
echo executar a rotina de dados automaticamente todos os dias as 03:00.
echo.

set TASK_NAME="EmQuemEuVoto_Atualizacao_Diaria"
set SCRIPT_PATH="%~dp0atualizar_dados_diarios.py"
set WORKDIR="%~dp0.."

echo Nome da Tarefa: %TASK_NAME%
echo Diretorio de Trabalho: %WORKDIR%
echo.

schtasks /create /tn %TASK_NAME% /tr "python %SCRIPT_PATH%" /sc daily /st 03:00 /f /ru "%USERNAME%"

if %ERRORLEVEL% EQU 0 (
    echo.
    echo =================================================================
    echo  SUCESSO: Tarefa agendada configurada para as 03:00 diariamente!
    echo  Para visualizar ou editar, abra o "Agendador de Tarefas" do Windows.
    echo =================================================================
) else (
    echo.
    echo AVISO: Se o comando falhou por falta de privilegios, execute
    echo este arquivo clicando com o botao direito e escolhendo
    echo "Executar como Administrador".
)

pause
