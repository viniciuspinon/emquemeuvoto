@echo off
chcp 65001 > nul
echo ======================================================
echo  EM QUEM EU VOTO - Servidor de Desenvolvimento Local
echo ======================================================
echo.
echo Iniciando servidor em http://127.0.0.1:8000 ...
echo Pressione Ctrl + C para encerrar.
echo.
python -m uvicorn api.main:app --host 127.0.0.1 --port 8000 --reload
