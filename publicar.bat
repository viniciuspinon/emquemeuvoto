@echo off
chcp 65001 > nul
echo ======================================================
echo  EM QUEM EU VOTO - Script de Publicacao para Producao
echo ======================================================
python scripts\publicar_producao.py %*
