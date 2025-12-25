@echo off
title OpenACB - Liga Endesa Analytics
color 0A

echo.
echo  ========================================
echo   OpenACB - Liga Endesa Analytics
echo  ========================================
echo.
echo  Starting local server...
echo.

cd /d "%~dp0dist"

:: Try Python first (most common)
where python >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo  Using Python server...
    echo  Opening http://localhost:8080
    echo.
    echo  Press Ctrl+C to stop the server.
    echo.
    start http://localhost:8080
    python -m http.server 8080
    goto :end
)

:: Try Python3
where python3 >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo  Using Python3 server...
    start http://localhost:8080
    python3 -m http.server 8080
    goto :end
)

:: Try PHP
where php >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo  Using PHP server...
    start http://localhost:8080
    php -S localhost:8080
    goto :end
)

:: Try npx (Node.js)
where npx >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo  Using Node.js server...
    start http://localhost:3000
    npx serve .
    goto :end
)

:: No server found
echo.
echo  ERROR: No web server found!
echo.
echo  Please install one of:
echo    - Python: https://www.python.org/downloads/
echo    - Node.js: https://nodejs.org/
echo.
echo  Or open RStudio and run:
echo    install.packages("servr")
echo    servr::httd("path/to/OpenACB_React/dist")
echo.
pause

:end
