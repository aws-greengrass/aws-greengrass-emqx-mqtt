@echo off

:: Copy default config file into work directory
set script_dir=%~dp0
for %%A in ("%script_dir%\..") do @(
    set rel_root_dir=%%~fA
)
copy /d /y %rel_root_dir%\etc\emqx.conf emqx.conf
set EMQX_NODE__EMQX_CONF=%CD%\emqx.conf

:: Must use delayed expansion and !variable! for multi-line variable support
setlocal EnableDelayedExpansion

:: If the user provides us with configuration, then append it to the config file.
:: Later config values overwrite earlier values, so appending is the right thing to do
if defined EMQX_CONF (
    if "!EMQX_CONF!" == "{configuration:/emqx/emqx.conf}" (
        echo No additional configuration specified
    ) else (
        echo We have user-provided config
        echo !EMQX_CONF! >> emqx.conf
    )
) else (
    echo No EMQX_CONF provided
)
