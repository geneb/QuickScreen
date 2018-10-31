\basic71\bc qscr /s /o;
\basic71\bc dialog /s /o;
\basic71\bc \quickpak\evaluate /s /o;
\basic71\bc formedit /s /o;
pause
\basic71\bc getfile /s /o;
\basic71\bc \quickpak\pulldnms /s /o;
\basic71\bc \quickpak\qedits /s /o;
\basic71\bc qscalc /s /o;
pause
\basic71\bc qscrmisc /s /o;
\basic71\bc qscrsave /s /o;
\basic71\bc scrndisp /o /s;
\basic71\bc scrnfile /o /s;
pause
\basic71\bc scrnlib /o /s;
\basic71\bc \quickpak\vertmenu /s /o;
\basic71\link @qscr.rsp
pause
del qscr.obj
del dialog.obj
del evaluate.obj
del formedit.obj
del getfile.obj
del pulldnms.obj
del qedits.obj
del qscalc.obj
del qscrmisc.obj
del qscrsave.obj
del scrndisp.obj
del scrnfile.obj
del scrnlib.obj
del vertmenu.obj
