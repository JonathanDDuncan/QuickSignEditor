xcopy .\dist\*.*  published\ /s /y
copy .\src\sw10*.*  published\

copy .\src\sw10*.*  published\
xcopy .\src\signmaker\*.*  published\signmaker /s /y
pause
git branch -dr origin/gh-pages
git subtree push --prefix published origin gh-pages

pause