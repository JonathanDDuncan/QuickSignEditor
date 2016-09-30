del .\published\*.css
del .\published\*.js
xcopy .\dist\*.*  published /s /y

del src\signmaker\lib\elm.js

move .\dist\*.js  src\signmaker\lib\elm.js
copy .\src\styles\*.css  src\signmaker\css\

copy src\signmaker\lib\elm.js "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\lib\"
copy .\src\styles\*.css  "..\..\..\SignWriter\SignWriter Studio\SignWriterStudio\dist\css\"

copy .\src\sw10.min.js  published\ /y
xcopy .\src\signmaker\*.*  published\signmaker /s /y

pause
git branch -dr origin/gh-pages
git subtree push --prefix published origin gh-pages

pause